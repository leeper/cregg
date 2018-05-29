#' @rdname diffs
#' @export
cj_anova <-
function(
  data,
  formula,
  id = NULL,
  weights = NULL,
  by = NULL,
  ...
) {
    
    # santiy check that 'formula' is LHS + RHS
    stopifnot(inherits(formula, "formula"))
    stopifnot(length(formula) > 2L)
    stopifnot(attr(terms(formula), "response") == 1L)
    
    # get RHS variables
    by_vars <- all.vars(stats::update(by, 0 ~ . ))
    
    # modify formula to include appropriate interaction
    formula_reduced <- formula
    formula_full <- update(formula, reformulate(paste0("(.) * ", by_vars)))
    
    # get `id` as character string
    #idvar <- all.vars(update(id, 0 ~ . ))
    
    # estimate model
    if (inherits(data, "data.frame") && is.null(weights)) {
        svy_design <- NULL
        estimates_full <- stats::glm(formula_full, data = data, ...)
        estimates_reduced <- stats::glm(formula_reduced, data = data, ...)
    } else if (inherits(data, "data.frame")) {
        stop("cj_anova() currently does not support 'weights'")
        svy_design <- survey::svydesign(ids = ~ 0, weights = weights, data = data)
        estimates_full <- survey::svyglm(formula_full, design = svy_design, ...)
        estimates_reduced <- survey::svyglm(formula_reduced, design = svy_design, ...)
    } else if (inherits(data, "survey.design")) {
        stop("cj_anova() currently does not support 'weights'")
        svy_design <- data
        estimates_full <- survey::svyglm(formula_full, design = svy_design, ...)
        estimates_reduced <- survey::svyglm(formula_reduced, design = svy_design, ...)
    } else {
        stop("'data' is not a 'data.frame' or 'survey.design' object")
    }
    
    anova(estimates_reduced, estimates_full, test = "F")
}
