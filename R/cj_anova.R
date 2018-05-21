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
    formula_full <- update(formula, reformulate(paste0("(.) * ", by_vars)))
    
    # get `id` as character string
    #idvar <- all.vars(update(id, 0 ~ . ))
    
    # estimate model
    if (inherits(data, "data.frame") && is.null(weights)) {
        svydesign <- NULL
        estimates_full <- stats::glm(formula_full, data = data, ...)
        estimates_reduced <- stats::glm(formula, data = data, ...)
    } else if (inherits(data, "data.frame")) {
        svydesign <- survey::svydesign(ids = ~ 0, weights = weights, data = data)
        estimates_full <- survey::svyglm(formula_full, design = svydesign, ...)
        estimates_reduced <- survey::svyglm(formula, design = svydesign, ...)
    } else if (inherits(data, "survey.design")) {
        svydesign <- data
        estimates_full <- survey::svyglm(formula_full, design = svydesign, ...)
        estimates_reduced <- survey::svyglm(formula, design = svydesign, ...)
    } else {
        stop("'data' is not a 'data.frame' or 'survey.design' object")
    }
    
    anova(estimates_reduced, estimates_full, test = "F")
}

# function to figure out term levels of coef_df "_name" column
split_coef_name_by_term <- function(coefficient, term_label) {
    # this strictly only works with two-way interactions
    term_vec <- strsplit(term_label, ":")[[1L]]
    if (length(term_vec) != 2L) {
        return(rep(NA_character_, 2))
    }
    
    ## extract level of term 1
    # regular expression using positive lookbehind and positive lookahead on term names
    m1 <- regexpr(paste0("(?<=", term_vec[1L], ").+(?=:", term_vec[2L], ")"), coefficient, perl = TRUE)
    term1_level <- regmatches(coefficient, m1)
    
    ## extract level of term 2
    m2 <- regexpr(paste0("(?<=:", term_vec[2L], ").+"), coefficient, perl = TRUE)
    term2_level <- regmatches(coefficient, m2)
    
    out <- c(term1_level, term2_level)
    names(out) <- c(term_vec)
    out
}
