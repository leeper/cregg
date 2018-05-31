#' @rdname diffs
#' @title Preference Heterogeneity Diagnostics
#' @description Tests for preference heterogeneity in conjoint experiments
#' @param data A data frame containing variables specified in \code{formula}. All RHS variables should be factors; the base level for each will be used in estimation and for AMCEs the base level's AMCE will be NA. Optionally, this can instead be an object of class \dQuote{survey.design} returned by \code{\link[survey]{svydesign}}.
#' @param formula A formula specifying a model to be estimated. All variables should be factors.
#' @param id Ignored.
#' @template weights
#' @template feature_order
#' @template feature_labels
#' @template level_order
#' @template alpha
#' @param by A formula containing only RHS variables, specifying grouping factors over which to perform estimation. For \code{amce_diffs}, this can be a factor or something coercable to factor. For \code{mm_diffs} the variable must only take two levels.
#' @param \dots Additional arguments to \code{\link{amce}}, \code{\link{freqs}}, or \code{\link{mm}}.
#' @author Thomas J. Leeper <thosjleeper@gmail.com>
#' @return \code{amce_diffs} and \code{mm_diffs} return a data frame similar to the one returned by \code{\link{cj}}, including a \code{BY} column (with the value \dQuote{Difference}) for easy merging with results returned by that function.
#' 
#' \code{cj_anova} returns an \code{\link[stats]{anova}} object.
#' 
#' @details \code{cj_anova} takes a model formula (\dQuote{reduced} model) and generates a \dQuote{full} model with full saturated interactions between the variables specified in \code{by} and all RHS variables in \code{formula}, then computes an F-test comparing the two models, providing a test for whether preferences vary across levels of \code{by}. This is, in essence, a test of whether all such interaction coefficients are distinguishable from zero. Because the test depends on overall model fit, not the coefficient variances, clustering is irrelevant.
#' 
#' \code{mm_diffs} provides a data frame of differences in marginal means (literally differencing the results from \code{\link{mm}} across levels of \code{by}. This provides the clearest direct measure of preference differences from a conjoint design. \code{by} must
#' 
#' \code{amce_diffs} provides a data frame of differences in AMCEs (the coefficient on an interaction by each RHS factor and the variable in \code{by}). This provides an estimate of the difference in causal effects of each factor level relative to the baseline level (i.e., the difference in conditional AMCEs). This quantity is easily misinterpreted as the difference in preferences, which it is not. If preferences in the reference category differ across levels of \code{by}, the the difference in conditional AMCEs will have an unpredictable sign and significance. See \code{\link{amce_by_reference}} for a diagnostic.
#' 
#' @examples
#' data("immigration")
#' # Test for heterogeneity by profile order
#' cj_anova(immigration, ChosenImmigrant ~ Gender + Education + LanguageSkills, by = ~ contest_no)
#' 
#' # Test for heterogeneity by CountryOfOrigin feature
#' cj_anova(immigration, ChosenImmigrant ~ Gender + Education, by = ~ CountryOfOrigin)
#' 
#' \dontrun{
#' # Differences in MMs by Gender feature
#' mm_diffs(immigration, ChosenImmigrant ~ LanguageSkills + Education, ~ Gender, id = ~ CaseID)
#' 
#' # Differences in AMCEs by Gender feature (i.e., feature interactions)
#' amce_diffs(immigration, ChosenImmigrant ~ LanguageSkills + Education, ~ Gender, id = ~ CaseID)
#' }
#'
#' # preferences differ for Male and Female immigrants with 'Broken English' ability
#' mm_diffs(immigration, ChosenImmigrant ~ LanguageSkills, ~ Gender, id = ~ CaseID)
#' 
#' # yet differences in conditional AMCEs  depend on the reference category
#' amce_diffs(immigration, ChosenImmigrant ~ LanguageSkills, ~ Gender, id = ~ CaseID)
#' immigration$LanguageSkills2 <- relevel(immigration$LanguageSkills, "Used Interpreter")
#' amce_diffs(immigration, ChosenImmigrant ~ LanguageSkills2, ~ Gender, id = ~ CaseID)
#'
#' @seealso \code{\link{amce}} \code{\link{mm}} \code{\link{freqs}} \code{\link{plot.cj_amce}}
#' @importFrom lmtest coeftest
#' @export
amce_diffs <-
function(
  data,
  formula,
  by,
  id = NULL,
  weights = NULL,
  feature_order = NULL,
  feature_labels = NULL,
  level_order = c("ascending", "descending"),
  alpha = 0.05,
  ...
) {
    
    # get outcome variable
    outcome <- all.vars(stats::update(formula, . ~ 0))
    
    # get RHS variables, variable labels, and factor levels
    RHS <- all.vars(stats::update(formula, 0 ~ . ))
    by <- stats::update(by, ~ . )
    # sanity check that 'by' is only an single variable
    stopifnot(length(by) == 2L)
    by_var <- as.character(by)[2L]
    # coerce 'by_var' to factor
    if (!is.factor(data[[by_var]])) {
        data[[by_var]] <- factor(data[[by_var]])
    }
    
    # process feature_order argument
    feature_order <- check_feature_order(feature_order, RHS)
    
    # set level_order (within features) to ascending or descending
    level_order <- match.arg(level_order)
    
    # function to produce "fancy" feature labels
    feature_labels <- clean_feature_labels(data = data, RHS = RHS, feature_labels = feature_labels)
    
    # convert feature labels and levels to data frame
    term_labels_df <- make_term_labels_df(data, feature_order, level_order = level_order)
    
    # modify formula to include appropriate interaction
    formula <- update(formula, reformulate(paste0("(.) * ", by_var)))
    
    # estimate model
    if (inherits(data, "data.frame") && is.null(weights)) {
        svydesign <- NULL
        mod <- stats::glm(formula, data = data, ...)
    } else if (inherits(data, "data.frame")) {
        svydesign <- survey::svydesign(ids = ~ 0, weights = weights, data = data)
        mod <- survey::svyglm(formula, design = svydesign, ...)
    } else if (inherits(data, "survey.design")) {
        svydesign <- data
        mod <- survey::svyglm(formula, design = svydesign, ...)
    } else {
        stop("'data' is not a 'data.frame' or 'survey.design' object")
    }
    
    # get model terms as rich data frame
    terms_df <- get_coef_metadata(mod = mod)
    # keep only interactions between 'by_var'
    terms_df <- terms_df[terms_df[[by_var]] & terms_df[["_order"]] != 1, , drop = FALSE]
    
    # NEED TO GENERALIZE TO GET THIS WORKING WITH CONSTRAINED DESIGNS
    
    # get coefficients as data frame (correct, if needed, for clustering)
    coef_summary <- get_coef_summary(mod = mod, data = data, id = id, alpha = alpha)
    # merge coef_df and coef_summary
    coef_summary <- merge(coef_summary, terms_df, by = "_coef")
    
    coef_summary[["outcome"]] <- outcome
    coef_summary[["BY"]] <- coef_summary[["_by_level"]]
    coef_summary[["statistic"]] <- "amce_difference"
    
    # return
    out <- structure(coef_summary[, c("BY", "outcome", "statistic", "_base_var", "_base_level", "_by_level", names(coef_summary)[c(2:7)])],
                     names = c("BY", "outcome", "statistic", "feature", "level", by_var, "estimate", "std.error", "z", "p", "lower", "upper"),
                     by = by_var,
                     class = c("cj_diffs", "data.frame"))
    out[["feature"]] <- factor(out[["feature"]], levels = feature_order, labels = feature_labels[feature_order])
    out[["level"]] <- factor(out[["level"]], levels = term_labels_df[["level"]])
    return(out)
}
