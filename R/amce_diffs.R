#' @rdname diffs
#' @title Preference Heterogeneity Diagnostics
#' @description Tests for preference heterogeneity in conjoint experiments
#' @param data A data frame containing variables specified in \code{formula}. All RHS variables should be factors; the base level for each will be used in estimation and for AMCEs the base level's AMCE will be NA. Optionally, this can instead be an object of class \dQuote{survey.design} returned by \code{\link[survey]{svydesign}}.
#' @param formula A formula specifying a model to be estimated. All variables should be factors; all levels across features should be unique.
#' @param id Ignored.
#' @template weights
#' @template feature_order
#' @template feature_labels
#' @template level_order
#' @template alpha
#' @param h0 A numeric value specifying a null hypothesis value to use when generating z-statistics and p-values (only used for \code{mm_diffs}).
#' @param by A formula containing only RHS variables, specifying grouping factors over which to perform estimation. For \code{amce_diffs}, this can be a factor or something coercable to factor. For \code{mm_diffs}, differences are calculated against the base level of this variable.
#' @param \dots Additional arguments to \code{\link{amce}}, \code{\link{cj_freqs}}, or \code{\link{mm}}.
#' @author Thomas J. Leeper <thosjleeper@gmail.com>
#' @return \code{amce_diffs} and \code{mm_diffs} return a data frame similar to the one returned by \code{\link{cj}}, including a \code{BY} column (with the value \dQuote{Difference}) for easy merging with results returned by that function.
#' 
#' \code{cj_anova} returns an \code{\link[stats]{anova}} object.
#' 
#' @details \code{cj_anova} takes a model formula (\dQuote{reduced} model) and generates a \dQuote{full} model with two-way interactions between the variables specified in \code{by} and all RHS variables in \code{formula}, then computes an F-test comparing the two models, providing a test for whether preferences vary across levels of \code{by}. This is, in essence, a test of whether all such interaction coefficients are distinguishable from zero. (Because the test depends on overall model fit, not the coefficient variances, clustering is irrelevant.)
#' 
#' \code{mm_diffs} provides a data frame of differences in marginal means (literally differencing the results from \code{\link{mm}} across levels of \code{by}. This provides the clearest direct measure of preference differences from a conjoint design.
#' 
#' \code{amce_diffs} provides a data frame of differences in AMCEs (the coefficient on an interaction between each RHS factor and the variable in \code{by}). This provides an estimate of the difference in causal effects of each factor level relative to the baseline level (i.e., the difference in conditional AMCEs). This quantity is easily misinterpreted as the difference in preferences, which it is not. Rather it is a difference in the effect of the factor on preferences relative to the baseline/reference category of that feature. If preferences in the reference category differ across levels of \code{by}, the the difference in conditional AMCEs will have an unpredictable sign and significance, making differences in marginal means a more sensible quantity of interest. See \code{\link{amce_by_reference}} for a diagnostic.
#' 
#' Note: \code{amce_diffs} does not work with constrained designs. To obtain such differences, subset the design by constraints and calculate differences within each subset.
#' 
#' @examples
#' data("immigration")
#' immigration$contest_no <- factor(immigration$contest_no)
#' # Test for heterogeneity by profile order
#' cj_anova(immigration, ChosenImmigrant ~ Gender + Education + LanguageSkills, by = ~ contest_no)
#' 
#' # Test for heterogeneity by CountryOfOrigin feature
#' cj_anova(immigration, ChosenImmigrant ~ Gender + Education, by = ~ CountryOfOrigin)
#' 
#' \donttest{
#' # Differences in MMs by Gender feature
#' mm_diffs(immigration, ChosenImmigrant ~ LanguageSkills + Education, ~ Gender, id = ~ CaseID)
#' 
#' # Differences in AMCEs by Gender feature (i.e., feature interactions)
#' amce_diffs(immigration, ChosenImmigrant ~ LanguageSkills + Education, ~ Gender, id = ~ CaseID)
#' }
#'
#' # preferences differ for Male and Female immigrants with 'Broken English' ability
#' (m1 <- mm_diffs(immigration, ChosenImmigrant ~ LanguageSkills, ~ Gender, id = ~ CaseID))
#' 
#' # yet differences in conditional AMCEs  depend on the reference category
#' amce_diffs(immigration, ChosenImmigrant ~ LanguageSkills, ~ Gender, id = ~ CaseID)
#' immigration$LanguageSkills2 <- relevel(immigration$LanguageSkills, "Used Interpreter")
#' amce_diffs(immigration, ChosenImmigrant ~ LanguageSkills2, ~ Gender, id = ~ CaseID)
#'
#' # while differences in MMs do not depend on the reference cateory
#' (m2 <- mm_diffs(immigration, ChosenImmigrant ~ LanguageSkills2, ~ Gender, id = ~ CaseID))
#' 
#' @seealso \code{\link{amce}} \code{\link{mm}} \code{\link{cj_freqs}} \code{\link{plot.cj_amce}}
#' @importFrom lmtest coeftest
#' @export
amce_diffs <-
function(
  data,
  formula,
  by,
  id = ~ 0,
  weights = NULL,
  feature_order = NULL,
  feature_labels = NULL,
  level_order = c("ascending", "descending"),
  alpha = 0.05,
  ...
) {
    
    # coerce to "cj_df" to preserve attributes
    if (inherits(data, "survey.design")) {
        data2 <- cj_df(data[["variables"]])
    } else {
        data2 <- cj_df(data)
    }
    
    # get outcome variable
    outcome <- all.vars(stats::update(formula, . ~ 0))
    
    # get RHS variables, variable labels, and factor levels
    RHS <- all.vars(stats::update(formula, 0 ~ . ))
    by <- stats::update(by, ~ . )
    # sanity check that 'by' is only an single variable
    stopifnot(length(by) == 2L)
    by_var <- as.character(by)[2L]
    # coerce 'by_var' to factor
    if (!is.factor(data2[[by_var]])) {
        data2[[by_var]] <- factor(data2[[by_var]])
    }
    
    # process feature_order argument
    feature_order <- check_feature_order(feature_order, RHS)
    
    # set level_order (within features) to ascending or descending
    level_order <- match.arg(level_order)
    
    # function to produce "fancy" feature labels
    feature_labels <- clean_feature_labels(data = data2, RHS = RHS, feature_labels = feature_labels)
    
    # convert feature labels and levels to data frame
    term_labels_df <- make_term_labels_df(data2, feature_order, level_order = level_order)
    
    # modify formula to include appropriate interaction
    formula <- update(formula, reformulate(paste0("(.) * ", by_var)))
    
    # estimate model
    if (inherits(data, "data.frame") && is.null(weights)) {
        # coerce to "cj_df" to preserve attributes
        svydesign <- survey::svydesign(ids = id, weights = ~ 1, data = data2)
        mod <- survey::svyglm(formula, design = svydesign, ...)
    } else if (inherits(data, "data.frame")) {
        # coerce to "cj_df" to preserve attributes
        svydesign <- survey::svydesign(ids = id, weights = weights, data = data2)
        mod <- survey::svyglm(formula, design = svydesign, ...)
    } else if (inherits(data, "survey.design")) {
        svydesign <- data
        mod <- survey::svyglm(formula, design = svydesign, ...)
    } else {
        stop("'data' is not a 'data.frame' or 'survey.design' object")
    }
    
    # get model terms as rich data frame
    terms_df <- get_coef_metadata(mod = mod)
    
    # if the design is not constrained, then differences are simply interaction terms:
    if (all(terms_df[["_order"]] <= 2L)) {
        # keep only interactions between 'by_var'
        terms_df <- terms_df[terms_df[[by_var]] & terms_df[["_order"]] != 1, , drop = FALSE]
        
        # get coefficients as data frame (correct, if needed, for clustering)
        coef_summary <- get_coef_summary(mod = mod, data = data2, id = NULL, alpha = alpha) # don't pass id
        # merge coef_df and coef_summary
        coef_summary <- merge(coef_summary, terms_df, by = "_coef")
        
        # add feature and level based upon named columns from `terms_df`
        coef_summary[["feature"]] <- NA_character_
        coef_summary[["level"]] <- NA_character_
        for (i in seq_along(RHS)) {
            coef_summary[["feature"]][coef_summary[[RHS[i]]]] <- RHS[i]
            coef_summary[["level"]][coef_summary[[RHS[i]]]] <- coef_summary[[paste0("_level_", RHS[i])]][coef_summary[[RHS[i]]]]
        }
        
        # add other metadata columns
        coef_summary[["outcome"]] <- outcome
        coef_summary[["BY"]] <- paste0(coef_summary[[paste0("_level_", by_var)]], " - ", levels(data2[[by_var]])[1L])
        coef_summary[["statistic"]] <- "amce_difference"
    } else {
        stop("amce_diffs() currently does not support constrained designs")
    }
    
    # return
    out <- structure(coef_summary[, c("BY", "outcome", "statistic", "feature", "level", paste0("_level_", by_var), names(coef_summary)[c(2:7)])],
                     names = c("BY", "outcome", "statistic", "feature", "level", by_var, "estimate", "std.error", "z", "p", "lower", "upper"),
                     by = by_var,
                     class = c("cj_diffs", "data.frame"))
    out[["level"]] <- factor(out[["level"]], levels = term_labels_df[["level"]])
    out[["feature"]] <- factor(out[["feature"]], levels = feature_order, labels = feature_labels[feature_order])
    return(out)
}
