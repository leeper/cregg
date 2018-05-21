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
#' data(hainmueller)
#' # Test for heterogeneity by profile order
#' cj_anova(hainmueller, ChosenImmigrant ~ Gender + Education + LanguageSkills, by = ~ contest_no)
#' 
#' # Test for heterogeneity by CountryOfOrigin feature
#' cj_anova(hainmueller, ChosenImmigrant ~ Gender + Education, by = ~ CountryOfOrigin)
#' 
#' \dontrun{
#' # Differences in MMs by Gender feature
#' mm_diffs(hainmueller, ChosenImmigrant ~ LanguageSkills + Education, ~ Gender, id = ~ CaseID)
#' 
#' # Differences in AMCEs by Gender feature (i.e., feature interactions)
#' amce_diffs(hainmueller, ChosenImmigrant ~ LanguageSkills + Education, ~ Gender, id = ~ CaseID)
#' }
#'
#' # preferences differ for Male and Female immigrants with 'Broken English' ability
#' mm_diffs(hainmueller, ChosenImmigrant ~ LanguageSkills, ~ Gender, id = ~ CaseID)
#' 
#' # yet differences in conditional AMCEs  depend on the reference category
#' amce_diffs(hainmueller, ChosenImmigrant ~ LanguageSkills, ~ Gender, id = ~ CaseID)
#' hainmueller$LanguageSkills2 <- relevel(hainmueller$LanguageSkills, "Used Interpreter")
#' amce_diffs(hainmueller, ChosenImmigrant ~ LanguageSkills2, ~ Gender, id = ~ CaseID)
#'
#' @seealso \code{\link{amce}} \code{\link{mm}} \code{\link{freqs}} \code{\link{plot.cj_amce}}
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
    if (!is.null(feature_order)) {
        if (length(RHS) > length(feature_order)) {
            warning("'feature_order' appears to be missing values")
        } else if (length(RHS) < length(feature_order)) {
            warning("'feature_order' appears to have excess values")
        }
    } else {
        feature_order <- RHS
    }
    
    # get `id` as character string
    idvar <- all.vars(update(id, 0 ~ . ))
    
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
    
    # extract coefficient names
    coefs <- coef(mod)
    
    # extract terms
    model_terms <- terms(mod)
    if (any(attr(model_terms, "order")) > 2L) {
        stop("Function behavior with higher-order interaction terms is undefined.")
    }
    
    # is there an intercept?
    intercept <- if (attr(model_terms, "intercept") == 1L) TRUE else FALSE
    
    # extract 'assign' attribute vector and attach names from formula terms
    assign_vec <- attr(model.matrix(mod), "assign")
    if (intercept) {
        # drop intercept from 'assign' vector temporarily
        assign_vec <- assign_vec[-1L]
        # drop intercept from 'coefs'
        coefs <- coefs[-1L]
    }
    names(assign_vec) <- attr(model_terms, "term.labels")[assign_vec]
    
    # extract 'factor' attribute, which is a variable-by-term matrix
    model_factors <- attr(model_terms, "factors")
    
    ## identify terms for each coefficient (using 'assign_vec' to extract)
    ## any column (a coef) with more than one non-zero entry mean involves two or more variables
    ## Note: it has to be non-zero because the matrix can contain 1s and 2s, which have different substantive meanings
    model_factors_df <- data.frame(t(model_factors[-1L, assign_vec, drop = FALSE] != 0), check.names = FALSE)
    
    # build a data frame of the coefficients and information from terms() and 'assign' attribute
    coef_df <- cbind.data.frame(
      # add coefficient names
      "_name" = names(coefs),
      # add coefficient estimate values
      #"_estimate" = coefs,
      # identify 'term' names
      "_term" = colnames(model_factors)[assign_vec],
      # identify interaction terms
      "_order" = attr(model_terms, "order")[assign_vec],
      # identify coefficients involving 'by_var'
      "_by" = model_factors_df[[by_var]]
    )
    # cleanup rownames
    rownames(coef_df) <- seq_len(nrow(coef_df))
    # convenience step to identify interactions
    coef_df[["_interaction"]] <- coef_df[["_order"]] > 1L
    
    # determine the variable that 'by_var' is interacted with
    coef_df[["_base_var"]] <- NA_character_
    base_term_list <- lapply(model_factors_df[setdiff(names(model_factors_df), by_var)], which)
    for (i in seq_along(base_term_list)) {
        coef_df[["_base_var"]][base_term_list[[i]]] <- names(base_term_list)[i]
    }
    
    # add factor levels for 'by_var' to 'coef_df'
    coef_df[["_by_level"]] <- NA_character_
    coef_df[["_base_level"]] <- NA_character_
        
    # get contrasts
    con <- contrasts(data[[by_var]])
    
    # apply function to data
    for (i in seq_len(nrow(coef_df))) {
        # if first-order term, don't apply function instead figure out base and by level manually
        if (coef_df[["_order"]][i] == 1L) {
            if (coef_df[["_term"]][i] == by_var) {
                # do nothing, because we'll just delete these rows below, momentarily
                coef_df[["_by_level"]][i] <- rownames(con)[1L]
            } else {
                # variable is first-order term for base variable
                coef_df[["_by_level"]][i] <- rownames(con)[1L]
                coef_df[["_base_level"]][i] <- regmatches(coef_df[["_name"]][i], 
                                                          regexpr(paste0("(?<=", coef_df[["_term"]][i], ").+"),
                                                                  coef_df[["_name"]][i],
                                                                  perl = TRUE))
            }
        } else {
            # use utility function to split coefficient names
            tmp <- split_coef_name_by_term(as.character(coef_df[["_name"]][i]), as.character(coef_df[["_term"]][i]))
            coef_df[["_base_level"]][i] <- tmp[1L]
            coef_df[["_by_level"]][i] <- paste0(tmp[2L], " - ", rownames(con)[1L])
        }
    }
    
    # drop lowest level of '_by_level' to leave only differences
    coef_df <- coef_df[!coef_df[["_by_level"]] == rownames(con)[1L], , drop = FALSE]
    
    # setup full coef summary (only includes subset of coefficients that are estimable)
    estimate_summary <- summary(mod)
    coef_summary <- coef(estimate_summary)
    confints <- confint(mod, level = 1-alpha)
    colnames(confints) <- c("lower", "upper")
    coef_summary <- cbind(coef_summary, confints)
    
    # populate 'coef_summary' with non-estimable coefficients ("aliased")
    if (any(aliased <- estimate_summary$aliased)) {
        cn <- names(aliased)
        coefs_tmp <- matrix(NA, length(aliased), 6, dimnames = list(cn, colnames(coef_summary)))
        coefs_tmp[!aliased, ] <- coef_summary
        coef_summary <- coefs_tmp
        rm(coefs_tmp)
    }
    # drop intercept if present
    if (intercept) {
        coef_summary <- coef_summary[-1L, , drop = FALSE]
    }
    coef_summary <- as.data.frame(coef_summary)
    coef_summary[["_name"]] <- rownames(coef_summary)
    rownames(coef_summary) <- seq_len(nrow(coef_summary))
    
    # merge coef_df and coef_summary
    merged <- merge(coef_summary, coef_df, by = "_name")
    merged[["outcome"]] <- outcome
    merged[["BY"]] <- "Difference"
    
    # return
    out <- structure(merged[, c("BY", "outcome", "_base_var", "_base_level", "_by_level", names(merged)[c(2:7)])],
                     names = c("BY", "outcome", "feature", "level", by_var, "estimate", "std.error", "t", "p", "lower", "upper"),
                     by = by_var,
                     class = c("cj_diffs", "data.frame"))
    out$feature <- factor(out$feature, levels = feature_order, labels = feature_labels[feature_order])
    out$level <- factor(out$level, levels = term_labels_df$level)
    return(out)
}
