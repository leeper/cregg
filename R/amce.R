#' @title Tidy estimation of AMCEs
#' @description Estimate AMCEs for a conjoint analysis and return a tidy data frame of results
#' @param data A data frame containing variables specified in \code{formula}. All RHS variables should be factors; the base level for each will be used in estimation and its reported AMCE will be zero (for printing).
#' @param formula A formula specifying an AMCE model to be estimated. All variables should be factors.
#' @param id An RHS formula specifying a variable holding respondent identifiers, to be used for clustering standard errors.
#' @param weights An (optional) RHS formula specifying a variable holding survey weights.
#' @param feature_order An (optional) character vector specifying the names of feature (RHS) variables in the order they should be encoded in the resulting data frame.
#' @param feature_labels A named list of \dQuote{fancy} feature labels to be used in output. By default, the function looks for a \dQuote{label} attribute on each variable in \code{formula} and uses that for pretty printing. This argument overrides those attributes or otherwise provides fancy labels for this purpose. This should be a list with names equal to variables in \code{formula} and character string values; arguments passed here override variable attributes.
#' @param level A numeric value indicating the significance level at which to calculate confidence intervals for the AMCEs (by default 0.95, meaning 95-percent CIs are returned).
#' @param \dots Additional arguments to \code{\link[stats]{glm}} or \code{\link[survey]{svyglm}}, the latter being used if \code{weights} is non-NULL.
#' @return A data frame
#' @details Users may desire to specify a \code{family} argument via \code{\dots}, which should be a \dQuote{family} object such as \code{\link[stats]{gaussian}}. Sensible alternatives are \code{\link[stats]{binomial}} (for binary outcomes) and \code{\link[stats]{quasibinomial}} (for weighted survey data).
#' @examples
#' data(hainmueller)
#' set.seed(12345)
#' cj(hainmueller, ChosenImmigrant ~ Gender + Education + LanguageSkills, 
#'    id = ~ CaseID, feature_order = c("LanguageSkills", "Gender", "Education"))
#'  
#' @seealso \code{\link{plot.cj_amce}}
#' @import stats
#' @importFrom sandwich vcovCL
#' @export
amce <- 
function(data,
         formula,
         id = NULL,
         weights = NULL,
         feature_order = NULL,
         feature_labels = NULL,
         level = 0.95,
         ...
) {

    # get outcome variable
    outcome <- all.vars(stats::update(formula, . ~ 0))
    
    # get RHS variables, variable labels, and factor levels
    RHS <- all.vars(stats::update(formula, 0 ~ . ))
    
    # function to produce "fancy" feature labels
    feature_labels <- clean_feature_labels(data = data, RHS = RHS, feature_labels = feature_labels)
    
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
    
    # convert feature labels and levels to data frame
    term_labels_df <- make_term_labels_df(data, feature_order)
    
    # estimate model
    if (inherits(data, "data.frame") && is.null(weights)) {
        svydesign <- NULL
        mod <- stats::glm(formula, data = data, ...)
    } else if (inherits(data, "data.frame")) {
        svydesign <- survey::svydesign(ids = id, weights = weights, data = data)
        mod <- survey::svyglm(formula, design = svydesign, ...)
    } else if (inherits(data, "survey.design")) {
        svydesign <- data
        mod <- survey::svyglm(formula, design = svydesign, ...)
    } else {
        stop("'data' is not a 'data.frame' or 'survey.design' object")
    }
    
    # calculate marginal effects and standard errors
    ## standard errors
    if (is.null(id) || !length(all.vars(id))) {
        vc <- stats::vcov(mod)
    } else {
        if (inherits(data, "data.frame")) {
            cluster_vector <- stats::get_all_vars(id, data)[[1L]]
        } else if (inherits(data, "survey.design")) {
            cluster_vector <- stats::get_all_vars(id, data[["variables"]])[[1L]]
        }
        vc <- sandwich::vcovCL(mod, cluster_vector)
    }
    ## marginal effects
    if (is.null(svydesign)) {
        coef_dat <- summary(margins::margins(mod, data = data, vcov = vc), level = level)
    } else {
        coef_dat <- summary(margins::margins(mod, data = data, design = svydesign, vcov = vc), level = level)
    }
    names(coef_dat) <- c("factor", "estimate", "std.error", "z", "p", "lower", "upper")
    
    # cleanup term names
    coef_dat <- cbind(clean_term_names(coef_dat$factor, RHS), coef_dat[-1L])
    
    # fill in missing base levels with 0s
    coef_dat <- merge(coef_dat[!coef_dat$feature %in% c("", "(Intercept)"),], 
                      term_labels_df, 
                      by = c("feature", "level"), all = TRUE)
    coef_dat$estimate[is.na(coef_dat$estimate)] <- 0
    
    # label features and levels
    coef_dat$feature <- factor(coef_dat$feature,
                               levels = feature_order,
                               labels = feature_labels[feature_order])
    coef_dat$level <- factor(coef_dat$level, levels = term_labels_df$level)
    coef_dat$outcome <- outcome
    
    # return
    out <- coef_dat[c("outcome", "feature", "level", "estimate", "std.error", "z", "p", "lower", "upper")]
    out <- out[order(out$level),]
    rownames(out) <- seq_len(nrow(out))
    return(structure(out, class = c("cj_amce", "data.frame")))
}
