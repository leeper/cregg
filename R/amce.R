#' @rdname amce
#' @title Tidy estimation of AMCEs
#' @description Estimate AMCEs for a conjoint analysis and return a tidy data frame of results
#' @param data A data frame containing variables specified in \code{formula}. All RHS variables should be factors; the base level for each will be used in estimation and its reported AMCE will be NA (for printing). Optionally, this can instead be an object of class \dQuote{survey.design} returned by \code{\link[survey]{svydesign}}.
#' @param formula A formula specifying an AMCE model to be estimated. All variables should be factors.
#' @param variable An RHS formula containing a single factor variable from \code{formula}. This will be used by \code{amce_by_reference} to estimate AMCEs relative to each possible factor level as a reference category. If more than one RHS variables are specified, the first will be used.
#' @template id
#' @template weights
#' @template feature_order
#' @template feature_labels
#' @template level_order
#' @template alpha
#' @param \dots For \code{amce}: additional arguments to \code{\link[stats]{glm}} or \code{\link[survey]{svyglm}}, the latter being used if \code{weights} is non-NULL. For \code{amce_by_reference}: additional arguments passed to \code{amce}.
#' @return A data frame
#' @details \code{amce} provides estimates of AMCEs (or rather, average marginal effects for each feature level). It does not calculate AMCEs for constrained conjoint designs. The function can also be used for balance testing by specifying a covariate rather outcome on the left-hand side of \code{formula}. See examples.
#' 
#' \code{amce_by_reference} provides a tool for quick sensitivity analysis. AMCEs are defined relative to an arbitrary reference category (i.e., feature level). This function will loop over all feature levels (for a specified feature) to show how interpretation will be affected by choice of reference category. The resulting data frame will be a stacked result from \code{amce}, containing an additional \code{.reference} column specifying which level of \code{variable} was used as the reference category.
#' 
#' Users may desire to specify a \code{family} argument via \code{\dots}, which should be a \dQuote{family} object such as \code{gaussian}. Sensible alternatives are \code{binomial} (for binary outcomes) and quasibinomial (for weighted survey data). See \code{\link[stats]{family}} for details.
#' @examples
#' data(hainmueller)
#' # estimating AMCEs
#' amce(hainmueller, ChosenImmigrant ~ Gender + Education + LanguageSkills, 
#'      id = ~ CaseID, feature_order = c("LanguageSkills", "Gender", "Education"))
#' 
#' \dontrun{
#' # balance testing example
#' plot(amce(hainmueller[!is.na(hainmueller$ethnocentrism),
#'      ethnocentrism ~ Gender + Education + LanguageSkills, id = ~ CaseID))
#' 
#' # reference category sensitivity
#' x <- amce_by_reference(hainmueller, ChosenImmigrant ~ LanguageSkills + Education, 
#'        variable = ~ LanguageSkills, id = ~ CaseID)
#' plot(x, group = "BY")
#' }
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
         level_order = c("ascending", "descending"),
         alpha = 0.05,
         ...
) {

    # get outcome variable
    outcome <- all.vars(stats::update(formula, . ~ 0))
    
    # get RHS variables, variable labels, and factor levels
    RHS <- all.vars(stats::update(formula, 0 ~ . ))
    
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
        coef_dat <- summary(margins::margins(mod, data = data, vcov = vc), level = 1-alpha)
    } else {
        coef_dat <- summary(margins::margins(mod, data = data, design = svydesign, vcov = vc), level = 1-alpha)
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

#' @rdname amce
#' @export
amce_by_reference <- function(data, formula, variable, ...) {
    # get outcome variable
    variable <- all.vars(stats::update(variable, 0 ~ .))[[1L]]
    
    # get levels
    levs <- levels(data[[variable]])
    
    # loop over levels
    out <- list()
    for (i in seq_along(levs)) {
        data[[variable]] <- relevel(data[[variable]], levs[i])
        out[[i]] <- amce(data, formula, ...)
        out[[i]][["BY"]] <- levs[i]
    }

    # return value
    ## stack
    out <- do.call("rbind", out)
    ## add reference category column
    out[["BY"]] <- factor(out[["BY"]], levels = levs)
    return(structure(out, class = c("cj_amce", "data.frame"), by = "BY"))
}
