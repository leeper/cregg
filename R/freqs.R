#' @rdname freqs
#' @title Conjoint feature frequencies
#' @description Tabulate and visualize conjoint feature frequencies and proportions
#' @param data A data frame containing variables specified in \code{formula}. All RHS variables should be factors.
#' @param formula An RHS formula specifying conjoint features to tabulate. All RHS variables should be factors.
#' @template id
#' @template weights
#' @param margin A numeric value passed to \code{\link[base]{prop.table}}. If \code{NULL} overall proportions are calculated.
#' @template feature_order
#' @template feature_labels
#' @template level_order
#' @param \dots Ignored.
#' @details These two functions provide slightly different functionality. \code{props} provides tidy proportion tables to examine cross-feature restrictions in conjoint designs that are not equally randomized. This enables, for example, tabulation and visualization of complete restrictions (where combinations of two or more features are not permitted), as well as calculation of AMCEs for constrained designs appropriately weighted by the display proportions for particular combinations of features.
#' 
#' \code{freqs} provides \emph{marginal} display frequencies, which are a descriptive check on the presentation of individual conjoint features (for example, to ensure equal or intentionally unequal appearance of levels). This is mostly useful for plotting functionality provided in \code{\link{plot.cj_freqs}}, which provides barcharts for the frequency with which each level of each feature was presented.
#'
#' @examples
#' data(hainmueller)
#' # display frequencies
#' freqs(hainmueller, ~ Gender + Education + LanguageSkills, id = ~ CaseID)
#' 
#' # restrictions
#' ## check display proportions
#' props(hainmueller, ~ Job, id = ~ CaseID)
#' ## check which combinations were not allowed
#' subset(props(hainmueller, ~ Job + Education, id = ~ CaseID), Proportion == 0)
#' 
#' @seealso \code{\link{plot.cj_mm}}
#' @import stats
#' @importFrom survey svydesign svyby svymean
#' @export
freqs <- 
function(
  data,
  formula,
  id,
  weights = NULL,
  feature_order = NULL,
  feature_labels = NULL,
  level_order = c("ascending", "descending"),
  ...
) {
    
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
    
    # get `weights` as character string
    if (!is.null(weights)) {
        weightsvar <- all.vars(update(weights, 0 ~ . ))
    } else {
        weightsvar <- NULL
    }
    
    # set level_order (within features) to ascending or descending
    level_order <- match.arg(level_order)
    
    # function to produce "fancy" feature labels
    feature_labels <- clean_feature_labels(data = data, RHS = RHS, feature_labels = feature_labels)
    
    # convert feature labels and levels to data frame
    term_labels_df <- make_term_labels_df(data, feature_order, level_order = level_order)
    
    # reshape data
    long <- stats::reshape(data[c(RHS, idvar, weightsvar)], 
                           varying = list(names(data[RHS])), 
                           v.names = "Level", 
                           timevar = "Feature",
                           times = names(data[RHS]),
                           idvar = "observation",
                           direction = "long")
    
    # convert to survey object
    if (!is.null(weights)) {
        svylong <- survey::svydesign(ids = ~ 0, weights = weights, data = long)
    } else {
        svylong <- survey::svydesign(ids = ~ 0, weights = ~0, data = long)
    }
    
    # calculate AMMs, SEs, etc.
    coef_dat <- data.frame(survey::svytable(~ Level, design = svylong))
    names(coef_dat) <- c("level", "estimate")
    
    # attach feature labels
    coef_dat <- merge(coef_dat, make_term_labels_df(data, RHS), by = c("level"), all = TRUE)
    coef_dat$feature <- factor(coef_dat$feature,
                               levels = feature_order,
                               labels = feature_labels[feature_order])
    coef_dat$level <- factor(coef_dat$level, levels = term_labels_df$level)
    
    # return organized data frame
    coef_dat <- coef_dat[c("feature", "level", "estimate")]
    coef_dat <- coef_dat[order(coef_dat$level),]
    rownames(coef_dat) <- seq_len(nrow(coef_dat))
    return(structure(coef_dat, class = c("cj_freqs", "data.frame")))
}

#' @rdname freqs
#' @export
props <- 
function(data,
         formula,
         id,
         weights = NULL,
         margin = NULL,
         ...
) {
    
    # create survey design object
    if (inherits(data, "data.frame") && is.null(weights)) {
        svydesign <- survey::svydesign(ids = ~ 0, weights = ~ 1, data = data)
    } else if (inherits(data, "data.frame")) {
        svydesign <- survey::svydesign(ids = ~ 0, weights = weights, data = data)
    } else if (inherits(data, "survey.design")) {
        svydesign <- data
    } else {
        stop("'data' is not a 'data.frame' or 'survey.design' object")
    }
    
    # calculate display frequencies
    out <- data.frame(prop.table(survey::svytable(formula, design = svydesign), margin = margin))
    
    # return
    names(out)[names(out) == "Freq"] <- "Proportion"
    return(structure(out, class = c("cj_props", "data.frame")))
}
