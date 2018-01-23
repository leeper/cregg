#' @title Conjoint feature frequencies
#' @description Tabulate and visualize conjoint feature frequencies
#' @param data A data frame containing variables specified in \code{formula}. All RHS variables should be factors.
#' @param formula An RHS formula specifying conjoint features to tabulate. All RHS variables should be factors.
#' @param id An RHS formula specifying a variable holding respondent identifiers, to be used for clustering standard errors.
#' @param weights An (optional) RHS formula specifying a variable holding survey weights.
#' @param feature_order An (optional) character vector specifying the names of feature (RHS) variables in the order they should be encoded in the resulting data frame.
#' @param feature_labels A named list of \dQuote{fancy} feature labels to be used in output. By default, the function looks for a \dQuote{label} attribute on each variable in \code{formula} and uses that for pretty printing. This argument overrides those attributes or otherwise provides fancy labels for this purpose. This should be a list with names equal to variables in \code{formula} and character string values; arguments passed here override variable attributes.
#' @param \dots Ignored.
#' @details \code{freqs} provides a descriptive check on the presentation of conjoint features (to ensure equal or intentionally unequal appearance of levels). This is mostly useful for plotting functionality provided in \code{\link{plot.cj_freqs}}.
#' @examples
#' data(hainmueller)
#' freqs(hainmueller, ~ Gender + Education + LanguageSkills, id = ~ CaseID)
#' @seealso \code{\link{plot.cj_mm}}
#' @import stats
#' @importFrom survey svydesign svyby svymean
#' @export
freqs <- 
function(data,
         formula,
         id,
         weights = NULL,
         feature_order = NULL,
         feature_labels = NULL,
         ...
) {
    
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
    
    # get `id` as character string
    idvar <- all.vars(update(id, 0 ~ . ))
    
    # get `weights` as character string
    if (!is.null(weights)) {
        weightsvar <- all.vars(update(weights, 0 ~ . ))
    } else {
        weightsvar <- NULL
    }
    
    # function used in cj and ammplot to produce "fancy" feature labels
    feature_labels <- clean_feature_labels(data = data, RHS = RHS, feature_labels = feature_labels)
    
    # convert feature labels and levels to data frame
    term_labels_df <- make_term_labels_df(data, feature_order)
    
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
        svylong <- survey::svydesign(ids = id, weights = weights, data = long)
    } else {
        svylong <- survey::svydesign(ids = id, weights = ~0, data = long)
    }
    
    # calculate AMMs, SEs, etc.
    coef_dat <- data.frame(survey::svytable(~ Level, design = svylong))
    names(coef_dat) <- c("level", "estimate")
    
    # attach feature labels
    coef_dat <- merge(coef_dat, make_term_labels_df(data, RHS), by = c("level"), all = TRUE)
    coef_dat$feature <- factor(coef_dat$feature,
                               levels = feature_order,
                               labels = feature_labels[feature_order])
    
    # return organized data frame
    coef_dat <- coef_dat[c("feature", "level", "estimate")]
    coef_dat <- coef_dat[order(coef_dat$level),]
    rownames(coef_dat) <- seq_len(nrow(coef_dat))
    return(structure(coef_dat, class = c("cj_freqs", "data.frame")))
}
