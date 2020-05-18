#' @rdname cj_freqs
#' @title Conjoint feature frequencies
#' @description Tabulate and visualize conjoint features, and their display frequencies and proportions
#' @param data A data frame containing variables specified in \code{formula}. All RHS variables should be factors; all levels across features should be unique.
#' @param formula An RHS formula specifying conjoint features to tabulate. All RHS variables should be factors; all levels across features should be unique.
#' @template id
#' @template weights
#' @param margin A numeric value passed to \code{\link[base]{prop.table}}. If \code{NULL} overall proportions are calculated.
#' @template feature_order
#' @template feature_labels
#' @template level_order
#' @param include_reference A logical indicating whether to include a \dQuote{reference} column that indicates whether a feature level is the reference category for that feature. Default is \code{FALSE}.
#' @param \dots Ignored.
#' @return A data frame of class \dQuote{cj_freqs}, \dQuote{cj_props}, etc.
#' @details These functions provide related but slightly different functionality. \code{cj_table} simply creates a data frame of features and their levels, which is useful for printing. \code{cj_props} provides tidy proportion tables to examine cross-feature restrictions in conjoint designs that are not equally randomized. This enables, for example, tabulation and visualization of complete restrictions (where combinations of two or more features are not permitted), as well as calculation of AMCEs for constrained designs appropriately weighted by the display proportions for particular combinations of features.
#' 
#' \code{cj_freqs} provides \emph{marginal} display frequencies, which are a descriptive check on the presentation of individual conjoint features (for example, to ensure equal or intentionally unequal appearance of levels). This is mostly useful for plotting functionality provided in \code{\link{plot.cj_freqs}}, which provides barcharts for the frequency with which each level of each feature was presented.
#'
#' @examples
#' data(immigration)
#' # identify all levels
#' cj_table(immigration, ~ Gender + Education + LanguageSkills)
#' cj_table(immigration, ~ Gender + Education + LanguageSkills, include_ref = TRUE)
#' 
#' # display frequencies
#' (f <- cj_freqs(immigration, ~ Gender + Education + LanguageSkills, id = ~ CaseID))
#' 
#' # restrictions
#' ## check display proportions
#' cj_props(immigration, ~ Job, id = ~ CaseID)
#' ## check which combinations were not allowed
#' subset(cj_props(immigration, ~ Job + Education, id = ~ CaseID), Proportion == 0)
#' 
#' \donttest{
#' # plotting
#' (p <- plot(f))
#'
#' ## change ggplot2 theme
#' p + ggplot2::theme_bw()
#'
#' ## monochrome bars
#' p + ggplot2::scale_fill_manual(values = rep("black", 9)) + 
#'   ggplot2::theme(legend.position = "none")
#' }
#' @seealso \code{\link{plot.cj_mm}}
#' @import stats
#' @importFrom survey svydesign svyby svymean
#' @export
cj_freqs <- 
function(
  data,
  formula,
  id = NULL,
  weights = NULL,
  feature_order = NULL,
  feature_labels = NULL,
  level_order = c("ascending", "descending"),
  ...
) {
    
    # get RHS variables, variable labels, and factor levels
    RHS <- all.vars(stats::update(formula, 0 ~ . ))
    
    # process feature_order argument
    feature_order <- check_feature_order(feature_order, RHS)
    
    # get `id` as character string
    if (!is.null(id)) {
        idvar <- all.vars(update(id, 0 ~ . ))
    } else {
        idvar <- NULL
    }
    
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
    out <- merge(coef_dat, make_term_labels_df(data, RHS), by = c("level"), all = TRUE)
    out[["level"]] <- factor(out[["level"]], levels = term_labels_df[["level"]])
    out[["feature"]] <- factor(out[["feature"]],
                               levels = feature_order,
                               labels = feature_labels[feature_order])
    
    # return organized data frame
    out[["statistic"]] <- "frequencies"
    out <- out[c("statistic", "feature", "level", "estimate")]
    out <- out[order(out[["level"]]),]
    rownames(out) <- seq_len(nrow(out))
    return(structure(out, class = c("cj_freqs", "data.frame")))
}
