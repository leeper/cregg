#' @rdname mm
#' @title Marginal Means
#' @description Calculate (descriptive) marginal means (MMs) from a conjoint design
#' @param data A data frame containing variables specified in \code{formula}. All RHS variables should be factors.
#' @param formula A formula specifying an outcome (LHS) and conjoint features (RHS) to describe. All RHS variables should be factors.
#' @template id
#' @template weights
#' @template feature_order
#' @template feature_labels
#' @template level_order
#' @param h0 A numeric value specifying a null hypothesis value to use when generating z-statistics and p-values.
#' @template alpha
#' @param \dots Ignored.
#' @details \code{mm} provides descriptive representations of conjoint data as marginal means (MMs), which represent the mean outcome across all appearances of a particular conjoint feature level, averaging across all other features. In forced choice conjoint designs, MMs by definition average 0.5 with values above 0.5 indicating features that increase profile favorability and values below 0.5 indicating features that decrease profile favorability. For continuous outcomes, MMs can take any value in the full range of the outcome.
#' 
#' But note that if feature levels can co-occur, such that both alternatives share a feature level, then the MMs on forced choice outcomes are bounded by the probability of co-occurrence (as a lower bound) and 1 minus that probability as an upper bound.
#' 
#' Plotting functionality is provided in \code{\link{plot.cj_mm}}.
#' 
#' @examples
#' data(hainmueller)
#' mm(hainmueller, ChosenImmigrant ~ Gender + Education + LanguageSkills,
#'    id = ~ CaseID, h0 = 0.5)
#' @seealso \code{\link{mm_diffs}} \code{\link{plot.cj_mm}}
#' @import stats
#' @importFrom survey svydesign svyby svymean
#' @export
mm <- 
function(
  data,
  formula,
  id,
  weights = NULL,
  feature_order = NULL,
  feature_labels = NULL,
  level_order = c("ascending", "descending"),
  h0 = 0,
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
    
    # function used in cj and ammplot to produce "fancy" feature labels
    feature_labels <- clean_feature_labels(data = data, RHS = RHS, feature_labels = feature_labels)
    
    # convert feature labels and levels to data frame
    term_labels_df <- make_term_labels_df(data, feature_order, level_order = level_order)
    
    # get `weights` as character string
    if (!is.null(weights)) {
        weightsvar <- all.vars(update(weights, 0 ~ . ))
    } else {
        weightsvar <- NULL
    }
    
    # reshape data
    long <- stats::reshape(data[c(outcome, RHS, idvar, weightsvar)], 
                           varying = list(names(data[RHS])), 
                           v.names = "Level", 
                           timevar = "Feature",
                           times = names(data[RHS]),
                           idvar = "observation",
                           direction = "long")
    names(long)[names(long) == outcome] <- "OUTCOME"
    
    # convert to survey object
    if (!is.null(weights)) {
        svylong <- survey::svydesign(ids = ~ 0, weights = weights, data = long)
    } else {
        svylong <- survey::svydesign(ids = ~ 0, weights = ~0, data = long)
    }
    
    # calculate MMs, SEs, etc.
    coef_dat <- survey::svyby(~ OUTCOME, ~ Level, FUN = survey::svymean, design = svylong, na.rm = TRUE)
    coef_dat$z <- (coef_dat$OUTCOME - h0)/coef_dat$se
    coef_dat$p <- 2*stats::pnorm(-coef_dat$z)
    coef_dat$lower <- coef_dat$OUTCOME - stats::qnorm((1-alpha) + (alpha/2)) * coef_dat$se
    coef_dat$upper <- coef_dat$OUTCOME + stats::qnorm((1-alpha) + (alpha/2)) * coef_dat$se
    names(coef_dat) <- c("level", "estimate", "std.error", "z", "p", "lower", "upper")
    
    # attach feature labels
    coef_dat <- merge(coef_dat, make_term_labels_df(data, RHS), by = c("level"), all = TRUE)
    coef_dat$feature <- factor(coef_dat$feature,
                               levels = feature_order,
                               labels = feature_labels[feature_order])
    coef_dat$level <- factor(coef_dat$level, levels = term_labels_df$level)
    coef_dat$outcome <- outcome
    
    # return organized data frame
    coef_dat <- coef_dat[c("outcome", "feature", "level", "estimate", "std.error", "z", "p", "lower", "upper")]
    coef_dat <- coef_dat[order(coef_dat$level),]
    rownames(coef_dat) <- seq_len(nrow(coef_dat))
    return(structure(coef_dat, class = c("cj_mm", "data.frame")))
}
