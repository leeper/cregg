#' @rdname mm
#' @title Marginal Means
#' @description Calculate (descriptive) marginal means (MMs) from a conjoint design
#' @param data A data frame containing variables specified in \code{formula}. All RHS variables should be factors.
#' @param formula A formula specifying an outcome (LHS) and conjoint features (RHS) to describe. All variables should be factors; all levels across features should be unique, with constraints specified with an asterisk (*) between features, as in \code{amce}.
#' @template id
#' @template weights
#' @template feature_order
#' @template feature_labels
#' @template level_order
#' @param h0 A numeric value specifying a null hypothesis value to use when generating z-statistics and p-values.
#' @template alpha
#' @param \dots Ignored.
#' @details \code{mm} provides descriptive representations of conjoint data as marginal means (MMs), which represent the mean outcome across all appearances of a particular conjoint feature level, averaging across all other features. In forced choice conjoint designs with two profiles per choice task, MMs by definition average 0.5 with values above 0.5 indicating features that increase profile favorability and values below 0.5 indicating features that decrease profile favorability. For continuous outcomes, MMs can take any value in the full range of the outcome.
#' 
#' But note that if feature levels can co-occur, such that both alternatives share a feature level, then the MMs on forced choice outcomes are bounded by the probability of co-occurrence (as a lower bound) and 1 minus that probability as an upper bound.
#' 
#' Plotting functionality is provided in \code{\link{plot.cj_mm}}.
#' 
#' @examples
#' \donttest{
#' data(immigration)
#' # marginal means
#' mm(immigration, ChosenImmigrant ~ Gender + Education + LanguageSkills,
#'    id = ~ CaseID, h0 = 0.5)
#'
#' # marginal means with design constraints
#' mm(immigration, ChosenImmigrant ~ Gender + LanguageSkills + PriorEntry + 
#'      CountryOfOrigin * ReasonForApplication, id = ~CaseID)
#'
#' # higher-order marginal means with feature interactions
#' immigration$language_entry <- 
#'   interaction(immigration$LanguageSkills, immigration$PriorEntry, sep = "_")
#' mm(immigration, ChosenImmigrant ~ language_entry,
#'    id = ~CaseID)
#' }
#' @seealso \code{\link{mm_diffs}} \code{\link{plot.cj_mm}}
#' @import stats
#' @importFrom survey svydesign svyby svymean
#' @export
mm <- 
function(
  data,
  formula,
  id = NULL,
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
    if (!length(outcome) || outcome == ".") {
        stop("'formula' is missing a left-hand outcome variable")
    }
    
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
    out <- survey::svyby(~ OUTCOME, ~ Level, FUN = survey::svymean, design = svylong, na.rm = TRUE)
    out[["z"]] <- (out[["OUTCOME"]] - h0)/out[["se"]]
    out[["p"]] <- (2*stats::pnorm(-abs(out[["z"]])))
    out[["lower"]] <- out[["OUTCOME"]] - stats::qnorm((1-alpha) + (alpha/2)) * out[["se"]]
    out[["upper"]] <- out[["OUTCOME"]] + stats::qnorm((1-alpha) + (alpha/2)) * out[["se"]]
    names(out) <- c("level", "estimate", "std.error", "z", "p", "lower", "upper")
    
    # attach feature labels
    out <- merge(out, make_term_labels_df(data, RHS), by = c("level"), all = TRUE)
    out[["level"]] <- factor(out[["level"]], levels = term_labels_df[["level"]])
    out[["feature"]] <- factor(out[["feature"]],
                               levels = feature_order,
                               labels = feature_labels[feature_order])
    out[["outcome"]] <- outcome
    
    # return organized data frame
    out[["statistic"]] <- "mm"
    out <- out[c("outcome", "statistic", "feature", "level", "estimate", "std.error", "z", "p", "lower", "upper")]
    out <- out[order(out[["level"]]),]
    rownames(out) <- seq_len(nrow(out))
    return(structure(out, class = c("cj_mm", "data.frame")))
}
