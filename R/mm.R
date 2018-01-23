#' @title Marginal Means
#' @description Calculate (descriptive) marginal means (MMs) from a conjoint design
#' @param data A data frame containing variables specified in \code{formula}. All RHS variables should be factors; the base level for each will be used in estimation and its reported marginal mean will be zero (for printing).
#' @param formula A formula specifying an outcome (LHS) and conjoint features (RHS) to describe. All RHS variables should be factors.
#' @param id An RHS formula specifying a variable holding respondent identifiers, to be used for clustering standard errors.
#' @param weights An (optional) RHS formula specifying a variable holding survey weights.
#' @param feature_order An (optional) character vector specifying the names of feature (RHS) variables in the order they should be encoded in the resulting data frame.
#' @param feature_labels A named list of \dQuote{fancy} feature labels to be used in output. By default, the function looks for a \dQuote{label} attribute on each variable in \code{formula} and uses that for pretty printing. This argument overrides those attributes or otherwise provides fancy labels for this purpose. This should be a list with names equal to variables in \code{formula} and character string values; arguments passed here override variable attributes.
#' @param level A numeric value indicating the significance level at which to calculate confidence intervals for the MMs (by default 0.95, meaning 95-percent CIs are returned).
#' @param \dots Ignored.
#' @details \code{mm} provides descriptive representations of conjoint data as marginal means (MMs), which represent the mean outcome across all appearances of a particular conjoint feature level, averaging across all other features. In forced choice conjoint designs, MMs by definition average 0.5 with values above 0.5 indicating features that increase profile favorability and values below 0.5 indicating features that decrease profile favorability. For continuous outcomes, AMMs can take any value in the full range of the outcome. Plotting functionality is provided in \code{\link{plot.cj_mm}}.
#' @examples
#' data(hainmueller)
#' set.seed(12345)
#' hainmueller$weights <- runif(nrow(hainmueller))
#' mm(hainmueller, ChosenImmigrant ~ Gender + Education + LanguageSkills, id = ~ CaseID, weights = ~ weights)
#' @seealso \code{\link{plot.cj_mm}}
#' @import stats
#' @importFrom survey svydesign svyby svymean
#' @export
mm <- 
function(data,
         formula,
         id,
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
        svylong <- survey::svydesign(ids = id, weights = weights, data = long)
    } else {
        svylong <- survey::svydesign(ids = id, weights = ~0, data = long)
    }
    
    # calculate AMMs, SEs, etc.
    coef_dat <- survey::svyby(~ OUTCOME, ~ Level, FUN = survey::svymean, design = svylong)
    coef_dat$z <- coef_dat$OUTCOME/coef_dat$se
    coef_dat$p <- 2*stats::pnorm(-coef_dat$z)
    coef_dat$lower <- coef_dat$OUTCOME - stats::qnorm(level + ((1-level)/2)) * coef_dat$se
    coef_dat$upper <- coef_dat$OUTCOME + stats::qnorm(level + ((1-level)/2)) * coef_dat$se
    names(coef_dat) <- c("level", "estimate", "std.error", "z", "p", "lower", "upper")
    
    # attach feature labels
    coef_dat <- merge(coef_dat, make_term_labels_df(data, RHS), by = c("level"), all = TRUE)
    coef_dat$feature <- factor(coef_dat$feature,
                               levels = feature_order,
                               labels = feature_labels[feature_order])
    coef_dat$outcome <- outcome
    
    # return organized data frame
    coef_dat <- coef_dat[c("outcome", "feature", "level", "estimate", "std.error", "z", "p", "lower", "upper")]
    coef_dat <- coef_dat[order(coef_dat$level),]
    rownames(coef_dat) <- seq_len(nrow(coef_dat))
    return(structure(coef_dat, class = c("cj_mm", "data.frame")))
}
