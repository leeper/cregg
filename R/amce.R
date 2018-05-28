#' @rdname amce
#' @title Tidy estimation of AMCEs
#' @description Estimate AMCEs for a conjoint analysis and return a tidy data frame of results
#' @param data A data frame containing variables specified in \code{formula}. All RHS variables should be factors; the base level for each will be used in estimation and its reported AMCE will be NA (for printing). Optionally, this can instead be an object of class \dQuote{survey.design} returned by \code{\link[survey]{svydesign}}.
#' @param formula A formula specifying an AMCE model to be estimated. All variables should be factors.
#' @param variable An RHS formula containing a single factor variable from \code{formula}. This will be used by \code{amce_by_reference} to estimate AMCEs relative to each possible factor level as a reference category. If more than one RHS variables are specified, the first will be used.
#' @template id
#' @template weights
#' @template constraints
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
#' data("taxes")
#' # estimating AMCEs
#' amce(taxes, chose_plan ~ taxrate1 + taxrate2 + taxrate3 + 
#'      taxrate4 + taxrate5 + taxrate6 + taxrev, id = ~ ID)
#' 
#' \dontrun{
#' data("immigration")
#' # estimating AMCEs with constraints
#' amce(immigration, ChosenImmigrant ~ Gender + ReasonForApplication * CountryOfOrigin,
#'      id = ~CaseID, constraints = list(~ReasonForApplication + CountryOfOrigin))
#' 
#' # balance testing example
#' plot(amce(immigration[!is.na(immigration$ethnocentrism),
#'      ethnocentrism ~ Gender + Education + LanguageSkills, id = ~ CaseID))
#' 
#' # reference category sensitivity
#' x <- amce_by_reference(immigration, ChosenImmigrant ~ LanguageSkills + Education, 
#'        variable = ~ LanguageSkills, id = ~ CaseID)
#' # plot
#' plot(x, group = "BY")
#' }
#' @seealso \code{\link{amce_diffs}} \code{\link{mm}} \code{\link{plot.cj_amce}}
#' @import stats
#' @importFrom sandwich vcovCL
#' @export
amce <- 
function(
  data,
  formula,
  id = NULL,
  weights = NULL,
  constraints = NULL,
  feature_order = NULL,
  feature_labels = NULL,
  level_order = c("ascending", "descending"),
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
    
    # handle constraints if, present
    if (!is.null(constraints)) {
        # with constraints, we need averaging of multiple AMEs over subsets of data
        # but the key thing is it is averaging over possible feature combinations (unweighted by their actual frequency)
        
        # check that constraints only have two terms
        if (any(lengths(constraints) > 2L)) {
            stop("'constraints' can only have two variables")
        }
        
        # vector of constrained variables
        constrained_vars <- unlist(lapply(constraints, all.vars))
        ## check that constraints do not include duplicated terms
        if (any(duplicated(constrained_vars))) {
            stop("all variables in 'constraints' must be unique")
        }
        constrained_vars <- unique(constrained_vars)
        # vector of unconstrained variables
        unconstrained_vars <- RHS[!RHS %in% constrained_vars]
        
        # first estimate unconstrained terms
        if (length(unconstrained_vars)) {
            # estimate 'unconstrained_vars', by calling `amce()` without constraints
            unconstrained_amces <- amce(data = data,
                                        formula = formula,
                                        id = id,
                                        weights = weights,
                                        constraints = NULL,
                                        feature_order = feature_order,
                                        feature_labels = feature_labels,
                                        level_order = level_order,
                                        alpha = alpha,
                                        ...)
            # subset to unconstrained variables
            browser()
            unconstrained_amces <- unconstrained_amces[unconstrained_amces[["feature"]] %in% unname(unlist(feature_labels[unconstrained_vars])), , drop = FALSE]
        } else {
            # if no 'unconstrained_vars', return empty data frame
            unconstrained_amces <- data.frame(outcome = character(),
                               statistic = character(),
                               feature = character(),
                               level = character(),
                               estimate = numeric(),
                               std.error = numeric(),
                               z = numeric(),
                               p = numeric(),
                               lower = numeric(),
                               upper = numeric(),
                               check.names = FALSE,
                               stringsAsFactors = FALSE)
        }
        
        # then estimate constrained terms over appropriate subsets of data
        
        ## NEED TO CHECK CONSTRAINTS FOR REPETITION OF TERMS
        
        ## for each constrained term, we need to:
        ## > handle the fact that `mod` will have NA coefficient estimates for interaction terms that are not possible
        ## > estimate its marginal effect on the subset of data where the two (or more) features co-occur
        ## > handle that different levels of a given feature may have different constraints
        constrained_amces <- lapply(constraints, function(one) {
            # subset the data for this constraint
            proportions <- props(data = data, formula = one)
            proportions_allowed <- subset(proportions, Proportion != 0)
            proportions_disallowed <- subset(proportions, Proportion == 0)
            
            # identify variables
            variables_in_this_constraint <- setdiff(unlist(lapply(one, all.vars)), "~")
            var1 <- variables_in_this_constraint[1L]
            var2 <- variables_in_this_constraint[2L]
            
            # function to calculate AMCEs by averaging across MM differences
            calculate_amce <- function(to_average) {
                # calculate AMCE etc
                est <- mean(to_average[["estimate"]], na.rm = TRUE)
                se <- sqrt(mean(na.omit(to_average[["std.error"]]^2), na.rm = TRUE))
                to_average[1, "outcome"] <- outcome
                to_average[1, "statistic"] <- "amce"
                to_average[1, "estimate"] <- est
                to_average[1, "std.error"] <- se
                to_average[1, "z"] <- est/se
                to_average[1, "p"] <- 2L*(1L-stats::pnorm(abs(to_average[1, "z"])))
                to_average[1, "lower"] <- est - (stats::qnorm(1-alpha) * se)
                to_average[1, "upper"] <- est + (stats::qnorm(1-alpha) * se)
                # cleanup return
                if (var1 %in% names(to_average)) {
                    to_average[1, "feature"] <- feature_labels[[var1]]
                    to_average[, "level"] <- to_average[[var1]]
                    to_average[[var1]] <- NULL
                } else {
                    to_average[1, "feature"] <- feature_labels[[var2]]
                    to_average[, "level"] <- to_average[[var2]]
                    to_average[[var2]] <- NULL
                }
                return(to_average[1, , drop = FALSE])
            }
            
            # calculate MEs for first variable, constraining second
            one_out1 <- cj(data = data, formula = update(formula, one), id = id, estimate = "mm_diff", alpha = alpha, by = formula(paste("~", var1)))
            ## average over differences to get AMCEs, dropping base levels
            one_out1_subset <- one_out1[one_out1[["feature"]] == feature_labels[[var2]], ]
            one_out1 <- do.call("rbind", lapply(split(one_out1_subset, one_out1_subset[[var1]], drop = TRUE), calculate_amce))
            
            # calculate MEs for second variable, constraining first
            one_out2 <- cj(data = data, formula = update(formula, one), id = id, estimate = "mm_diff", alpha = alpha, by = formula(paste("~", var2)))
            ## average over differences to get AMCEs, dropping base levels
            one_out2_subset <- one_out2[one_out2[["feature"]] == feature_labels[[var1]], ]
            one_out2 <- do.call("rbind", lapply(split(one_out2_subset, one_out2_subset[[var2]], drop = TRUE), calculate_amce))
            
            # combine both sets of AMCEs
            one_out <- rbind(one_out1, one_out2)
            one_out[["BY"]] <- NULL
            
            # restore base levels of both variables
            one_out <- rbind(one_out,
                             data.frame(statistic = rep("amce", 2L),
                                        outcome = rep(outcome, 2L),
                                        feature = unlist(unname(feature_labels[c(var1, var2)])),
                                        level = c(levels(data[[var1]])[1L], levels(data[[var2]])[1L]),
                                        estimate = NA_real_,
                                        std.error = NA_real_,
                                        z = NA_real_,
                                        p = NA_real_,
                                        lower = NA_real_,
                                        upper = NA_real_))
            rownames(one_out) <- seq_len(nrow(one_out))
            return(one_out)
        })
        
        # update out with constrained AMCEs
        out <- rbind(unconstrained_amces, do.call("rbind", constrained_amces))
    } else {
        ## without constraints AMCEs are simple marginal effects
        ## this section is also called recursively when `!is.null(constraints)`
        if (is.null(svydesign)) {
            out <- summary(margins::margins(mod, data = data, vcov = vc), level = 1-alpha)
        } else {
            out <- summary(margins::margins(mod, data = data, design = svydesign, vcov = vc), level = 1-alpha)
        }
        # cleanup output from summary(margins())
        names(out) <- c("factor", "estimate", "std.error", "z", "p", "lower", "upper")
        
        # cleanup term names
        out <- cbind(clean_term_names(out[["factor"]], RHS), out[-1L])
        
        # fill in missing base levels with 0s
        out <- merge(out[!out$feature %in% c("", "(Intercept)"),], 
                          term_labels_df, 
                          by = c("feature", "level"), all = TRUE)
        out[["estimate"]][is.na(out[["estimate"]])] <- 0
        
        # label features and levels
        out[["feature"]] <- factor(out[["feature"]],
                                   levels = feature_order,
                                   labels = feature_labels[feature_order])
        out[["level"]] <- factor(out[["level"]], levels = term_labels_df[["level"]])
        out[["outcome"]] <- outcome
        out[["statistic"]] <- "amce"
    }
    
    # return
    out <- out[c("outcome", "statistic", "feature", "level", "estimate", "std.error", "z", "p", "lower", "upper")]
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
