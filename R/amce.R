#' @rdname amce
#' @title Tidy estimation of AMCEs
#' @description Estimate AMCEs for a conjoint analysis and return a tidy data frame of results
#' @param data A data frame containing variables specified in \code{formula}. All RHS variables should be factors; the base level for each will be used in estimation and its reported AMCE will be NA (for printing). Optionally, this can instead be an object of class \dQuote{survey.design} returned by \code{\link[survey]{svydesign}}.
#' @param formula A formula specifying an AMCE model to be estimated. All variables should be factors. Two-way interactions can be specified to handle constraints between factors in the design. These are detected automatically. Higher-order constraints are not allowed.
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
    
    # detect constraints from 'formula'
    formula_terms <- terms(formula)
    if (any(attr(formula_terms, "order") >= 2)) {
        # constraints are present
        constrained_terms <- attr(formula_terms, "factors")[ , attr(formula_terms, "order") == 2, drop = FALSE]
        constrained_vars <- colnames(constrained_terms)
        constraints <- lapply(constrained_vars, function(tmp) {
            as.formula(paste0("~", tmp))
        })
        # vector of constrained variables
        constrained_vars <- unlist(lapply(constraints, all.vars))
        ## check that constraints do not include duplicated terms
        if (any(duplicated(constrained_vars))) {
            stop("All variables in constraints must be unique and constraints may only be two-way")
        }
        constrained_vars <- unique(constrained_vars)
        
        # vector of unconstrained variables
        unconstrained_vars <- RHS[!RHS %in% constrained_vars]
    } else {
        constraints <- NULL
        constrained_vars <- NULL
        unconstrained_vars <- RHS
    }
    
    # process feature_order argument
    feature_order <- check_feature_order(feature_order, RHS)
    
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
    
    # empty AMCEs (used in two places below)
    empty_amces <- data.frame(outcome = character(),
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
    
    # get coefficients as data frame (correct, if needed, for clustering)
    coef_summary <- get_coef_summary(mod = mod, data = data, id = id, alpha = alpha)
    
    # first estimate unconstrained terms
    if (length(unconstrained_vars)) {
        # extract 'unconstrained_vars'
        out <- coef_summary
        names(out) <- c("estimate", "std.error", "z", "p", "lower", "upper", "factor")
        
        # cleanup term names
        out <- cbind(clean_term_names(out[["factor"]], RHS), out[names(out) != "factor"])
        
        # fill in missing base levels with 0s
        out <- merge(out[!out$feature %in% c("", "(Intercept)"),],
                          term_labels_df, 
                          by = c("feature", "level"), all = TRUE)
        out[["estimate"]][is.na(out[["estimate"]])] <- 0
        out[["outcome"]] <- outcome
        out[["statistic"]] <- "amce"
        
        # subset to variables in 'unconstrained_vars'
        unconstrained_amces <- out[out[["feature"]] %in% unconstrained_vars, , drop = FALSE]
        
    } else {
        # if no 'unconstrained_vars', return empty data frame
        unconstrained_amces <- empty_amces
    }
    
    # then calculate constrained terms
    if (!is.null(constraints)) {
        ## for each constraint, we need to:
        ## > figure out what estimated coefficients go with each of the two terms (using `get_coef_metadata()` to identify terms and `get_coef_summary()` to get a clean model summary)
        ## > average effects of first constrained variable across levels of second variable
        ## > average effects of second constrained variable across levels of first variable
        ##     > if we were to generalize this to higher-order constraints, the code would simply have to be made more complex
        ## > `rbind()` the two sets of results back together
        constrained_amces <- lapply(constraints, function(one) {
            # identify variables
            variables_in_this_constraint <- setdiff(unlist(lapply(one, all.vars)), "~")
            var1 <- variables_in_this_constraint[1L]
            var2 <- variables_in_this_constraint[2L]
            
            # function to calculate AMCEs by averaging coefficients
            calculate_amce <- function(to_average, feature) {
                
                # subset to estimable coefficients
                to_average <- to_average[!is.na(to_average[["Estimate"]]), , drop = FALSE]
                # make sure estimates are sorted in correct order
                to_average <- to_average[order(to_average[["_order"]]),]
                
                # var-cov matrix of these estimates
                this_varcov <- vcov(mod)[to_average[["_coef"]], to_average[["_coef"]]]
                
                # calculate AMCE, giving uniform weight to features
                ## calculate weights (base term is weighted 1; other alters equally)
                wts <- c(1L, rep(1L/nrow(to_average), nrow(to_average) - 1L))
                est <- sum(to_average[["Estimate"]] * wts)
                ## variance is weights %*% vcov %*% weights
                variance <- (wts %*% this_varcov %*% wts)[1L,1L,drop = TRUE]
                
                # populate output
                averaged <- data.frame(outcome = outcome,
                                       statistic = "amce",
                                       feature = feature,
                                       level = to_average[to_average[["_order"]] == 1L, "_base_level", drop = TRUE],
                                       estimate = est,
                                       std.error = sqrt(variance),
                                       z = est/sqrt(variance),
                                       p = 2L*(stats::pnorm(-abs(est/sqrt(variance)))),
                                       check.names = FALSE,
                                       stringsAsFactors = FALSE
                                       )
                averaged[["lower"]] <- est - (stats::qnorm(1-alpha) * sqrt(variance))
                averaged[["upper"]] <- est + (stats::qnorm(1-alpha) * sqrt(variance))
                return(averaged)
            }
            
            terms_df <- get_coef_metadata(mod)
            # calculate MEs for first variable, constraining second
            ## subset terms_df to terms with 'var1' and/or 'var2'
            terms_df1 <- terms_df[terms_df[[var1]] | terms_df[[var2]], , drop = FALSE]
            # add term levels to 'term_df1'
            terms_df1 <- identify_term_levels(terms_df1, data, base_var = var1, by_var = var2)
            ## merge terms_df with `coef_summary`
            terms_df1 <- merge(coef_summary, terms_df1, by = "_coef")
            ## split merged object by 'var1' levels, excluding base level, calculating AMCE
            terms_df1 <- terms_df1[terms_df1[["_base_level"]] != levels(data[[var1]])[1L], , drop = FALSE]
            one_out1_list <- lapply(split(terms_df1, terms_df1[["_base_level"]], drop = TRUE), calculate_amce, feature = var1)
            one_out1 <- do.call("rbind", one_out1_list)
            
            # calculate MEs for second variable, constraining first
            ## subset terms_df to terms with 'var1' and/or 'var2'
            terms_df2 <- terms_df[terms_df[[var1]] | terms_df[[var2]], , drop = FALSE]
            # add term levels to 'term_df1'
            terms_df2 <- identify_term_levels(terms_df2, data, base_var = var2, by_var = var1)
            ## merge terms_df with `coef_summary`
            terms_df2 <- merge(coef_summary, terms_df2, by = "_coef")
            ## split merged object by 'var1' levels, excluding base level, calculating AMCE
            terms_df2 <- terms_df2[terms_df2[["_base_level"]] != levels(data[[var2]])[1L], , drop = FALSE]
            one_out2_list <- lapply(split(terms_df2, terms_df2[["_base_level"]], drop = TRUE), calculate_amce, feature = var2)
            one_out2 <- do.call("rbind", one_out2_list)
            
            # NOTE: IF WE HAD HIGHER-ORDER CONSTRAINTS, WE WOULD NEED ADDITIONAL STEPS HERE
            
            # combine both sets of AMCEs for this constraint
            one_out <- rbind(one_out1, one_out2)
            one_out[["BY"]] <- NULL
            
            # restore base levels of both variables
            one_out <- rbind(one_out,
                             data.frame(statistic = rep("amce", 2L),
                                        outcome = rep(outcome, 2L),
                                        feature = c(var1,var2),
                                        level = c(levels(data[[var1]])[1L], levels(data[[var2]])[1L]),
                                        estimate = c(0L, 0L),
                                        std.error = NA_real_,
                                        z = NA_real_,
                                        p = NA_real_,
                                        lower = NA_real_,
                                        upper = NA_real_))
            rownames(one_out) <- seq_len(nrow(one_out))
            return(one_out)
        })
        
        # rbind all constraint pairs together
        constrained_amces <- do.call("rbind", constrained_amces)
        
    } else {
        # if no 'constrained_vars', return empty data frame
        constrained_amces <- empty_amces
    }
    
    # update out with constrained AMCEs
    out <- rbind(unconstrained_amces, constrained_amces)
    
    # label features and levels
    out[["feature"]] <- factor(out[["feature"]],
                               levels = feature_order,
                               labels = feature_labels[feature_order])
    out <- out[c("outcome", "statistic", "feature", "level", "estimate", "std.error", "z", "p", "lower", "upper")]
    out[["level"]] <- factor(out[["level"]], levels = term_labels_df[["level"]])
    out <- out[order(out$level),]
    rownames(out) <- seq_len(nrow(out))
    
    # return
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
