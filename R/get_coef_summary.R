# function to convert model estimates (possibly corrected for clustering) into a data frame
## a survey design object from `amce()` or `amce_diffs()` will already be clustered so this just formats the output
## if somehow a model is passed in that's not clustered, then `id` can be used to do the clustering
## used in `amce_diffs()` and can be merged against output of `get_coef_metadata()` by `"_name"`
get_coef_summary <- function(mod, data, id = NULL, alpha = 0.05) {
    
    # is there an intercept?
    intercept <- if (attr(terms(mod), "intercept") == 1L) TRUE else FALSE
    
    # setup standard errors and create `coef_summary`
    if (inherits(mod, "svyglm")) {
        # get `coef_summary` matrix
        coef_summary <- unclass(lmtest::coeftest(mod))
        
        # calculate confidence intervals
        confints <- confint(mod, level = 1-alpha)
        colnames(confints) <- c("lower", "upper")
        coef_summary <- cbind(coef_summary, confints)
        
    } else {
        # get clustered var-cov matrix if mod is not already clustered (i.e., a svyglm)
        if (inherits(data, "data.frame")) {
            cluster_vector <- stats::get_all_vars(id, data)[[1L]]
        } else if (inherits(data, "survey.design")) {
            cluster_vector <- stats::get_all_vars(id, data[["variables"]])[[1L]]
        }
        vc <- sandwich::vcovCL(mod, cluster_vector)
        
        # get `coef_summary` matrix
        coef_summary <- unclass(lmtest::coeftest(mod, vc))
        
        # calculate confidence intervals
        coef_summary <- cbind(coef_summary,
                              "lower" = coef_summary[,"Estimate"] - stats::qnorm((1-alpha) + (alpha/2)) * coef_summary[, "Std. Error"],
                              "upper" = coef_summary[,"Estimate"] + stats::qnorm((1-alpha) + (alpha/2)) * coef_summary[, "Std. Error"])
    }
    
    # setup full coef summary (only includes subset of coefficients that are estimable)
    estimate_summary <- summary(mod)
    
    # populate 'coef_summary' with non-estimable coefficients ("aliased")
    if (any(aliased <- estimate_summary$aliased)) {
        cn <- names(aliased)
        coefs_tmp <- matrix(NA, length(aliased), 6, dimnames = list(cn, colnames(coef_summary)))
        coefs_tmp[!aliased, ] <- coef_summary
        coef_summary <- coefs_tmp
        rm(coefs_tmp)
    }
    # drop intercept if present
    if (intercept) {
        coef_summary <- coef_summary[-1L, , drop = FALSE]
    }
    coef_summary <- as.data.frame(coef_summary)
    coef_summary[["_coef"]] <- rownames(coef_summary)
    rownames(coef_summary) <- seq_len(nrow(coef_summary))
    
    return(coef_summary)
}
