#' @rdname diffs
#' @export
mm_diffs <-
function(
  data,
  formula,
  by,
  id = ~ 0,
  weights = NULL,
  feature_order = NULL,
  feature_labels = NULL,
  level_order = c("ascending", "descending"),
  alpha = 0.05,
  h0 = 0,
  ...
) {
    
    # get outcome variable
    outcome <- all.vars(stats::update(formula, . ~ 0))
    if (!length(outcome) || outcome == ".") {
        stop("'formula' is missing a left-hand outcome variable")
    }
    
    # get RHS variables, variable labels, and factor levels
    RHS <- all.vars(stats::update(formula, 0 ~ . ))
    by <- stats::update(by, ~ . )
    # sanity check that 'by' is only an single variable
    stopifnot(length(by) == 2L)
    by_var <- as.character(by)[2L]
    
    # coerce 'by_var' to factor
    if (!is.factor(data[[by_var]])) {
        data[[by_var]] <- factor(data[[by_var]])
    }
    
    # process feature_order argument
    feature_order <- check_feature_order(feature_order, RHS)
    
    # set level_order (within features) to ascending or descending
    level_order <- match.arg(level_order)
    
    # function to produce "fancy" feature labels
    feature_labels <- clean_feature_labels(data = data, RHS = RHS, feature_labels = feature_labels)
    
    # convert feature labels and levels to data frame
    term_labels_df <- make_term_labels_df(data, feature_order, level_order = level_order)
    
    # estimate marginal means, by 'by_var'
    mm <- cj(data = data, formula = formula, estimate = "mm", id = id, weights = weights, by = by,
             feature_order = feature_order, feature_labels = feature_labels, level_order = level_order, alpha = alpha, h0 = h0, ...)
    
    # split the output of 'mm' and order by factor levels
    mm_split <- split(mm, mm[["BY"]])[levels(data[[by_var]])]
    
    # loop over all levels, differencing against the first one
    for (i in seq_len(length(mm_split))[-1L]) {
        
        # differences for all variables
        mm_split[[i]][["estimate"]] <- mm_split[[i]][["estimate"]] - mm_split[[1L]][["estimate"]]
        # SE of difference
        variance <- ((mm_split[[i]][["std.error"]]^2)) + ((mm_split[[1L]][["std.error"]]^2))
        mm_split[[i]][["std.error"]] <- sqrt( variance )
        
        # z-statistic
        mm_split[[i]][["z"]] <- mm_split[[i]][["estimate"]]/mm_split[[i]][["std.error"]]
        
        # p-value
        mm_split[[i]][["p"]] <- 2L*(1L-stats::pnorm(abs(mm_split[[i]][["z"]])))
        
        # CIs
        mm_split[[i]][["lower"]] <- mm_split[[i]][["estimate"]] - (stats::qnorm(1-alpha) * mm_split[[i]][["std.error"]])
        mm_split[[i]][["upper"]] <- mm_split[[i]][["estimate"]] + (stats::qnorm(1-alpha) * mm_split[[i]][["std.error"]])
        
        # format output
        ## add column indicating value of 'by_var'
        mm_split[[i]][[by_var]] <- mm_split[[i]][[by_var]][1L]
        ## indicate explicit difference as 'BY' column
        mm_split[[i]][["BY"]] <- paste0(mm_split[[i]][[by_var]][1L], " - ", mm_split[[1L]][[by_var]][1L])
    }
    # bind list of differences (except the baseline level)
    out <- do.call("rbind", mm_split[-1L])
    out[["statistic"]] <- "mm_difference"
    rownames(out) <- seq_len(nrow(out))
    class(out) <- c("cj_diffs", "data.frame")
    return(out[c("BY", "statistic", setdiff(names(out), c("BY", "statistic")))])
}
