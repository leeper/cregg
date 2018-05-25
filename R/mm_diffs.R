#' @rdname diffs
#' @export
mm_diffs <-
function(
  data,
  formula,
  by,
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
    by <- stats::update(by, ~ . )
    # sanity check that 'by' is only an single variable
    stopifnot(length(by) == 2L)
    by_var <- as.character(by)[2L]
    
    # coerce 'by_var' to factor
    if (!is.factor(data[[by_var]])) {
        data[[by_var]] <- factor(data[[by_var]])
    }
    
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
    
    # function to produce "fancy" feature labels
    feature_labels <- clean_feature_labels(data = data, RHS = RHS, feature_labels = feature_labels)
    
    # convert feature labels and levels to data frame
    term_labels_df <- make_term_labels_df(data, feature_order, level_order = level_order)
    
    mm <- cj(data = data, formula = formula, estimate = "mm", id = id, weights = weights, by = by,
             feature_order = feature_order, feature_labels = feature_labels, level_order = level_order, alpha = alpha, ...)
    # split and order by factor levels
    mm_split <- split(mm, mm[["BY"]])[levels(data[[by_var]])]
    
    # loop over all levels, differencing against the first one
    for (i in seq_len(length(mm_split))[-1L]) {
        # difference
        ## temporarily preserve differences within the 'by' variable
        this_level_diff <- 
            mm_split[[i]][mm_split[[i]][["level"]] == levels(data[[by_var]])[i], "estimate"] - 
            mm_split[[1L]][mm_split[[1L]][["level"]] == levels(data[[by_var]])[1L], "estimate"]
        this_level_se <- 
            sqrt(mm_split[[i]][mm_split[[i]][["level"]] == levels(data[[by_var]])[i], "std.error"]^2 + 
                 mm_split[[1L]][mm_split[[1L]][["level"]] == levels(data[[by_var]])[1L], "std.error"]^2)
        
        ## differences for all variables
        mm_split[[i]][["estimate"]] <- mm_split[[i]][["estimate"]] - mm_split[[1L]][["estimate"]]
        # SE of difference
        variance <- ((mm_split[[i]][["std.error"]]^2)) + ((mm_split[[1L]][["std.error"]]^2))
        mm_split[[i]][["std.error"]] <- sqrt( variance )
        
        ## overwrite differences and SEs thereof for 'by' variable (given they aren't ordered correctly)
        mm_split[[i]][mm_split[[i]][["level"]] == levels(data[[by_var]])[i], "estimate"] <- this_level_diff
        mm_split[[i]][mm_split[[i]][["level"]] == levels(data[[by_var]])[i], "std.error"] <- this_level_se
        
        # z-statistic
        mm_split[[i]][["z"]] <- mm_split[[i]][["estimate"]]/mm_split[[i]][["std.error"]]
        
        # p-value
        mm_split[[i]][["p"]] <- 2L*(1L-stats::pnorm(mm_split[[i]][["z"]]))
        
        # CIs
        mm_split[[i]][["lower"]] <- mm_split[[i]][["estimate"]] - (stats::qnorm(1-alpha) * mm_split[[i]][["std.error"]])
        mm_split[[i]][["upper"]] <- mm_split[[i]][["estimate"]] + (stats::qnorm(1-alpha) * mm_split[[i]][["std.error"]])
        
        # format output
        mm_split[[i]][[by_var]] <- paste0(mm_split[[i]][[by_var]][1L], " - ", mm_split[[1L]][[by_var]][1L])
    }
    # bind list of differences (except the baseline level)
    out <- do.call("rbind", mm_split[-1L])
    out[["BY"]] <- "Difference"
    rownames(out) <- seq_len(nrow(out))
    class(out) <- c("cj_diffs", "data.frame")
    return(out)
}
