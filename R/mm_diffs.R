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
    
    # get RHS variables, variable labels, and factor levels
    RHS <- all.vars(stats::update(formula, 0 ~ . ))
    by <- stats::update(by, ~ . )
    # sanity check that 'by' is only an single variable
    stopifnot(length(by) == 2L)
    by_var <- as.character(by)[2L]
    # ensure that by only takes two values
    stopifnot(length(na.omit(unique(data[[by_var]]))) == 2L)
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
    mm_split <- split(mm, mm[["BY"]])
    out <- mm_split[[1L]]
    ## difference
    out[["estimate"]] <- mm_split[[2L]][["estimate"]] - mm_split[[1L]][["estimate"]]
    ## SE of difference
    n_1 <- sum(!is.na(data[data[[by_var]] == mm_split[[1L]][[by_var]][1L], outcome, drop = TRUE]))
    n_2 <- sum(!is.na(data[data[[by_var]] == mm_split[[2L]][[by_var]][1L], outcome, drop = TRUE]))
    variance <- ((mm_split[[2L]][["std.error"]]^2)) + ((mm_split[[1L]][["std.error"]]^2))
    out[["std.error"]] <- sqrt( variance )
    ## t
    names(out)[names(out) == "z"] <- "t"
    out[["t"]] <- out[["estimate"]]/out[["std.error"]]
    ## p-value
    den_1 <- (mm_split[[1L]][["std.error"]]^4)/((n_2^2) * (n_2 -1L))
    den_2 <- (mm_split[[2L]][["std.error"]]^4)/((n_1^2) * (n_1 -1L))
    degrees_of_freedom <- (variance^2) / (den_1 + den_2)
    out[["p"]] <- 2*stats::pt(-out[["t"]], degrees_of_freedom)
    
    out[["BY"]] <- "Difference"
    out[[by_var]] <- paste0(mm_split[[2L]][[by_var]][1L], " - ", mm_split[[1L]][[by_var]][1L])
    out[["lower"]] <- out[["estimate"]] - stats::qt((1-alpha) + (alpha/2), degrees_of_freedom) * out[["std.error"]]
    out[["upper"]] <- out[["estimate"]] + stats::qt((1-alpha) + (alpha/2), degrees_of_freedom) * out[["std.error"]]
    class(out) <- c("cj_diffs", "data.frame")
    return(out)
}
