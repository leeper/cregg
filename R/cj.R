#' @rdname cregg
#' @export
cj <-
function(
  data,
  formula,
  id = ~ 0,
  weights = NULL,
  estimate = c("amce", "frequencies", "mm", "amce_differences", "mm_differences"),
  feature_order = NULL,
  feature_labels = NULL,
  level_order = c("ascending", "descending"),
  by = NULL,
  ...
) {
    estimate <- match.arg(estimate)
    
    # coerce to "cj_df" to preserve attributes
    data <- cj_df(data)
    
    if (!is.null(by)) {
        # get RHS variables, variable labels, and factor levels
        RHS <- all.vars(stats::update(formula, 0 ~ . ))
        
        # get RHS variables, variable labels, and factor levels
        by_vars <- all.vars(stats::update(by, 0 ~ . ))
        
        # process feature_order argument
        feature_order <- check_feature_order(feature_order, RHS)
        
        # set level_order (within features) to ascending or descending
        level_order <- match.arg(level_order)
        
        # function to produce "fancy" feature labels
        ## we need to handle this here, otherwise `split()` will drop feature labels from variable attributes
        feature_labels <- clean_feature_labels(data = data, RHS = RHS, feature_labels = feature_labels)
        
        # convert feature labels and levels to data frame
        term_labels_df <- make_term_labels_df(data, feature_order, level_order = level_order)
    
        # amce_diffs handles `by` internally
        if (estimate == "mm_differences") {
            return(mm_diffs(data = data, formula = formula, by = by, id = id, weights = weights,
                            feature_order = feature_order, feature_labels = feature_labels, level_order = level_order, ...))
        } else if (estimate == "amce_differences") {
            return(amce_diffs(data = data, formula = formula, by = by, id = id, weights = weights,
                              feature_order = feature_order, feature_labels = feature_labels, level_order = level_order, ...))
        }
        
        # split
        split_df <- split(data, model.frame(by, data = data, na.action = NULL), sep = "***")
        # get results for subsets
        BY <- list()
        for(i in seq_along(split_df)) {
            # execute function on subset of data
            BY[[i]] <- switch(estimate, amce = amce, freq = cj_freqs, mm = mm)(
                         data = split_df[[i]],
                         formula = formula,
                         id = id,
                         weights = weights,
                         feature_order = feature_order,
                         feature_labels = feature_labels,
                         level_order = level_order,
                         ...)
            BY[[i]][["BY"]] <- i
        }
        ## get names of subsets
        by_vals <- stats::setNames(do.call("rbind.data.frame", strsplit(names(split_df), "***", fixed = TRUE)), by_vars)
        by_vals[["BY"]] <- factor(seq_len(nrow(by_vals)))
        # combine back into data frame
        out <- do.call("rbind", BY)
        out <- structure(
          merge(out, by_vals, by = "BY"),
          class = c(paste0("cj_", estimate), "data.frame"),
          BY = TRUE
        )
        out[["BY"]] <- factor(names(split_df)[out$BY])
        out[["statistic"]] <- estimate
        
        # label features and levels
        out <- out[c("BY", "outcome", "statistic", "feature", "level", "estimate", "std.error", "z", "p", "lower", "upper", by_vars)]
        rownames(out) <- seq_len(nrow(out))
        
    } else {
        if (estimate %in% c("mm_differences", "amce_differences")) {
            stop("Argument 'by' is required when estimate %in% c('mm_differences', 'amce_differences')")
        }
        by_vars <- NULL
        out <- switch(estimate,
                 amce = amce(data = data, formula = formula, id = id, weights = weights,
                             feature_order = feature_order, feature_labels = feature_labels, level_order = level_order, ...),
                 freqs = cj_freqs(data = data, formula = formula, id = id, weights = weights,
                               feature_order = feature_order, feature_labels = feature_labels, level_order = level_order, ...),
                 mm = mm(data = data, formula = formula, id = id, weights = weights,
                         feature_order = feature_order, feature_labels = feature_labels, level_order = level_order, ...)
               )
    }
    # return value
    return(structure(out, by = by_vars))
}
