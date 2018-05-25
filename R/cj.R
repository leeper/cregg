#' @rdname cregg
#' @export
cj <-
function(
  data,
  formula,
  id = NULL,
  weights = NULL,
  estimate = c("amce", "freqs", "mm", "amce_differences", "mm_differences"),
  by = NULL,
  ...
) {
    estimate <- match.arg(estimate)
    
    if (!is.null(by)) {
        # get RHS variables, variable labels, and factor levels
        by_vars <- all.vars(stats::update(by, 0 ~ . ))
        
        # amce_diffs handles `by` internally
        if (estimate == "mm_differences") {
            return(mm_diffs(data = data, formula = formula, by = by, id = id, weights = weights, ...))
        } else if (estimate == "amce_differences") {
            return(amce_diffs(data = data, formula = formula, by = by, id = id, weights = weights, ...))
        }
        
        # split
        split_df <- split(data, model.frame(by, data = data, na.action = NULL), sep = "***")
        # get results for subsets
        BY <- list()
        for(i in seq_along(split_df)) {
            BY[[i]] <- switch(estimate, amce = amce, freqs = freqs, mm = mm)(data = split_df[[i]], formula = formula, id = id, weights = weights, ...)
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
        out$BY <- factor(names(split_df)[out$BY])
    } else {
        if (estimate %in% c("mm_differences", "amce_differences")) {
            stop("Argument 'by' is required when estimate %in% c('mm_differences', 'amce_differences')")
        }
        by_vars <- NULL
        out <- switch(estimate,
                 amce = amce(data = data, formula = formula, id = id, weights = weights, ...),
                 freqs = freqs(data = data, formula = formula, id = id, weights = weights, ...),
                 mm = mm(data = data, formula = formula, id = id, weights = weights, ...)
               )
    }
    # return value
    return(structure(out, by = by_vars))
}
