#' @rdname cj_freqs
#' @export
cj_table <-
function(
  data,
  formula,
  feature_order = NULL,
  feature_labels = NULL,
  level_order = c("ascending", "descending"),
  include_reference = FALSE,
  ...
) {
        
    # get RHS variables, variable labels, and factor levels
    RHS <- all.vars(stats::update(formula, 0 ~ . ))
    
    # process feature_order argument
    feature_order <- check_feature_order(feature_order, RHS)
    
    # set level_order (within features) to ascending or descending
    level_order <- match.arg(level_order)
    
    # function to produce "fancy" feature labels
    feature_labels <- clean_feature_labels(data = data, RHS = RHS, feature_labels = feature_labels)
    
    # convert feature labels and levels to data frame
    out <- make_term_labels_df(data, feature_names = feature_order, level_order = level_order)
    out[["level"]] <- factor(out[["level"]], levels = out[["level"]])
    out[["feature"]] <- factor(out[["feature"]],
                               levels = feature_order,
                               labels = feature_labels[feature_order])
    rownames(out) <- seq_len(nrow(out))
    
    if (isTRUE(include_reference)) {
        out[["reference"]] <- FALSE
        # identify reference categories
        reference_categories <- unlist(lapply(data[RHS], function(x) levels(x)[1L]))
        out[["reference"]][out[["level"]] %in% reference_categories] <- TRUE
    }
    
    return(out)
}
