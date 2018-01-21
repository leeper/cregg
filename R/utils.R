# function used in cj and ammplot to produce "fancy" feature labels
clean_feature_labels <- function(data, RHS, feature_labels) {
    if (inherits(data, "data.frame")) {
        fancy_labels <- stats::setNames(lapply(data[RHS], attr, "label"), RHS)
    } else if (inherits(data, "survey.design")) {
        fancy_labels <- stats::setNames(lapply(data[["variables"]][RHS], attr, "label"), RHS)
    } else {
        stop("'data' is not a 'data.frame' or 'survey.design' object")
    }
    ## check `feature_labels`, if present
    missing_labels <- RHS[!RHS %in% names(feature_labels)]
    feature_labels <- c(feature_labels, stats::setNames(rep(list(NULL), length(missing_labels)), missing_labels))
    ## clean fancy labels (use variable name if variable has no "label" attribute)
    for (i in seq_along(feature_labels)) {
        if (is.null(feature_labels[[i]]) || feature_labels[[i]] == "") {
            if (is.null(fancy_labels[[RHS[i]]])) {
                feature_labels[[i]] <- RHS[i]
            } else {
                feature_labels[[i]] <- fancy_labels[[RHS[i]]]
            }
        }
    }
    return(feature_labels)
}

# function to cleanup term labels (due to factor variable blah blah)
clean_term_names <- function(x, RHS) {
    out <- data.frame(feature = character(length(x)), 
                      level = character(length(x)), 
                      stringsAsFactors = FALSE)
    for (i in seq_along(RHS)) {
        w <- grepl(RHS[i], x)
        out$feature[w] <- RHS[i]
        out$level[w] <- sub(paste0("^`?", RHS[i], "`?"), "", x[w])
    }
    
    # check whether levels are all unique
    if (any(duplicated(out$level))) {
        warning("Some level labels are duplicated across features. This may cause problems!")
    }
    
    return(out)
}

# function used in cj to produce a data frame of features and levels
make_term_labels_df <- function(data, RHS) {
    if (inherits(data, "data.frame")) {
        term_levels_list <- lapply(data[RHS], levels)
    } else if (inherits(data, "survey.design")) {
        term_levels_list <- lapply(data[["variables"]][RHS], levels)
    } else {
        stop("'data' is not a 'data.frame' or 'survey.design' object")
    }
    term_levels <- rev(unlist(term_levels_list))
    term_labels <- stats::setNames(rep(RHS, lengths(term_levels_list)), rev(term_levels))
    data.frame(feature = unlist(term_labels), level = unlist(names(term_labels)), stringsAsFactors = FALSE)
}
