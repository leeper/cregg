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

# function used to produce a data frame of features and levels
make_term_labels_df <- function(data, feature_names, level_order = c("ascending", "descending")) {
    # setup data
    if (inherits(data, "data.frame")) {
        term_levels_list <- lapply(data[feature_names], levels)
    } else if (inherits(data, "survey.design")) {
        term_levels_list <- lapply(data[["variables"]][feature_names], levels)
    } else {
        stop("'data' is not a 'data.frame' or 'survey.design' object")
    }
    
    # figure out level order
    level_order <- match.arg(level_order)
    if (level_order == "descending") {
        term_levels_list[] <- lapply(term_levels_list, rev)
    }
    
    # construct data frame
    term_levels <- rev(unlist(term_levels_list))
    term_labels <- stats::setNames(rep(feature_names, lengths(term_levels_list)), rev(term_levels))
    data.frame(feature = unlist(term_labels), level = unlist(names(term_labels)), stringsAsFactors = FALSE)
}

# function used in plot() methods to make pretty feature headers
make_feature_headers <- function(x, fmt = "(%s)") {
    feature_levels <- rev(split(x$level, x$feature))
    for (i in seq_along(feature_levels)) {
        feature_levels[[i]] <- levels(x$level)[match(feature_levels[[i]], levels(x$level))]
        feature_levels[[i]] <- c(feature_levels[[i]], sprintf(fmt, names(feature_levels)[i]))
    }
    factor(as.character(x$level), levels = unique(unname(unlist(feature_levels))))
}
