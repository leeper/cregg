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

# function used to check whether, if specified, the 'feature_order' argument is valid
check_feature_order <- function(feature_order, RHS) {
    if (!is.null(feature_order)) {
        if (length(RHS) > length(feature_order)) {
            stop("'feature_order' appears to be missing values")
        } else if (length(RHS) < length(feature_order)) {
            stop("'feature_order' appears to have excess values")
        } else if (any(!names(feature_order) %in% RHS)) {
            stop("'feature_order' appears to contain erroneous values")
        }
    } else {
        feature_order <- RHS
    }
    return(feature_order)
}

# function to figure out term levels of terms_df "_name" column
## used in `get_terms_df()` (below; which is used within `amce_diffs()`)
split_coef_name_by_term <- function(coefficient, term_label) {
    # this strictly only works with two-way interactions
    term_vec <- strsplit(term_label, ":")[[1L]]
    if (length(term_vec) != 2L) {
        return(rep(NA_character_, 2))
    }
    
    ## extract level of term 1
    # regular expression using positive lookbehind and positive lookahead on term names
    m1 <- regexpr(paste0("(?<=", term_vec[1L], ").+(?=:", term_vec[2L], ")"), coefficient, perl = TRUE)
    term1_level <- regmatches(coefficient, m1)
    
    ## extract level of term 2
    m2 <- regexpr(paste0("(?<=:", term_vec[2L], ").+"), coefficient, perl = TRUE)
    term2_level <- regmatches(coefficient, m2)
    
    out <- c(term1_level, term2_level)
    names(out) <- c(term_vec)
    out
}

# function to get model terms converted into data frame
## used in `amce_diffs()`
get_terms_df <- function(mod, data, by_var) {
    # extract coefficient names
    coefs <- coef(mod)
    
    # extract terms
    model_terms <- terms(mod)
    if (any(attr(model_terms, "order")) > 2L) {
        stop("Function behavior with higher-order interaction terms is undefined.")
    }
    
    # is there an intercept?
    intercept <- if (attr(model_terms, "intercept") == 1L) TRUE else FALSE
    
    # extract 'assign' attribute vector and attach names from formula terms
    assign_vec <- attr(model.matrix(mod), "assign")
    if (intercept) {
        # drop intercept from 'assign' vector temporarily
        assign_vec <- assign_vec[-1L]
        # drop intercept from 'coefs'
        coefs <- coefs[-1L]
    }
    names(assign_vec) <- attr(model_terms, "term.labels")[assign_vec]
    
    # extract 'factor' attribute, which is a variable-by-term matrix
    model_factors <- attr(model_terms, "factors")
    
    ## identify terms for each coefficient (using 'assign_vec' to extract)
    ## any column (a coef) with more than one non-zero entry mean involves two or more variables
    ## Note: it has to be non-zero because the matrix can contain 1s and 2s, which have different substantive meanings
    model_factors_df <- data.frame(t(model_factors[-1L, assign_vec, drop = FALSE] != 0), check.names = FALSE)
    
    # build a data frame of the coefficients and information from terms() and 'assign' attribute
    terms_df <- cbind.data.frame(
      # add coefficient names
      "_name" = names(coefs),
      # add coefficient estimate values
      #"_estimate" = coefs,
      # identify 'term' names
      "_term" = colnames(model_factors)[assign_vec],
      # identify interaction terms
      "_order" = attr(model_terms, "order")[assign_vec],
      # identify coefficients involving 'by_var'
      "_by" = model_factors_df[[by_var]]
    )
    # cleanup rownames
    rownames(terms_df) <- seq_len(nrow(terms_df))
    # convenience step to identify interactions
    terms_df[["_interaction"]] <- terms_df[["_order"]] > 1L
    
    # determine the variable that 'by_var' is interacted with
    terms_df[["_base_var"]] <- NA_character_
    base_term_list <- lapply(model_factors_df[setdiff(names(model_factors_df), by_var)], which)
    for (i in seq_along(base_term_list)) {
        terms_df[["_base_var"]][base_term_list[[i]]] <- names(base_term_list)[i]
    }
    
    # add factor levels for 'by_var' to 'terms_df'
    terms_df[["_by_level"]] <- NA_character_
    terms_df[["_base_level"]] <- NA_character_
        
    # get contrasts
    con <- contrasts(data[[by_var]])
    
    # apply function to data
    for (i in seq_len(nrow(terms_df))) {
        # if first-order term, don't apply function instead figure out base and by level manually
        if (terms_df[["_order"]][i] == 1L) {
            if (terms_df[["_term"]][i] == by_var) {
                # do nothing, because we'll just delete these rows later
                terms_df[["_by_level"]][i] <- rownames(con)[1L]
            } else {
                # variable is first-order term for base variable
                terms_df[["_by_level"]][i] <- rownames(con)[1L]
                terms_df[["_base_level"]][i] <- regmatches(terms_df[["_name"]][i], 
                                                          regexpr(paste0("(?<=", terms_df[["_term"]][i], ").+"),
                                                                  terms_df[["_name"]][i],
                                                                  perl = TRUE))
            }
        } else {
            # use utility function to split coefficient names
            tmp <- split_coef_name_by_term(as.character(terms_df[["_name"]][i]), as.character(terms_df[["_term"]][i]))
            terms_df[["_base_level"]][i] <- tmp[names(tmp) != by_var]
            terms_df[["_by_level"]][i] <- paste0(tmp[names(tmp) != by_var], " - ", rownames(con)[1L])
        }
    }
    
    # return terms_df
    return(terms_df)
}


# function to convert model estimates (possibly corrected for clustering) into a data frame
## used in `amce_diffs()`
get_coef_summary <- function(mod, data, id = NULL, alpha = 0.05) {
    
    # is there an intercept?
    intercept <- if (attr(terms(mod), "intercept") == 1L) TRUE else FALSE
    
    # setup standard errors and create `coef_summary`
    if (is.null(id) || !length(all.vars(id))) {
        # get `coef_summary` matrix
        coef_summary <- unclass(lmtest::coeftest(mod))
        
        # calculate confidence intervals
        confints <- confint(mod, level = 1-alpha)
        colnames(confints) <- c("lower", "upper")
        coef_summary <- cbind(coef_summary, confints)
        
    } else {
        # get clustered var-cov matrix
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
    coef_summary[["_name"]] <- rownames(coef_summary)
    rownames(coef_summary) <- seq_len(nrow(coef_summary))
    
    return(coef_summary)
}
