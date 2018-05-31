# function to get model terms converted into data frame where rows are coefficients and columns explain those coefficients
## used in `amce()` and `amce_diffs()`
get_coef_metadata <- function(mod, data = model.frame(mod)) {
    # extract coefficient names
    coefs <- coef(mod)
    
    # extract terms
    model_terms <- terms(mod)
    
    # extract `dataClasses` attribute
    data_classes <- attr(model_terms, "dataClasses")
    
    # is there an intercept?
    intercept <- if (attr(model_terms, "intercept") == 1L) TRUE else FALSE
    
    # extract 'assign' attribute vector and attach names from formula terms
    assign_vec <- attr(model.matrix(mod), "assign")
    if (intercept) {
        # drop intercept from 'assign' vector temporarily
        assign_vec <- assign_vec[-1L]
        # drop intercept from 'coefs'
        coefs <- coefs[-1L]
        # drop intercept from 'data_classes'
        data_classes <- data_classes[-1L]
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
      "_coef" = names(coefs),
      # add coefficient estimate values
      #"_estimate" = coefs,
      # identify 'term' names
      "_term" = colnames(model_factors)[assign_vec],
      # identify interaction terms
      "_order" = attr(model_terms, "order")[assign_vec]
    )
    # identify data class, based on term
    terms_df[["_class"]] <- get_term_class_from_term_name(terms_df[["_term"]], data_classes)
    # cbind() the logical indicating which column has which variables in it
    terms_df <- cbind(terms_df, model_factors_df)
    # cleanup rownames
    rownames(terms_df) <- seq_len(nrow(terms_df))
    
    # attach term level names to 'terms_df'
    ## population new columns
    terms_df[paste0("_level_", names(model_factors_df))] <- NA_character_
    ## get all term levels
    term_levels <- get_term_level_from_coef_name(coefficient = terms_df[["_coef"]], term = terms_df[["_term"]])
    ## populate with values
    for (i in seq_along(term_levels)) {
        terms_df[i, paste0("_level_", names(term_levels[[i]]))] <- term_levels[[i]]
    }
    ## fill in base levels, which are NA at this point
    for (i in seq_along(model_factors_df)) {
        if (data_classes[names(model_factors_df)[i]] == "numeric") {
            terms_df[is.na(terms_df[[paste0("_level_", names(model_factors_df)[i])]]), paste0("_level_", names(model_factors_df)[i])] <- "0"
        } else {
            terms_df[is.na(terms_df[[paste0("_level_", names(model_factors_df)[i])]]), paste0("_level_", names(model_factors_df)[i])] <- levels(data[[names(model_factors_df)[i]]])[1L]
        }
    }
    
    # return
    return(terms_df)
}

# function to figure out term levels of terms_df "_coef" column
## used in `get_coef_metadata()` (below; which is used within `amce_diffs()`)
get_term_level_from_coef_name <- function(coefficient, term) {
    # ensure we're working with character vectors
    coefficient <- as.character(coefficient)
    term <- as.character(term)
    
    # setup output list
    out <- rep(list(c()), length(coefficient))
    
    # loop over term names
    for (i in seq_along(out)) {
        # split term by `:`
        term_list <- strsplit(term[i], ":")[[1L]]
        # use those term names to identify levels
        if (length(term_list) == 1L) {
            # first order term
            this_match <- regexpr(paste0("(?<=", term_list[1L], ").+"), coefficient[[i]], perl = TRUE)
            if (this_match == -1L) {
                # if no match, use "1" (because it's not a factor variable)
                out[[i]] <- "1"
            } else {
                out[[i]] <- regmatches(coefficient[[i]], this_match)
            }
        } else {
            # second and higher order term
            for (j in seq_len(length(term_list))) {
                # regular expression using positive lookbehind and positive lookahead on term names
                if (j < length(term_list)) {
                    # for all but the last term, grab the text between the term name and the following term name, preceded by : (`:NEXT_TERM_NAME`)
                    this_match <- regexpr(paste0("(?<=", term_list[j], ").+(?=:", term_list[j+1L], ")"), coefficient[[i]], perl = TRUE)
                } else {
                    # for last term, grab all the text after the term name
                    this_match <- regexpr(paste0("(?<=:", term_list[j], ").+"), coefficient[[i]], perl = TRUE)
                }
                # add to existing list
                if (this_match == -1L) {
                    # if no match, use "1" (because it's not a factor variable)
                    out[[i]] <- c(out[[i]], "1")
                } else {
                    out[[i]] <- c(out[[i]], regmatches(coefficient[[i]], this_match))
                }
            }
        }
        names(out[[i]]) <- term_list
    }
    
    # attach names of terms to list and return
    names(out) <- term
    return(out)
}

# function to convert a vector of term names `a`, `a:b`, etc. into a vector of colon-separated data classes
get_term_class_from_term_name <- function(term, data_classes) {
    class_out <- as.character(term)
    for (i in seq_along(term)) {
        for (j in seq_along(data_classes)) {
            class_out[i] <- sub(names(data_classes)[j], data_classes[j], class_out[i])
        }
    }
    class_out
}
