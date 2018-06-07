# function to modify output of `get_coef_metadata()` to something that is constraint-specific
## this is used in `amce()` on the subset of `terms_df` that contains 'base_var' and 'by_var' terms
identify_term_levels <- function(terms_df, data, base_var, by_var) {
    
    if (any(terms_df[["_order"]] > 2L)) {
        stop("Function behavior with higher-order interaction terms is undefined.")
    }
    
    # add factor levels for 'by_var' to 'terms_df'
    terms_df[["_by_level"]] <- NA_character_
    terms_df[["_base_level"]] <- NA_character_
        
    # get contrasts
    base_var_first_level <- levels(factor(data[[base_var]]))[1L]
    by_var_first_level <- levels(factor(data[[by_var]]))[1L]
    
    # get term levels from term labels and coefficient names
    term_levels <- get_term_level_from_coef_name(terms_df[["_coef"]], terms_df[["_term"]])
    
    # loop over rows, identify level of 'base_var' and 'by_var'
    for (i in seq_len(nrow(terms_df))) {
        # if first-order term, don't apply function instead figure out base and by level manually
        if (terms_df[["_order"]][i] == 1L) {
            if (terms_df[["_term"]][i] == by_var) {
                # variable is first-order term for by variable
                terms_df[["_base_level"]][i] <- base_var_first_level
                terms_df[["_by_level"]][i] <- regmatches(terms_df[["_coef"]][i], 
                                                          regexpr(paste0("(?<=", terms_df[["_term"]][i], ").+"),
                                                                  terms_df[["_coef"]][i],
                                                                  perl = TRUE))
            } else {
                # variable is first-order term for base variable
                terms_df[["_by_level"]][i] <- by_var_first_level
                terms_df[["_base_level"]][i] <- regmatches(terms_df[["_coef"]][i], 
                                                          regexpr(paste0("(?<=", terms_df[["_term"]][i], ").+"),
                                                                  terms_df[["_coef"]][i],
                                                                  perl = TRUE))
            }
        } else {
            # figure out base_var and by_var levels form `term_levels` list
            terms_df[["_base_level"]][i] <- unname(term_levels[[i]][names(term_levels[[i]]) == base_var])
            terms_df[["_by_level"]][i] <- unname(term_levels[[i]][names(term_levels[[i]]) == by_var])
        }
    }
    
    # return terms_df
    return(terms_df)
}
