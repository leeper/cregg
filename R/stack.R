#' @title Reshape and tidy a conjoint dataset
#' @description Survey software tends to 
#' @param data A data frame containing variables specified in \code{formula}. All RHS variables should be factors; the base level for each will be used in estimation and its reported AMCE will be zero (for printing).
#' @param outcome An LHS formula specifying an outcome variable name.
#' @param variables A named list
#' @param id An RHS formula specifying a variable holding respondent identifiers, to be used for clustering standard errors.
#' @param \dots Ignored
#' @return A data frame
#' @examples
#' data(hainmueller)
#' set.seed(12345)
#' cj(hainmueller, ChosenImmigrant ~ Gender + Education + LanguageSkills, 
#'    id = ~ CaseID, feature_order = c("LanguageSkills", "Gender", "Education"))
#'  
#' @seealso \code{\link{cj}}
#' @import stats
#' @export
half_stack <- 
function(data,
         variables,
         id = NULL,
         ...
) {

    # get ID variable
    id_var <- all.vars(stats::update(id, 0 ~ . ))[1L]
    
    # check `variables` list
    len <- diff(range(lengths(variables)))
    if (len != 0) {
        warning("Lengths of 'variables' are not all equal. Results may be weird.")
    }
    
    if (any(names(variables) == "")) {
        stop("All elements of 'variables' list must be named.")
    }
    
    long <- stats::reshape(
        data,
        varying = variables,
        v.names = names(variables),
        timevar = "pair",
        idvar = id_var,
        direction = "long"
    )
    len <- length(variables[[1L]])
    if (nrow(long) != (nrow(data) * len)) {
        message(sprintf("Number of rows in result (%d) is not the expected multiple of input data", nrow(long), nrow(data)))
    }
    return(long)
}

full_stack <- 
function(data,
         variables,
         id = NULL,
         ...
) {

    # get ID variable
    id_var <- all.vars(stats::update(id, 0 ~ . ))[1L]
    
    len <- diff(range(unlist(lapply(variables, function(x) lengths(x)))))
    if (len != 0) {
        warning("Lengths of 'variables' are not all equal. Results may be weird.")
    }
    
    # half stack
    variables_half <- lapply(variables, unlist)
    browser()
    half <- half_stack(
        data = data,
        outcome = outcome,
        variables = variables_half,
        id = id
    )
    
    # convert half stack to full stack
    long <- stats::reshape(
        half,
        varying = lapply(variables, names),
        v.names = names(variables),
        timevar = "AB",
        idvar = "id_pair",
        direction = "long"
    )
    len <- length(variables[[1L]])
    if (nrow(long) != (nrow(data) * len)) {
        message(sprintf("Number of rows in result (%d) is not the expected multiple of input data", nrow(long), nrow(data)))
    }
    return(long)
}
