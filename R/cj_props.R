#' @rdname cj_freqs
#' @export
cj_props <- 
function(data,
         formula,
         id,
         weights = NULL,
         margin = NULL,
         ...
) {
    
    # create survey design object
    if (inherits(data, "data.frame") && is.null(weights)) {
        svydesign <- survey::svydesign(ids = ~ 0, weights = ~ 1, data = data)
    } else if (inherits(data, "data.frame")) {
        svydesign <- survey::svydesign(ids = ~ 0, weights = weights, data = data)
    } else if (inherits(data, "survey.design")) {
        svydesign <- data
    } else {
        stop("'data' is not a 'data.frame' or 'survey.design' object")
    }
    
    # calculate display frequencies
    out <- data.frame(prop.table(survey::svytable(formula, design = svydesign), margin = margin))
    
    # return
    names(out)[names(out) == "Freq"] <- "Proportion"
    return(structure(out, class = c("cj_props", "data.frame")))
}
