#' @rdname cregg
#' @name cregg-package
#' @title Simple Conjoint Analyses and Visualization
#' @aliases cregg-package cregg
#' @docType package
#' @description Simple analyses of conjoint (factorial) experiments and visualization of results.
#' @param data A data frame containing variables specified in \code{formula}. All RHS variables should be factors; the base level for each will be used in estimation and for AMCEs the base level's AMCE will be zero.
#' @param formula A formula specifying a model to be estimated. All variables should be factors.
#' @param id An RHS formula specifying a variable holding respondent identifiers, to be used for clustering standard errors.
#' @param weights An (optional) RHS formula specifying a variable holding survey weights.
#' @param estimate A character string specifying an estimate type. Current options are average marginal component effects (or AMCEs, \dQuote{amce}, estimated via \code{\link{amce}}), display frequencies (\dQuote{freq}, estimated via \code{\link{freqs}}), or marginal means (or AMMs, \dQuote{mm}, estimated via \code{\link{mm}}). Additional options may be made available in the future.
#' @param by A formula containing only RHS variables, specifying grouping factors over which to perform estimation.
#' @param \dots Additional arguments to \code{\link{amce}}, \code{\link{freqs}}, or \code{\link{mm}}.
#' @author Thomas J. Leeper <thosjleeper@gmail.com>
#' @details The main function \code{cj} is a convenience function wrapper around the underlying estimation functions that provide for average marginal component effects (AMCEs), by default, via the \code{\link{amce}} function, marginal means (MMs) via the \code{\link{mm}} function, and display frequencies via \code{\link{freqs}}. Additional estimands may be supported in the future through their own functions and through the \code{cj} interface. Plotting is provided via ggplot2 for both types of estimates.
#' 
#' The only additional functionality provided by \code{cj} over the underlying functions is the \code{by} argument, which will perform operations on subsets of \code{data}, returning a single data frame. This can be useful, for example, for evaluating profile spillover effects and subgroup results.
#' 
#' @examples
#' \dontrun{
#' # load data
#' requireNamespace("ggplot2")
#' data("hainmueller")
#' 
#' # calculate MMs
#' d1 <- cj(hainmueller, ChosenImmigrant ~ Gender + Education + 
#'          LanguageSkills + CountryOfOrigin + Job + JobExperience + 
#'          JobPlans + ReasonForApplication + PriorEntry, id = ~ CaseID,
#'          estimate = "mm")
#' 
#' # plot MMs
#' plot(d1)
#'
#' # MMs split by profile number
#' stacked <- do.call("rbind", lapply(1:3, function(x) {
#'     out <- mm(hainmueller[hainmueller$contest_no == x,], ChosenImmigrant ~ Gender + 
#'               Education + LanguageSkills + CountryOfOrigin + Job + JobExperience + 
#'               JobPlans + ReasonForApplication + PriorEntry, id = ~ CaseID)
#'     out$contest <- x
#'     out
#' }))
#' 
#' ## plot with grouping
#' plot(stacked, group = "contest", feature_headers = FALSE)
#' 
#' ## plot with facetting
#' plot(stacked) + ggplot2::facet_wrap(~contest, nrow = 1L)
#' 
#' # estimate AMCEs
#' d2 <- cj(hainmueller, ChosenImmigrant ~ Gender + Education + 
#'          LanguageSkills + CountryOfOrigin + Job + JobExperience + 
#'          JobPlans + ReasonForApplication + PriorEntry, id = ~ CaseID)
#' 
#' # plot AMCEs
#' plot(d2)
#' 
#' # grouped operations
#' ## examine profile ordering
#' x <- cj(hainmueller, ChosenImmigrant ~ Gender + Education + LanguageSkills,
#'         id = ~ CaseID, estimate = "mm", by = ~ contest_no)
#' plot(x, group = "contest_no", vline = 0.5)
#' 
#' ## subgroup analysis
#' hainmueller$ethnosplit <- cut(hainmueller$ethnocentrism, 2)
#' x <- cj(na.omit(hainmueller), ChosenImmigrant ~ Gender + Education + LanguageSkills,
#'         id = ~ CaseID, estimate = "mm", by = ~ ethnosplit)
#' plot(x, group = "ethnosplit", vline = 0.5)
#' }
#' @seealso \code{\link{amce}} \code{\link{mm}} \code{\link{freqs}} \code{\link{plot.cj_amce}}
#' @keywords package 
NULL

#' @rdname cregg
#' @export
cj <-
function(data,
         formula,
         id = NULL,
         weights = NULL,
         estimate = c("amce", "freqs", "mm"),
         by = NULL,
         ...
) {
    estimate <- match.arg(estimate)
    
    if (!is.null(by)) {
        # get RHS variables, variable labels, and factor levels
        by_vars <- all.vars(stats::update(by, 0 ~ . ))
        # split
        split_df <- split(data, model.frame(by, data = data), sep = "***")
        # get results for subsets
        BY <- list()
        for(i in seq_along(split_df)) {
            BY[[i]] <- switch(estimate, amce = amce, freqs = freqs, mm = mm)(data = split_df[[i]], formula = formula, id = id, weights = weights, ...)
            BY[[i]][["BY"]] <- i
        }
        ## get names of subsets
        by_vals <- stats::setNames(do.call("rbind.data.frame", strsplit(names(split_df), "***", fixed = TRUE)), by_vars)
        by_vals[["BY"]] <- seq_len(nrow(by_vals))
        # combine back into data frame
        out <- do.call("rbind", BY)
        out <- structure(
          merge(out, by_vals, by = "BY"),
          class = c(paste0("cj_", estimate), "data.frame"),
          BY = TRUE
        )
    } else {
        out <- switch(estimate,
                 amce = amce(data = data, formula = formula, id = id, weights = weights, ...),
                 freqs = freqs(data = data, formula = formula, id = id, weights = weights, ...),
                 mm = mm(data = data, formula = formula, id = id, weights = weights, ...)
               )
    }
    # return value
    return(structure(out, by = by_vars))
}
