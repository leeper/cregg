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
#' @param \dots Additional arguments to \code{\link{amce}}, \code{\link{freqs}}, or \code{\link{mm}}.
#' @author Thomas J. Leeper <thosjleeper@gmail.com>
#' @details The main function \code{cj} is a convenience function wrapper around the underlying estimation functions that provide for average marginal component effects (AMCEs), by default, via the \code{\link{amce}} function and marginal means (MMs) via the \code{\link{mm}} function. Additional estimands may be supported in the future through their own functions and through the \code{cj} interface. Plotting is provided via ggplot2 for both types of estimates.
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
         ...
) {
    estimate <- match.arg(estimate)
    switch(estimate,
           amce = amce(data = data, formula = formula, id = id, weights = weights, ...),
           freqs = freqs(data = data, formula = formula, id = id, weights = weights, ...),
           mm = mm(data = data, formula = formula, id = id, weights = weights, ...)
           )
}
