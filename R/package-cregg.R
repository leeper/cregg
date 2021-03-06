#' @rdname cregg
#' @name cregg-package
#' @title Simple Conjoint Analyses and Visualization
#' @aliases cregg-package cregg cj
#' @docType package
#' @description Simple analyses of conjoint (factorial) experiments and visualization of results.
#' @param data A data frame containing variables specified in \code{formula}. All RHS variables should be factors; the base level for each will be used in estimation and for AMCEs the base level's AMCE will be zero. Optionally, this can instead be an object of class \dQuote{survey.design} returned by \code{\link[survey]{svydesign}}.
#' @param formula A formula specifying a model to be estimated. ; all levels across features should be unique. For \code{estimate = "amce"} in a constrained conjoint design, two-way interactions can be specified to handle constraints between factors in the design. These are detected automatically. Higher-order constraints are not allowed and interactions are ignored for all other values of \code{estimate} as constraints are irrelevant to those statistics.
#' @param id An RHS formula specifying a variable holding respondent identifiers, to be used for clustering standard errors.
#' @param weights An (optional) RHS formula specifying a variable holding survey weights.
#' @param estimate A character string specifying an estimate type. Current options are average marginal component effects (or AMCEs, \dQuote{amce}, estimated via \code{\link{amce}}), display frequencies (\dQuote{frequncies}, estimated via \code{\link{cj_freqs}}), marginal means (or AMMs, \dQuote{mm}, estimated via \code{\link{mm}}), differences in MMs (\dQuote{mm_differences}, via \code{\link{mm_diffs}}), or differences in AMCEs (\dQuote{amce_differences}, via \code{\link{amce_diffs}}). Additional options may be made available in the future. Non-ambiguous abbreviations are allowed.
#' @template feature_order
#' @template feature_labels
#' @template level_order
#' @param by A formula containing only RHS variables, specifying grouping factors over which to perform estimation.
#' @param \dots Additional arguments to \code{\link{amce}}, \code{\link{cj_freqs}}, \code{\link{mm}}, \code{\link{mm_diffs}}, or \code{\link{amce_diffs}}.
#' @author Thomas J. Leeper <thosjleeper@gmail.com>
#' @return A data frame with special class to facilitate plotting (e.g., \dQuote{cj_amce}, \dQuote{cj_mm}, etc.)
#' @details The main function \code{cj} is a convenience function wrapper around the underlying estimation functions that provide for average marginal component effects (AMCEs), by default, via the \code{\link{amce}} function, marginal means (MMs) via the \code{\link{mm}} function, and display frequencies via \code{\link{cj_freqs}} and \code{\link{cj_props}}. Additional estimands may be supported in the future through their own functions and through the \code{cj} interface. Plotting is provided via ggplot2 for all types of estimates.
#' 
#' The only additional functionality provided by \code{cj} over the underlying functions is the \code{by} argument, which will perform operations on subsets of \code{data}, returning a single data frame. This can be useful, for example, for evaluating profile spillover effects and subgroup results, or in any situation where one might be inclined to use a \code{for} loop or \code{lapply}, calling \code{cj} repeatedly on subgroups.
#' 
#' Note: Some features of cregg (namely, the \code{\link{amce_diffs}}) function, or \code{estimate = "amce_diff"} here) only work with full factorial conjoint experiments. Designs involving two-way constraints between features are supported simply by expressing interactions between constrained terms in \code{formula} (again, except for \code{amce_diffs}). Higher-order constraints may be supported in the future.
#' 
#' @examples
#' \donttest{
#' # load data
#' requireNamespace("ggplot2")
#' data("immigration")
#' immigration$contest_no <- factor(immigration$contest_no)
#' data("taxes")
#' 
#' # calculate MMs
#' f1 <- ChosenImmigrant ~ Gender + Education + 
#'          LanguageSkills + CountryOfOrigin + Job + JobExperience + 
#'          JobPlans + ReasonForApplication + PriorEntry
#' d1 <- cj(immigration, f1, id = ~ CaseID, estimate = "mm", h0 = 0.5)
#' # plot MMs
#' plot(d1, vline = 0.5)
#'
#' # calculate MMs for survey-weighted data
#' d1 <- cj(taxes, chose_plan ~ taxrate1 + taxrate2 + taxrate3 +
#'          taxrate4 + taxrate5 + taxrate6 + taxrev, id = ~ ID,
#'          weights = ~ weight, estimate = "mm", h0 = 0.5)
#' # plot MMs
#' plot(d1, vline = 0.5)
#'
#' # MMs split by profile number
#' stacked <- cj(immigration, f1, id = ~ CaseID,
#'               estimate = "mm", by = ~ contest_no)
#' 
#' ## plot with grouping
#' plot(stacked, group = "contest_no", vline = 0.5, feature_headers = FALSE)
#' 
#' ## plot with facetting
#' plot(stacked) + ggplot2::facet_wrap(~ contest_no, nrow = 1L)
#' 
#' # estimate AMCEs
#' d2 <- cj(immigration, f1, id = ~ CaseID)
#' 
#' # plot AMCEs
#' plot(d2)
#' 
#' ## subgroup analysis
#' immigration$ethnosplit <- cut(immigration$ethnocentrism, 2)
#' x <- cj(na.omit(immigration), ChosenImmigrant ~ Gender + Education + LanguageSkills,
#'         id = ~ CaseID, estimate = "mm", h0 = 0.5, by = ~ ethnosplit)
#' plot(x, group = "ethnosplit", vline = 0.5)
#' 
#' # combinations of/interactions between features
#' immigration$language_entry <- 
#'   interaction(immigration$LanguageSkills, immigration$PriorEntry, sep = "_")
#'
#' ## higher-order MMs for feature combinations
#' cj(immigration, ChosenImmigrant ~ language_entry,
#'    id = ~CaseID, estimate = "mm", h0 = 0.5)
#'
#' ## constrained designs
#' ## in a constrained design, some cells are unobserved:
#' subset(cj_props(immigration, ~ Job + Education), Proportion == 0)
#' ## MMs and AMCEs only use data from observed cells
#' ## In `immigraation`, this means while the MM for `Job == "Janitor"` is an average 
#' ## across all levels of Education:
#' mm(subset(immigration, Job == "Janitor"), ChosenImmigrant ~ Education)
#' ## the MM for `Job == "Doctor"` is an average across only 3 levels of education:
#' mm(subset(immigration, Job == "Doctor"), ChosenImmigrant ~ Education)
#' ## Use `cj_props()` to see constraints:
#' subset(cj_props(immigration, ~ Job + Education), Job == "Doctor" & Proportion != 0)
#' 
#' ## Substantively, the MM of "Doctor" might be higher than other levels of `Job`
#' ## this could be due to the feature itself or due to the fact that it is constrained
#' ## with a different subset of other feature levels than alternative levels of `Job`
#' ## this may mean analysts want to report MMs (or AMCEs) only for the unconstrained levels:
#' elev <- c("Two-Year College", "College Degree", "Graduate Degree")
#' jlev <- c("Financial Analyst", "Computer Programmer", "Research Scientist", "Doctor")
#' mm(subset(immigration, Education %in% elev), ChosenImmigrant ~ Job)
#' mm(subset(immigration, Job %in% jlev), ChosenImmigrant ~ Education)
#' ## or, present estimates excluding constrained levels:
#' mm(subset(immigration, !Education %in% elev), ChosenImmigrant ~ Job)
#' mm(subset(immigration, !Job %in% jlev), ChosenImmigrant ~ Education)
#' }
#' @seealso
#'  Functions: \code{\link{amce}}, \code{\link{mm}}, \code{\link{cj_freqs}}, \code{\link{mm_diffs}}, \code{\link{plot.cj_amce}}, \code{\link{cj_tidy}}
#'  Data: \code{\link{immigration}}, \code{\link{taxes}}
#' @keywords package 
NULL
