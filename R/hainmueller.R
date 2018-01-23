#' @rdname hainmueller
#' @docType data
#' @title Immigration Conjoint Experiment Dataset from Hainmueller et. al. (2014)
#' @description A dataset containing the results of a conjoint survey of a representative sample of American adults who were asked to choose which hypothetical immigrants they think should be admitted into the United States. Each row corresponds to a single profile presented to the respondent.
#' @format A data frame with 13960 observations on the following 16 variables.
#'  \describe{
#'    \item{\samp{CaseID}}{a numeric vector indicating the respondent to which the particular profile corresponds}
#'    \item{\samp{contest_no}}{a numeric vector indicating the number of the task to which the profile corresponds}
#'    \item{\samp{Education}}{a factor with levels \dQuote{No formal}, \dQuote{4th grade}, \dQuote{8th grade}, \dQuote{High school}, \dQuote{Two-year college}, \dQuote{college Degree}, \dQuote{Graduate degree}}
#'    \item{\samp{Gender}}{a factor with levels \dQuote{Female}, \dQuote{Male}}
#'    \item{\samp{CountryOfOrigin}}{a factor with levels \dQuote{India}, \dQuote{Germany}, \dQuote{France}, \dQuote{Mexico}, \dQuote{Philippines}, \dQuote{Poland}, \dQuote{China}, \dQuote{Sudan}, \dQuote{Somalia}, \dQuote{Iraq}}
#'    \item{\samp{ReasonForApplication}}{a factor with levels \dQuote{Reunite with family}, \dQuote{Seek better job}, \dQuote{Escape persecution}}
#'    \item{\samp{Job}}{a factor with levels \dQuote{Janitor}, \dQuote{Waiter}, \dQuote{Child care provider}, \dQuote{Gardener}, \dQuote{Financial analyst}, \dQuote{Construction worker}, \dQuote{Teacher}, \dQuote{Computer programmer}, \dQuote{Nurse}, \dQuote{Research scientist}, \dQuote{Doctor}}
#'    \item{\samp{JobExperience}}{a factor with levels \dQuote{None}, \dQuote{1-2 years}, \dQuote{3-5 years}, \dQuote{5+ years}}
#'    \item{\samp{JobPlans}}{a factor with levels \dQuote{Will look for work}, \dQuote{Contract with employer}, \dQuote{Interviews with employer}, \dQuote{No plans to look for work}}
#'    \item{\samp{PriorEntry}}{a factor with levels \dQuote{Never}, \dQuote{Once as tourist}, \dQuote{Many times as tourist}, \dQuote{Six months with family}, \dQuote{Once w/o authorization}}
#'    \item{\samp{LanguageSkills}}{a factor with levels \dQuote{Fluent English}, \dQuote{Broken English}, \dQuote{Tried English but unable}, \dQuote{Used interpreter}}
#'    \item{\samp{ChosenImmigrant}}{a numeric vector denoting whether the immigrant profile was selected}
#'    \item{\samp{ethnocentrism}}{a numeric vector}
#'    \item{\samp{profile}}{a numeric vector giving the profile number}
#'    \item{\samp{LangPos}}{a numeric vector}
#'    \item{\samp{PriorPos}}{a numeric vector}
#'  }
#' @source Hainmueller, J., Hopkins, D., and Yamamoto T. (2014) Causal Inference in Conjoint Analysis: Understanding Multi-Dimensional Choices via Stated Preference Experiments. Political Analysis 22(1):1-30
#' @note This is a modified version of the dataset available from the \href{https://cran.r-project.org/package=cjoint}{cjoint} package.
#' @examples
#' \dontrun{
#' data("hainmueller")
#' cj(hainmueller, ChosenImmigrant ~ Gender + Education, id = ~ CaseID)
#' }
#' @seealso \code{\link{amce}} \code{\link{mm}} \code{\link{mm}}
"hainmueller"
