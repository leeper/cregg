#' @title Tidy a conjoint dataset
#' @description Coerce a \dQuote{wide} conjoint dataset into a \dQuote{long}/\dQuote{tidy} one for use with cregg
#' @param data A data frame containing a conjoint dataset in \dQuote{wide} format (see Details).
#' @param profile_variables A named list of two-element lists capturing profile-specific variables (either features, or profile-specific outcomes, like rating scales). For each element in the list, the first element contains vectors of feature variable names for the first profile in each decision task (hereafter, profile \dQuote{A}) and the second element contains vectors of feature variable names for the second profile in each decision task (hereafter, profile \dQuote{B}). The names at the highest level are used to name variables in the long/tidy output.
#' @param task_variables A named list of vectors of variables constituting task-level variables (i.e., variables that differ by task but not across profiles within a task). These could be outcome variables, response times, etc.
#' @param id An RHS formula specifying a variable holding respondent identifiers.
#' @details A conjoint survey typically comes to the analyst in a \dQuote{wide} form, where the number of rows is equal to the number of survey respondents and columns represent choices and features for each choice task and task profile. For example, a design with 1000 respondents and five forced-choice decision tasks, with 6 features each, will have 1000 rows and 5x2x6 feature columns, plus five forced-choice outcome variable columns recording which alternative was selected for each task. To analyse these data, the data frame needs to be reshaped to \dQuote{long} or \dQuote{tidy} format, with 1000x5x2 rows, six feature columns, and one outcome column. Multiple outcomes or other task-specific variables would increase the number of columns in the result, as will respondent-varying characteristics which need to be replicated across each decision task and profile.
#' 
#' Some care is needed with outcomes variables. If a variable in the original format records \emph{which} of the two profiles was chosen (e.g., \dQuote{left} and \dQuote{right}), it should go in \code{task_variables}. If it records whether a profile was chosen (e.g., for each task there is a \dQuote{left_chosen} and \dQuote{right_chosen} variable), then both variables should go in \code{profile_variables} as they vary at the profile level. Similarly, one needs to be careful with the output of \code{cj_tidy} to ensure that a task-level variable is further recoded to encode which alternative was selected (see examples).
#' 
#' \code{cj_tidy} performs tidying in two steps, once to stack decision tasks and a second time to stack the profiles within each task. Generally, \code{data} should not use the variable names \dQuote{task}, \dQuote{pair}, or \dQuote{profile}.
#' 
#' @return A data frame with rows equal to the number of respondents times the number of tasks times the number of profiles (fixed at 2), to be fed into any other function in the package. The columns will include the names of elements in \code{profile_variables} and \code{task_variables}, and \code{id}, along with an indicator \code{task} (from 1 to the number of tasks), \code{pair} (an indicator for each task pair), \code{profile} (either \dQuote{A} or \dQuote{B}), and any other respondent-varying covariates not specified. As such, respondent-varying variables do not need to be specified at all.
#' @examples
#' \dontrun{
#' data("wide_conjoint")
#' # profile_variables
#' list1 <- list(
#'  feature1 = list(
#'      names(wide_conjoint)[grep("^feature1.{1}1", names(wide_conjoint))],
#'      names(wide_conjoint)[grep("^feature1.{1}2", names(wide_conjoint))]
#'  ),
#'  feature2 = list(
#'      names(wide_conjoint)[grep("^feature2.{1}1", names(wide_conjoint))],
#'      names(wide_conjoint)[grep("^feature2.{1}2", names(wide_conjoint))]
#'  ),
#'  feature3 = list(
#'      names(wide_conjoint)[grep("^feature3.{1}1", names(wide_conjoint))],
#'      names(wide_conjoint)[grep("^feature3.{1}2", names(wide_conjoint))]
#'  ),
#'  rating = list(
#'      names(wide_conjoint)[grep("^rating.+1", names(wide_conjoint))],
#'      names(wide_conjoint)[grep("^rating.+2", names(wide_conjoint))]
#'  )
#' )
#' # task variables
#' list2 <- list(choice = paste0("choice_", letters[1:4]),
#'               timing = paste0("timing_", letters[1:4]))
#'
#' # perform reshape
#' str(long <- cj_tidy(wide_conjoint, profile_variables = list1, task_variables = list2, id = ~ respondent))
#' stopifnot(nrow(long) == nrow(wide_conjoint)*4*2)
#' 
#' # recode outcome so it is coded sensibly
#' long$chosen <- ifelse((long$profile == "A" & long$choice == 1) | 
#'                        (long$profile == "B" & long$choice == 2), 1, 0)
#' # use for analysis
#' cj(long, chosen ~ feature1 + feature2 + feature3, id = ~ respondent)
#' }
#' @seealso \code{\link{cj}}
#' @export
cj_tidy <-
function(
  data,
  profile_variables,
  task_variables,
  id
) {
    
    # create 'idvar' character string
    idvar <- all.vars(update(id, 0 ~ . ))
    
    # sanity check `profile_variables`
    ## all are two-element lists
    stopifnot(all(lengths(profile_variables) == 2L))
    ## all are the same length
    n_tasks <- unique(unlist(lapply(profile_variables, lengths)))
    stopifnot(length(n_tasks) == 1L)
    
    # sanity check `task_variables`
    stopifnot(all(lengths(task_variables) == n_tasks))
    
    # warn for possible variable name problems
    if (any(c("profile", "task", "pair") %in% names(data))) {
        stop("'profile', 'task', and 'pair' are reserved variable names and cannot appear in 'data'")
    }
    
    # half stack pairs of variables into partial stack (nrow(data) * n_tasks)
    varying_list <- c(stats::setNames(unlist(profile_variables, recursive = FALSE),
                                      paste0(rep(names(profile_variables), each = 2L), "_", c("A", "B"))),
                      task_variables)
    halfstack <- stats::reshape(
      data,
      varying = varying_list,
      v.names = names(varying_list),
      timevar = "task",
      idvar = idvar,
      ids = data[[idvar]],
      direction = "long"
    )
    ## verify length of 'halfstack'
    if (nrow(halfstack) != nrow(data) * n_tasks) {
        if (nrow(halfstack) > nrow(data) * n_tasks) {
            warning(sprintf("Half-stacked dataset has more rows (%0.0f) than expected (%0.0f)", nrow(halfstack), nrow(data) * n_tasks))
        } else {
            warning(sprintf("Half-stacked dataset has fewer rows (%0.0f) than expected (%0.0f)", nrow(halfstack), nrow(data) * n_tasks))
        }
    }
    
    ## duplicate 'task_variables' to create left and right versions, dropping original
    for (i in seq_len(length(task_variables))) {
        halfstack[[paste0(names(task_variables)[i], "_A")]] <- halfstack[[names(task_variables)[i]]]
        halfstack[[paste0(names(task_variables)[i], "_B")]] <- halfstack[[names(task_variables)[i]]]
        halfstack[[names(task_variables)[i]]] <- NULL
        ## drop all variables listed in this element of 'task_variables'
        for (j in task_variables[[i]]) {
            halfstack[[j]] <- NULL
        }
    }
    
    # full stack the intermediate structure into the full dataset (nrow(data) * n_tasks * 2)
    ## define feature_variable names in 'halfstack'
    profiles_halfstack <- split(paste0(rep(names(profile_variables), each = 2L), "_", c("A", "B")), rep(seq_len(length(profile_variables)), each = 2L))
    ## define task_variable names in 'halfstack'
    tasks_halfstack <- split(paste0(rep(names(task_variables), each = 2L), "_", c("A", "B")), rep(seq_len(length(task_variables)), each = 2L))
    
    ## do the reshape
    fullstack <- stats::reshape(
      halfstack,
      varying = c(profiles_halfstack, tasks_halfstack),
      v.names = c(names(profile_variables), names(task_variables)),
      timevar = "profile",
      idvar = "pair",
      direction = "long"
    )
    ## verify length of 'fullstack'
    if (nrow(fullstack) != nrow(data) * n_tasks * 2L) {
        if (nrow(fullstack) > nrow(data) * n_tasks * 2L) {
            warning(sprintf("Full-stacked dataset has more rows (%0.0f) than expected (%0.0f)", nrow(fullstack), nrow(data) * n_tasks * 2L))
        } else {
            warning(sprintf("Full-stacked dataset has fewer rows (%0.0f) than expected (%0.0f)", nrow(fullstack), nrow(data) * n_tasks * 2L))
        }
    }
    
    # cleanup return
    ## factorize 'profile' variable
    fullstack[["profile"]] <- factor(fullstack[["profile"]], labels = c("A", "B"))
    ## remove unnecesary reshaping attribute
    attr(fullstack, "reshapeLong") <- NULL
    return(fullstack)
}
