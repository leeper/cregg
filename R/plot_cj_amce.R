#' @rdname plot
#' @aliases plot.cj_mm plot.cj_amce plot.cj_freqs
#' @title Plot AMCE estimates, MM descriptives, and frequency plots
#' @description ggplot2-based plotting of conjoint AMCEs estimates and MMs
#' @param x A data frame returned from \code{\link{cj}} or \code{\link{mm}}.
#' @param group Optionally a character string specifying a grouping factor. This is useful when, for example, subgroup analyses or comparing AMCEs for different outcomes. An alternative is to use \code{\link[ggplot2]{facet_wrap}} for faceted graphics.
#' @param feature_headers A logical indicating whether to include headers for each feature to visually separate levels for each feature (beyond the color palette).
#' @param header_fmt A character string specifying a \code{fmt} argument to \code{\link[base]{sprintf}}, which will be used when generating the feature headers (if \code{feature_headers = TRUE}).
#' @param size A numeric value specifying point size in \code{\link[ggplot2]{geom_point}}.
#' @param xlab A label for the x-axis
#' @param ylab A label for the y-axis
#' @param legend_title A character string specifying a label for the legend.
#' @param legend_pos An argument forwarded to the \code{legend.position} argument in \code{\link[ggplot2]{theme}}.
#' @param xlim A two-element number vector specifying limits for the x-axis. If \code{NULL}, a default value is calculated from the data.
#' @param vline Optionally, a numeric value specifying an x-intercept for a vertical line. This can be useful in distinguishing the midpoint of the estimates (e.g., a zero line for AMCEs).
#' @param vline_color A character string specifying a color for the \code{vline}.
#' @param theme A ggplot2 theme object
#' @param \dots Ignored.
#' @return A ggplot2 object
#' @examples
#' \donttest{
#' # load data
#' data("immigration")
#' 
#' # calculate MMs
#' d1 <- mm(immigration, ChosenImmigrant ~ Gender + Education + 
#'          LanguageSkills + CountryOfOrigin + Job + JobExperience + 
#'          JobPlans + ReasonForApplication + PriorEntry, id = ~ CaseID)
#' 
#' # plot MMs
#' ## simple plot
#' plot(d1)
#'
#' ## gridlines to aid interpretation
#' plot(d1) + ggplot2::theme_grey()
#'
#' ## plot with facetting by feature
#' plot(d1, feature_headers = FALSE) + 
#'   ggplot2::facet_wrap(~feature, ncol = 1L, 
#'                       scales = "free_y", strip.position = "right")
#'
#' # MMs split by profile number
#' stacked <- cj(immigration, ChosenImmigrant ~ Gender + 
#'               Education + LanguageSkills + CountryOfOrigin + Job + JobExperience + 
#'               JobPlans + ReasonForApplication + PriorEntry, id = ~ CaseID,
#'               estimate = "mm", by = ~ contest_no)
#' 
#' ## plot with grouping
#' plot(stacked, group = "contest_no", feature_headers = FALSE)
#' 
#' ## plot with facetting
#' plot(stacked) + ggplot2::facet_wrap(~contest_no, nrow = 1L)
#' 
#' # estimate AMCEs over different subsets of data
#' reasons12 <- subset(immigration, ReasonForApplication %in% levels(ReasonForApplication)[1:2])
#' d2_1 <- cj(immigration, ChosenImmigrant ~ CountryOfOrigin, id = ~ CaseID)
#' d2_2 <- cj(reasons12, ChosenImmigrant ~ CountryOfOrigin, id = ~ CaseID,
#'            feature_labels = list(CountryOfOrigin = "Country Of Origin"))
#' d2_1$reasons <- "1,2,3"
#' d2_2$reasons <- "1,2"
#' plot(rbind(d2_1, d2_2), group = "reasons")
#' }
#' @seealso \code{\link{amce}}, \code{\link{mm}}
#' @import ggplot2
#' @importFrom ggstance position_dodgev
#' @import scales
#' @export
plot.cj_amce <- 
function(
  x,
  group = attr(x, "by"),
  feature_headers = TRUE,
  header_fmt = "(%s)",
  size = 1.0,
  xlab = "Estimated AMCE",
  ylab = "",
  legend_title = if (is.null(group)) "Feature" else group,
  legend_pos = "bottom",
  xlim = NULL,
  vline = 0,
  vline_color = "gray",
  theme = ggplot2::theme_bw(),
  ...
) {
    
    # optionally, add gaps between features
    if (isTRUE(feature_headers)) {
        x$level <- make_feature_headers(x, fmt = header_fmt)
        to_merge <- data.frame(feature = unique(x$feature), level = sprintf(header_fmt, unique(x$feature)))
        if ("BY" %in% names(x)) {
            to_merge <- do.call("rbind", lapply(unique(x[["BY"]]), function(lev) {
                to_merge[["BY"]] <- lev
                to_merge
            }))
        } else if (!is.null(group)) {
            to_merge <- do.call("rbind", lapply(unique(x[[group]]), function(lev) {
                to_merge[[group]] <- lev
                to_merge
            }))
        }
        x <- merge(x, to_merge, all = TRUE)
    }
    
    if (is.null(group)) {
        p <- ggplot2::ggplot(data = x, ggplot2::aes_string(x = "estimate", y = "level", colour = "feature"))
    } else {
        p <- ggplot2::ggplot(data = x, ggplot2::aes_string(x = "estimate", y = "level", colour = group, group = group))
    }
    
    if (is.null(xlim)) {
        xmin <- min(x$lower, na.rm = TRUE)
        xmin <- if (xmin < 0) 1.04*xmin else .96*xmin
        xmax <- max(x$upper, na.rm = TRUE)
        xmax <- if (xmax > 0) 1.04*xmax else .96*xmax
        # make symmetric
        if (abs(xmin) > abs(xmax)) {
            xmax <- abs(xmin)
        } else {
            xmin <- -xmax
        }
        xlim <- c(xmin, xmax)
    }
    
    if (!is.null(vline)) {
        p <- p + ggplot2::geom_vline(xintercept = vline, colour = vline_color)
    }
    
    p <- p + ggplot2::geom_point(position = ggstance::position_dodgev(height = 0.75), size = size, na.rm = TRUE) +
             ggplot2::geom_errorbarh(ggplot2::aes_string(xmin = "lower", xmax = "upper"),  
                                     size = 0.2, height = 0, na.rm = TRUE,
                                     position = ggstance::position_dodgev(height = 0.75))
    if (is.null(group)) {
        p <- p + ggplot2::scale_colour_discrete(guide = ggplot2::guide_legend(title = legend_title))
    } else {
        p <- p + ggplot2::scale_colour_discrete(breaks = levels(x[[group]]),
                                                labels = levels(x[[group]]),
                                                guide = ggplot2::guide_legend(title = legend_title))
    }
    p <- p +
      ggplot2::scale_x_continuous(limits = xlim, oob = scales::rescale_none) +
      ggplot2::xlab(xlab) + 
      ggplot2::ylab(ylab) + 
      theme + ggplot2::theme(
        legend.position = legend_pos,
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      ) + 
      ggplot2::guides(colour = ggplot2::guide_legend(title = legend_title))
    return(p)
}
