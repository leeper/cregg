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
#' @details Two \code{plot} methods are implemented: one for \dQuote{amce} data frames of conjoint AMCE estimates (returned by \code{\link{amce}}), and another for \dQuote{mm} data frames as returned by \code{\link{mm}}. Both functions are basically identical but vary slightly in their superficial features (axis labels, etc.).
#' @return A ggplot2 object
#' @examples
#' \dontrun{
#' # load data
#' requireNamespace("ggplot2")
#' data("hainmueller")
#' 
#' # calculate MMs
#' d1 <- mm(hainmueller, ChosenImmigrant ~ Gender + Education + 
#'          LanguageSkills + CountryOfOrigin + Job + JobExperience + 
#'          JobPlans + ReasonForApplication + PriorEntry, id = ~ CaseID)
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
#' stacked$contest <- factor(stacked$contest, levels = 1:3, labels = c("First", "Second", "Third"))
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
#' @seealso \code{\link{amce}}, \code{\link{mm}}
#' @export
plot.cj_amce <- 
function(x, 
         group = NULL,
         feature_headers = TRUE,
         header_fmt = "(%s)",
         size = 1.0,
         xlab = "Estimated AMCE",
         ylab = "",
         legend_title = "Feature",
         legend_pos = "bottom",
         xlim = NULL,
         vline = 0,
         vline_color = "gray",
         theme = ggplot2::theme_bw(),
         ...
) {
    
    # check for dependencies
    requireNamespace("ggplot2")
    requireNamespace("scales")
    requireNamespace("ggstance")
    
    # optionally, add gaps between features
    if (isTRUE(feature_headers)) {
        x$level <- make_feature_headers(x, fmt = header_fmt)
        x <- merge(x, data.frame(feature = unique(x$feature), level = sprintf(header_fmt, unique(x$feature))), all = TRUE)
    }
    
    if (is.null(group)) {
        p <- ggplot2::ggplot(data = x, ggplot2::aes_string(x = "estimate", y = "level", colour = "feature"))
    } else {
        p <- ggplot2::ggplot(data = x, ggplot2::aes_string(x = "estimate", y = "level", colour = group, group = group))
    }
    
    if (is.null(xlim)) {
        xmin <- min(x$lower, na.rm = TRUE)
        xmax <- max(x$upper, na.rm = TRUE)
        xlim <- c(if (xmin < 0) 1.04*xmin else .96*xmin, if (xmax > 0) 1.04*xmax else .96*xmax)
    }
    
    if (!is.null(vline)) {
        p <- p + ggplot2::geom_vline(xintercept = vline, colour = vline_color)
    }
    
    p <- p + ggplot2::geom_point(position = ggstance::position_dodgev(height = 0.75), size = size) +
             ggplot2::geom_errorbarh(ggplot2::aes_string(xmin = "lower", xmax = "upper"),  
                                     size = 0.2, height = 0,
                                     position = ggstance::position_dodgev(height = 0.75)) + 
             ggplot2::scale_colour_discrete(guide = ggplot2::guide_legend(title = legend_title)) +
             ggplot2::scale_x_continuous(limits = xlim, oob = scales::rescale_none) +
             ggplot2::xlab(xlab) + 
             ggplot2::ylab(ylab)
    p <- p + theme + 
      ggplot2::theme(
        legend.position = legend_pos,
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      ) + 
      ggplot2::guides(colour = ggplot2::guide_legend(title = legend_title))
    return(p)
}

#' @export
plot.cj_mm <- 
function(x, 
         group = NULL,
         feature_headers = TRUE,
         header_fmt = "(%s)",
         size = 1.0,
         xlab = "Marginal Mean",
         ylab = "",
         legend_title = "Feature",
         legend_pos = "bottom",
         xlim = NULL,
         vline = 0,
         vline_color = "gray",
         theme = ggplot2::theme_bw(),
         ...
) {
    
    # check for dependencies
    requireNamespace("ggplot2")
    requireNamespace("scales")
    requireNamespace("ggstance")
    
    # optionally, add gaps between features
    if (isTRUE(feature_headers)) {
        x$level <- make_feature_headers(x, fmt = header_fmt)
        x <- merge(x, data.frame(feature = unique(x$feature), level = sprintf(header_fmt, unique(x$feature))), all = TRUE)
    }
    
    if (is.null(group)) {
        p <- ggplot2::ggplot(data = x, ggplot2::aes_string(x = "estimate", y = "level", colour = "feature"))
    } else {
        p <- ggplot2::ggplot(data = x, ggplot2::aes_string(x = "estimate", y = "level", colour = group, group = group))
    }
    
    if (is.null(xlim)) {
        xmin <- min(x$lower, na.rm = TRUE)
        xmax <- max(x$upper, na.rm = TRUE)
        xlim <- c(if (xmin < 0) 1.04*xmin else .96*xmin, if (xmax > 0) 1.04*xmax else .96*xmax)
    }
    
    if (!is.null(vline)) {
        p <- p + ggplot2::geom_vline(xintercept = vline, colour = vline_color)
    }
    
    p <- p + ggplot2::geom_point(position = ggstance::position_dodgev(height = 0.75), size = size) +
             ggplot2::geom_errorbarh(ggplot2::aes_string(xmin = "lower", xmax = "upper"),  
                                     size = 0.2, height = 0,
                                     position = ggstance::position_dodgev(height = 0.75)) + 
             ggplot2::scale_colour_discrete(guide = ggplot2::guide_legend(title = legend_title)) +
             ggplot2::scale_x_continuous(limits = xlim, oob = scales::rescale_none) +
             ggplot2::xlab(xlab) + 
             ggplot2::ylab(ylab)
    p <- p + theme + 
      ggplot2::theme(
        legend.position = legend_pos,
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      )
    return(p)
}

#' @export
plot.cj_freqs <- 
function(x, 
         group = NULL,
         feature_headers = TRUE,
         header_fmt = "(%s)",
         xlab = "",
         ylab = "Frequency",
         legend_title = "Feature",
         legend_pos = "bottom",
         theme = ggplot2::theme_bw(),
         ...
) {
    
    # check for dependencies
    requireNamespace("ggplot2")
    requireNamespace("scales")
    
    # optionally, add gaps between features
    if (isTRUE(feature_headers)) {
        x$level <- make_feature_headers(x, fmt = header_fmt)
        x <- merge(x, data.frame(feature = unique(x$feature), level = sprintf(header_fmt, unique(x$feature))), all = TRUE)
    }
    
    if (is.null(group)) {
        p <- ggplot2::ggplot(data = x, ggplot2::aes_string(y = "estimate", x = "level", fill = "feature"))
    } else {
        p <- ggplot2::ggplot(data = x, ggplot2::aes_string(y = "estimate", x = "level", fill = group, group = group))
    }
    
    p <- p + ggplot2::geom_col() + 
             ggplot2::scale_fill_discrete(guide = ggplot2::guide_legend(title = legend_title)) +
             ggplot2::coord_flip() + 
             ggplot2::xlab(xlab) + 
             ggplot2::ylab(ylab)
    
    p <- p + theme + 
      ggplot2::theme(
        legend.position = legend_pos,
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      )
    return(p)
}

make_feature_headers <- function(x, fmt = "(%s)") {
    feature_levels <- rev(split(x$level, x$feature))
    for (i in seq_along(feature_levels)) {
        feature_levels[[i]] <- levels(x$level)[match(feature_levels[[i]], levels(x$level))]
        feature_levels[[i]] <- c(feature_levels[[i]], sprintf(fmt, names(feature_levels)[i]))
    }
    factor(as.character(x$level), levels = unique(unname(unlist(feature_levels))))
}
