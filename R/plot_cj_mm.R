#' @rdname plot
#' @export
plot.cj_mm <- 
function(
  x,
  group = attr(x, "by"),
  feature_headers = TRUE,
  header_fmt = "(%s)",
  size = 1.0,
  xlab = "Marginal Mean",
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
