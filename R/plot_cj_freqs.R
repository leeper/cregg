#' @rdname plot
#' @export
plot.cj_freqs <- 
function(
  x,
  group = attr(x, "by"),
  feature_headers = TRUE,
  header_fmt = "(%s)",
  xlab = "",
  ylab = "Frequency",
  legend_title = if (is.null(group)) "Feature" else group,
  legend_pos = "bottom",
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
        p <- ggplot2::ggplot(data = x, ggplot2::aes_string(y = "estimate", x = "level", fill = "feature"))
    } else {
        p <- ggplot2::ggplot(data = x, ggplot2::aes_string(y = "estimate", x = "level", fill = group, group = group))
    }
    
    p <- p + ggplot2::geom_col(na.rm = TRUE) + 
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
