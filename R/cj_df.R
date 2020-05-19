#' @rdname cj_df
#' @title Create a \dQuote{cj_df} data frame
#' @description A simple data frame extension that preserves attributes during subsetting operations.
#' @param x A data frame
#' @param i See \code{\link{[.data.frame}}
#' @param j See \code{\link{[.data.frame}}
#' @param drop Ignored.
#' @return An data frame with additional \dQuote{cj_df} class, which has subsetting methods that preserve variables attributes.
#' @examples
#' x1 <- data.frame(a = 1:3, b = 4:6)
#' attr(x1$a, "label") <- "Variable A"
#' 
#' # cj_df() returns a data frame
#' inherits(x1, "data.frame")
#' class(x1)
#' 
#' # attributes dropped for data frames
#' attr(x1[1:2,]$a, "label")
#' 
#' # attributes preserved with a cj_df
#' attr(cj_df(x1)[1:2,]$a, "label")
#' 
#' @export
cj_df <- function(x) {
    UseMethod("cj_df")
}

#' @rdname cj_df
#' @export
cj_df.data.frame <- function(x) {
    structure(x, class = c("cj_df", "data.frame"))
}

#' @rdname cj_df
#' @export
`[.cj_df` <- function (x, i, j, drop = FALSE) {
    x <- structure(x, class = "data.frame")
    if (missing(i)) {
        x2 <- x[, j, drop = FALSE]
    } else {
        if (missing(j)) {
            if (is.character(i)) {
                x2 <- x[, i, drop = FALSE]
            } else {
                x2 <- x[i, , drop = FALSE]
            }
        } else {
            x2 <- x[i, j, drop = FALSE]
        }
        # restore attributes
        for (coln in seq_along(x2)) {
            attributes(x2[[coln]]) <- attributes(x[[names(x2)[coln]]])
        }
    }
    
    # return
    return(cj_df(x2))
}
