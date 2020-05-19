context("cj_df() behaves as expected")

data(immigration, package = "cregg")
data(taxes, package = "cregg")

test_that("cj_df() works", {
    expect_true(inherits(immigration, "cj_df"), label = "'immigration' is 'cj_df' object")
    expect_true(inherits(taxes, "cj_df"), label = "'taxes' is 'cj_df' object")
    
    expect_true(inherits(cj_df(mtcars), "cj_df"), label = "cj_df() coerces to 'cj_df' object")
    expect_true(!inherits(as.data.frame(cj_df(mtcars)), "cj_df"), label = "as.data.frame() drops 'cj_df' class")
})

test_that("Subsetting 'cj_df' behaves as expected", {
    # [i,j] methods
    expect_true(identical(dim(immigration[1:15, c("Education", "Gender")]), c(15L, 2L)), label = "[i,j] works on 'cj_df' object")
    expect_true(!is.null(attr(immigration[1:15, c("Education", "Gender")][["Education"]], "label")), label = "[i,j] on 'cj_df' object returns label")
    
    expect_true(identical(dim(immigration[, c("Education", "Gender")]), c(13960L, 2L)), label = "[,j] works on 'cj_df' object")
    expect_true(!is.null(attr(immigration[, c("Education", "Gender")][["Education"]], "label")), label = "[,j] on 'cj_df' object returns label")
    
    expect_true(identical(dim(immigration[, "Education"]), c(13960L, 1L)), label = "[,j, drop = default] works on 'cj_df' object")
    expect_true(!is.null(attr(immigration[, "Education"][["Education"]], "label")), label = "[,j, drop = default] on 'cj_df' object returns label")
    
    expect_true(identical(dim(immigration[, "Education", drop = FALSE]), c(13960L, 1L)), label = "[,j, drop = FALSE] works on 'cj_df' object")
    expect_true(!is.null(attr(immigration[, "Education", drop = FALSE][["Education"]], "label")), label = "[,j, drop = FALSE] on 'cj_df' object returns label")
    
    expect_true(identical(dim(immigration[, 3:4]), c(13960L, 2L)), label = "[,j, drop = default] works with integer column on 'cj_df' object")
    expect_true(!is.null(attr(immigration[, 3:4][["Education"]], "label")), label = "[,j, drop = default] with integer column on 'cj_df' object returns label")

    expect_true(identical(dim(immigration[, 3]), c(13960L, 1L)), label = "[,j, drop = default] works with integer column on 'cj_df' object")
    expect_true(!is.null(attr(immigration[, 3][["Education"]], "label")), label = "[,j, drop = default] with integer column on 'cj_df' object returns label")
    
    # [[i]] methods
    expect_true(!inherits(immigration[["Education"]], "cj_df"), label = "[[i]] with integer works on 'cj_df' object")
    expect_true(!is.null(attr(immigration[["Education"]], "label")), label = "[[i]] with integer on 'cj_df' object returns label")
    
    expect_true(!inherits(immigration[["Education"]], "cj_df"), label = "[[i]] with character works on 'cj_df' object")
    expect_true(!is.null(attr(immigration[["Education"]], "label")), label = "[[i]] with character on 'cj_df' object returns label")
    
    # subsetting methods
    expect_true(nrow(head(immigration, 15)) == 15L, label = "head() works on 'cj_df' object")
    expect_true(!is.null(attr(head(immigration, 15)[["Education"]], "label")), label = "head() on 'cj_df' object returns label")
    
    expect_true(nrow(tail(immigration, 15)) == 15L, label = "tail() works on 'cj_df' object")
    expect_true(!is.null(attr(tail(immigration, 15)[["Education"]], "label")), label = "tail() on 'cj_df' object returns label")
    
    expect_true(nrow(subset(immigration, profile == 1)) == 6980L, label = "subset() rows-only works on 'cj_df' object")
    expect_true(!is.null(attr(subset(immigration, profile == 1)[["Education"]], "label")), label = "subset() on 'cj_df' object returns label")
    
    expect_true(nrow(subset(immigration, select = "Education")) == 13960L, label = "subset() columns-only works on 'cj_df' object")
    expect_true(!is.null(attr(subset(immigration, select = "Education")[["Education"]], "label")), label = "subset() on 'cj_df' object returns label")
    
    expect_true(nrow(subset(immigration, profile == 1, select = "Education")) == 6980L, label = "subset() rows and columns works on 'cj_df' object")
    expect_true(ncol(subset(immigration, profile == 1, select = "Education")) == 1L, label = "subset() rows and columns works on 'cj_df' object")
    expect_true(!is.null(attr(subset(immigration, profile == 1, select = "Education")[["Education"]], "label")), label = "subset() on 'cj_df' object returns label")
    
})
