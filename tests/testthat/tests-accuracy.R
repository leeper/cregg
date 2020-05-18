context("Estimates are correct")

# tolerance
tol <- 0.0001

# example data
require("stats")
set.seed(12345)
N <- 1000L
dat <- data.frame(id = rep(1:200, each = N/200L),
                  group = factor(rep(c(1L,2L), each = N/2L), 1:2, c("Low", "High")),
                  x1 = factor(sample(1:3, N, TRUE), 1:3, c("A", "B", "C")),
                  x2 = factor(sample(1:3, N, TRUE), 1:3, c("D", "E", "F")),
                  weight = runif(N))
attr(dat$x1, "label") <- "First Factor"
attr(dat$x2, "label") <- "Second Factor"
dat$y_star <- plogis(with(dat, model.matrix(~ 0 + interaction(x1, x2)) %*% c(0.5, -1, 0.5, 2, 0.3, 0.8, -0.2, 0.6, -0.8))[,1L])
dat$y <- rbinom(N, 1L, dat$y_star)
dat$y_star <- NULL

test_that("mm() returns correct marginal means", {
    mm_x1 <- aggregate(y~x1, data = dat, mean)
    mm_x2 <- aggregate(y~x2, data = dat, mean)
    
    mm_x1_lm_clust <- survey::svyglm(y ~ 0 + x1, design = survey::svydesign(id = ~ id, weights = ~ 1, data = dat))
    mm_x2_lm_clust <- survey::svyglm(y ~ 0 + x2, design = survey::svydesign(id = ~ id, weights = ~ 1, data = dat))
    mm_x1_var_clust <- coef(summary(mm_x1_lm_clust))[, "Std. Error", drop = TRUE]
    mm_x2_var_clust <- coef(summary(mm_x2_lm_clust))[, "Std. Error", drop = TRUE]
    
    mm_x1_lm_unclust <- survey::svyglm(y ~ 0 + x1, design = survey::svydesign(id = ~ 0, weights = ~ 1, data = dat))
    mm_x2_lm_unclust <- survey::svyglm(y ~ 0 + x2, design = survey::svydesign(id = ~ 0, weights = ~ 1, data = dat))
    mm_x1_var_unclust <- coef(summary(mm_x1_lm_unclust))[, "Std. Error", drop = TRUE]
    mm_x2_var_unclust <- coef(summary(mm_x2_lm_unclust))[, "Std. Error", drop = TRUE]
    
    est_clust <- mm(dat, y ~ x1 + x2, id = ~ id)
    est_unclust <- mm(dat, y ~ x1 + x2, id = ~ 0)

    ## structural checks
    expect_true(all(c("outcome", "statistic", "feature", "level", "estimate", "std.error", "lower", "upper") %in% names(est_clust)),
                label = "mm() returns correct column names")
    expect_true(identical(nrow(est_clust), 6L),
                label = "mm() returns correct number of estimates")
    expect_true(all(est_clust$feature %in% c("First Factor", "Second Factor")),
                label = "mm() returns correct feature labels")
    expect_true(all(est_clust$level %in% c(levels(dat$x1), levels(dat$x2))),
                label = "mm() returns correct levels of factors")
    expect_true(all(est_clust$outcome == "y"),
                label = "mm() returns correct outcome label")
    expect_true(inherits(mm(dat, y ~ x1 + x2, by = ~ group), "cj_mm"),
                label = "mm() works w/o 'id' argument")
    
    ## accuracy tests
    expect_true(all.equal(est_clust$estimate, c(mm_x1$y, mm_x2$y), tolerance = tol),
                label = "mm() with clustering returns correct MMs (based on aggregate())")
    expect_true(all.equal(est_clust$estimate, unname(c(coef(mm_x1_lm_clust), coef(mm_x2_lm_clust))), tolerance = tol),
                label = "mm() clustering returns correct MMs (based on lm(y ~ 0 + ...))")
    ## variance accuracy tests
    expect_true(all.equal(est_unclust$std.error, unname(c(mm_x1_var_unclust, mm_x2_var_unclust)), tolerance = 0.001), # TODO: ??
                label = "mm() returns correct unclustered variances for MMs")
    expect_true(all.equal(est_clust$std.error, unname(c(mm_x1_var_clust, mm_x2_var_clust)), tolerance = tol),
                label = "mm() returns correct clustered variances for MMs")
    
})

test_that("mm() responds to weighting/clustering", {
    est_unclust_unweight <- mm(dat, y ~ x1 + x2, id = ~ 0, weights = NULL)
    est_unclust_weight <- mm(dat, y ~ x1 + x2, id = ~ 0, weights = ~ weight)
    est_clust_unweight <- mm(dat, y ~ x1 + x2, id = ~ id, weights = NULL)
    est_clust_weight <- mm(dat, y ~ x1 + x2, id = ~ id, weights = ~ weight)
    expect_true(all.equal(est_unclust_unweight$estimate, est_clust_unweight$estimate, tolerance = tol),
                label = "mm() returns identical clustered and unclustered (unweighted)")
    expect_true(all.equal(est_unclust_weight$estimate, est_clust_weight$estimate, tolerance = tol),
                label = "mm() returns identical clustered and unclustered (weighted)")
    expect_false(isTRUE(all.equal(est_unclust_unweight$estimate, est_unclust_weight$estimate, tolerance = tol)),
                label = "mm() weighted estimates differ from unweighted")
    ## TODO: add variance test
})

test_that("mm_diffs() returns correct differences", {
    mm_x1 <- aggregate(y~x1 + group, data = dat, mean)
    mm_x2 <- aggregate(y~x2 + group, data = dat, mean)
    est <- mm_diffs(dat, y ~ x1 + x2, id = ~ id, by = ~ group)
    ## structural tests
    expect_true(all(c("BY", "outcome", "statistic", "feature", "level", "estimate", "std.error", "lower", "upper", "group") %in% names(est)),
                label = "mm_diffs() returns correct column names")
    expect_true(all(est$group == "High") & all(est$BY == "High - Low"),
                label = "mm_diffs() returns correct comparisons")
    expect_true(identical(nrow(est), 6L),
                label = "mm_diffs() returns correct number of estimates")
    expect_true(all(est$feature %in% c("First Factor", "Second Factor")),
                label = "mm_diffs() returns correct feature labels")
    expect_true(all(est$level %in% c(levels(dat$x1), levels(dat$x2))),
                label = "mm_diffs() returns correct levels of factors")
    expect_true(all(est$outcome == "y"),
                label = "mm_diffs() returns correct outcome label")
    expect_true(inherits(mm_diffs(dat, y ~ x1 + x2, by = ~ group), "cj_diffs"),
                label = "mm_diffs() works w/o 'id' argument")

    ## accuracy tests
    ## TODO: add variance test
    expect_true(all.equal(est$estimate, c(mm_x1$y[4:6] - mm_x1$y[1:3], mm_x2$y[4:6] - mm_x2$y[1:3]), tolerance = tol),
                label = "mm_diffs() returns correct differences")
})

test_that("amce() returns correct marginal effects for unconstrained designs", {
    reg <- glm(y~x1+x2, data = dat)
    est <- amce(dat, y ~ x1 + x2, id = ~ id)
    ## structural tests
    expect_true(all(c("outcome", "statistic", "feature", "level", "estimate", "std.error", "lower", "upper") %in% names(est)),
                label = "amce() returns correct column names")
    expect_true(identical(nrow(est), 6L),
                label = "amce() returns correct number of estimates")
    expect_true(all(est$feature %in% c("First Factor", "Second Factor")),
                label = "amce() returns correct feature labels")
    expect_true(all(est$level %in% c(levels(dat$x1), levels(dat$x2))),
                label = "amce() returns correct levels of factors")
    expect_true(all(est$outcome == "y"),
                label = "amce() returns correct outcome label")
    expect_true(all.equal(est$estimate,
                          unname(c(0, coef(reg)["x1B"], coef(reg)["x1C"], 0, coef(reg)["x2E"], coef(reg)["x2F"])),
                          tolerance = tol),
                label = "amce() returns correct effects for unconstrained design")
    expect_true(inherits(amce(dat, y ~ x1 + x2, id = ~ id), "cj_amce"),
                label = "amce() works w/o 'id' argument")
    
    ## accuracy tests
    ## TODO: add test of variances
    expect_true(all.equal(est$estimate, 
                         c(0, 
                           est[est$feature == "First Factor" & est$level == "B", "estimate"],
                           est[est$feature == "First Factor" & est$level == "C", "estimate"],
                           0, 
                           est[est$feature == "Second Factor" & est$level == "E", "estimate"],
                           est[est$feature == "Second Factor" & est$level == "F", "estimate"]
                         ), tolerance = tol),
                label = "amce() returns effects in correct order")
})

test_that("amce() responds to weighting/clustering for unconstrained design", {
    est_unclust_unweight <- amce(dat, y ~ x1 + x2, id = ~ 0, weights = NULL)
    est_unclust_weight <- amce(dat, y ~ x1 + x2, id = ~ 0, weights = ~ weight)
    est_clust_unweight <-amce(dat, y ~ x1 + x2, id = ~ id, weights = NULL)
    est_clust_weight <- amce(dat, y ~ x1 + x2, id = ~ id, weights = ~ weight)
    expect_true(all.equal(est_unclust_unweight$estimate, est_clust_unweight$estimate, tolerance = tol),
                label = "amce() returns identical clustered and unclustered (unweighted)")
    expect_true(all.equal(est_unclust_weight$estimate, est_clust_weight$estimate, tolerance = tol),
                label = "amce() returns identical clustered and unclustered (weighted)")
    ## TODO: add variance test
})

test_that("amce() returns correct marginal effects for constrained designs", {
    reg <- glm(y~x1*x2, data = subset(dat, !(x1 == "C" & x2 == "F")))
    reg_coef <- coef(reg)
    est <- amce(subset(dat, !(x1 == "C" & x2 == "F")), y ~ x1 * x2, id = ~ id)
    ## structural tests
    expect_true(all(c("outcome", "statistic", "feature", "level", "estimate", "std.error", "lower", "upper") %in% names(est)),
                label = "amce() returns correct column names")
    expect_true(identical(nrow(est), 6L),
                label = "amce() returns correct number of estimates")
    ## NOTE: SUBSETTING DROPS 'label' ATTRIBUTES FROM VARIABLES, SO LABELS ARE "WRONG" HERE
    expect_true(all(est$feature %in% c("x1", "x2")),
                label = "amce() returns correct feature labels")
    expect_true(all(est$level %in% c(levels(dat$x1), levels(dat$x2))),
                label = "amce() returns correct levels of factors")
    expect_true(all(est$outcome == "y"),
                label = "amce() returns correct outcome label")

    ## accuracy tests
    ## TODO: add variance test
    expect_true(all.equal(est$estimate,
                          c(0,
                            mean(c(reg_coef["x1B"], reg_coef["x1B"] + reg_coef["x1B:x2E"], reg_coef["x1B"] + reg_coef["x1B:x2F"])),
                            mean(c(reg_coef["x1C"], reg_coef["x1C"] + reg_coef["x1C:x2E"])),
                            0,
                            mean(c(reg_coef["x2E"], reg_coef["x2E"] + reg_coef["x1B:x2E"], reg_coef["x2E"] + reg_coef["x1C:x2E"])),
                            mean(c(reg_coef["x2F"], reg_coef["x2F"] + reg_coef["x1B:x2F"]))),
                          tolerance = tol),
                label = "amce() returns correct effects for constrained design")
    expect_true(all.equal(est$estimate, 
                         c(0, 
                           est[est$feature == "x1" & est$level == "B", "estimate"],
                           est[est$feature == "x1" & est$level == "C", "estimate"],
                           0, 
                           est[est$feature == "x2" & est$level == "E", "estimate"],
                           est[est$feature == "x2" & est$level == "F", "estimate"]
                         ), tolerance = tol),
                label = "amce() returns effects in correct order")
})

test_that("cj() by group returns correct marginal effects", {
    reg_low <- glm(y~x1+x2, data = dat, subset = group == "Low")
    reg_high <- glm(y~x1+x2, data = dat, subset = group == "High")
    est <- cj(dat, y ~ x1 + x2, id = ~ id, by = ~ group)
    ## structural tests
    expect_true(all(c("outcome", "statistic", "feature", "level", "estimate", "std.error", "lower", "upper") %in% names(est)),
                label = "grouped cj() returns correct column names")
    expect_true(identical(nrow(est), 12L),
                label = "grouped cj() returns correct number of estimates")
    expect_true(all(est$feature %in% c("First Factor", "Second Factor")),
                label = "grouped cj() returns correct feature labels")
    expect_true(all(est$level %in% c(levels(dat$x1), levels(dat$x2))),
                label = "grouped cj() returns correct levels of factors")
    expect_true(all(est$outcome == "y"),
                label = "grouped cj() returns correct outcome label")
    expect_true(all(est$group %in% c("Low", "High")),
                label = "grouped cj() returns correct group labels")
    expect_true(all.equal(est$estimate[est$level %in% c("A", "D")], rep(0L, 4), tolerance = tol),
                label = "group cj() returns correct 0s")
    expect_true(inherits(cj(dat, y ~ x1 + x2, by = ~ group), "cj_amce"),
                label = "group cj() works w/o 'id' argument")
    
    ## accuracy tests
    ## TODO: add variance test
    expect_true(all.equal(c(coef(reg_low)[2:5], coef(reg_high)[2:5]),
                          est$estimate[c(2:3, 5:6, 8:9, 11:12)],
                          tolerance = tol,
                          check.attributes = FALSE),
                label = "group cj() returns correct marginal effects")
})

test_that("amce_diffs() returns correct differences", {
    reg_low <- glm(y~x1+x2, data = dat, subset = group == "Low")
    reg_high <- glm(y~x1+x2, data = dat, subset = group == "High")
    est <- amce_diffs(dat, y ~ x1 + x2, id = ~ id, by = ~ group)
    ## structural tests
    expect_true(all(c("outcome", "statistic", "feature", "level", "estimate", "std.error", "lower", "upper") %in% names(est)),
                label = "amce_diffs() returns correct column names")
    expect_true(identical(nrow(est), 4L),
                label = "amce_diffs() returns correct number of estimates")
    expect_true(all(est$feature %in% c("First Factor", "Second Factor")),
                label = "amce_diffs() returns correct feature labels")
    expect_true(all(est$level %in% c(levels(dat$x1), levels(dat$x2))),
                label = "amce_diffs() returns correct levels of factors")
    expect_true(all(est$outcome == "y"),
                label = "amce_diffs() returns correct outcome label")
    expect_true(all(est$group == "High"),
                label = "amce_diffs() returns correct group labels")
    expect_true(inherits(amce_diffs(dat, y ~ x1 + x2, by = ~ group), "cj_diffs"),
                label = "amce_diffs() works w/o 'id' argument")
    
    ## accuracy tests
    ## TODO: add variance test
    expect_true(all.equal(est$estimate,
                          (coef(reg_high) - coef(reg_low))[2:5],
                          tolerance = tol,
                          check.attributes = FALSE),
                label = "amce_diffs() returns correct differences")
})

test_that("Functions respect 'alpha' argument", {
    expect_false(
      identical(
        mm(dat, y ~ x1 + x2, id = ~ 0, alpha = 0.05),
        mm(dat, y ~ x1 + x2, id = ~ 0, alpha = 0.01)
      ), label = "mm() respects 'alpha' argument"
    )
    expect_false(
      identical(
        mm_diffs(dat, y ~ x1 + x2, id = ~ 0, alpha = 0.05, by = ~ group),
        mm_diffs(dat, y ~ x1 + x2, id = ~ 0, alpha = 0.01, by = ~ group)
      ), label = "mm_diffs() respects 'alpha' argument"
    )
    expect_false(
      identical(
        amce(dat, y ~ x1 + x2, id = ~ 0, alpha = 0.05),
        amce(dat, y ~ x1 + x2, id = ~ 0, alpha = 0.01)
      ), label = "amce() respects 'alpha' argument"
    )
    expect_false(
      identical(
        amce_diffs(dat, y ~ x1 + x2, id = ~ 0, alpha = 0.05, by = ~ group),
        amce_diffs(dat, y ~ x1 + x2, id = ~ 0, alpha = 0.01, by = ~ group)
      ), label = "amce_diffs() respects 'alpha' argument"
    )
})
