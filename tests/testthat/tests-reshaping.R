context("cj_tidy() works as expected")

data(wide_conjoint, package = "cregg")

test_that("cj_tidy() works in character string interface", {
    # profile_variables
    list1 <- list(
     feature1 = list(
         names(wide_conjoint)[grep("^feature1.{1}1", names(wide_conjoint))],
         names(wide_conjoint)[grep("^feature1.{1}2", names(wide_conjoint))]
     ),
     feature2 = list(
         names(wide_conjoint)[grep("^feature2.{1}1", names(wide_conjoint))],
         names(wide_conjoint)[grep("^feature2.{1}2", names(wide_conjoint))]
     ),
     feature3 = list(
         names(wide_conjoint)[grep("^feature3.{1}1", names(wide_conjoint))],
         names(wide_conjoint)[grep("^feature3.{1}2", names(wide_conjoint))]
     ),
     rating = list(
         names(wide_conjoint)[grep("^rating.+1", names(wide_conjoint))],
         names(wide_conjoint)[grep("^rating.+2", names(wide_conjoint))]
     )
    )
    # task variables
    list2 <- list(choice = paste0("choice_", letters[1:4]),
                  timing = paste0("timing_", letters[1:4]))
    
    long <- cj_tidy(wide_conjoint, profile_variables = list1, task_variables = list2, id = ~ respondent)
    expect_true(inherits(long, "data.frame"),
                label = "cj_tidy() returns a data frame")
    expect_true(nrow(long) == 800L,
                label = "cj_tidy() returns correct number of rows")
    expect_true(all(c("feature1", "feature2", "feature3", "rating", "choice", "timing",
                      "respondent", "task", "profile", "pair", "covariate1", "covariate2") %in% names(long)),
                label = "cj_tidy() returns correct columns")
    expect_true(all(long$task %in% 1L:100L),
                label = "cj_tidy() returns correct task identifiers")
    expect_true(all(long$profile %in% c("A", "B")),
                label = "cj_tidy() returns correct profile identifiers")
    expect_true(all(table(long$respondent) == 8L),
                label = "cj_tidy() returns correct number of respondent rows")
})

test_that("cj_tidy() works in formula interface", {
    # profile_variables
    list1 <- list(
     feature1 = list(
         ~ feature1a1 + feature1b1 + feature1c1 + feature1d1,
         ~ feature1a2 + feature1b2 + feature1c2 + feature1d2
     ),
     feature2 = list(
         ~ feature2a1 + feature2b1 + feature2c1 + feature2d1,
         ~ feature2a2 + feature2b2 + feature2c2 + feature2d2
     ),
     feature3 = list(
         ~ feature3a1 + feature3b1 + feature3c1 + feature3d1,
         ~ feature3a2 + feature3b2 + feature3c2 + feature3d2
     ),
     rating = list(
         ~ rating_a1 + rating_b1 + rating_c1 + rating_d1,
         ~ rating_a2 + rating_b2 + rating_c2 + rating_d2
     )
    )
    # task variables
    list2 <- list(choice = ~ choice_a + choice_b + choice_c + choice_d,
                  timing = ~ timing_a + timing_b + timing_c + timing_d)
    
    long <- cj_tidy(wide_conjoint, profile_variables = list1, task_variables = list2, id = ~ respondent)
    expect_true(inherits(long, "data.frame"),
                label = "cj_tidy() returns a data frame")
    expect_true(nrow(long) == 800L,
                label = "cj_tidy() returns correct number of rows")
    expect_true(all(c("feature1", "feature2", "feature3", "rating", "choice", "timing",
                      "respondent", "task", "profile", "pair", "covariate1", "covariate2") %in% names(long)),
                label = "cj_tidy() returns correct columns")
    expect_true(all(long$task %in% 1L:100L),
                label = "cj_tidy() returns correct task identifiers")
    expect_true(all(long$profile %in% c("A", "B")),
                label = "cj_tidy() returns correct profile identifiers")
    expect_true(all(table(long$respondent) == 8L),
                label = "cj_tidy() returns correct number of respondent rows")
})
