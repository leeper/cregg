context("Correct data structures returned")

data(immigration, package = "cregg")
immigration$wts <- stats::rbeta(nrow(immigration), 2, 5)*5
immigration$nonfactor = rep(letters[1:2], each = nrow(immigration)/2L)

test_that("cj() works", {
    expect_true(inherits(x <- cj(immigration, ChosenImmigrant ~ LanguageSkills, estimate = "mm", id = ~ CaseID), "cj_mm"),
                label = "cj() works w/o 'by'")
    expect_true(inherits(plot(x), "ggplot"), label = "plot.cj_amce() works w/o 'by' argument")
    expect_true(inherits(x <- cj(immigration, ChosenImmigrant ~ LanguageSkills, estimate = "mm", id = ~ CaseID, by = ~ Gender), "cj_mm"),
                label = "cj() works w/ 'by'")
    expect_true(inherits(plot(x, group = "BY"), "ggplot"), label = "plot.cj_amce() works w/ 'by' argument")
    expect_true(!identical(cj(immigration, ChosenImmigrant ~ LanguageSkills, estimate = "mm", id = ~ CaseID,
                              by = ~ Gender, level_order = "ascending")$level,
                           cj(immigration, ChosenImmigrant ~ LanguageSkills, estimate = "mm", id = ~ CaseID,
                              by = ~ Gender, level_order = "descending")$level),
                label = "cj() respects 'level_order'")
    expect_true(identical(levels(cj(immigration, ChosenImmigrant ~ Education + Gender, id = ~CaseID,
                                    feature_order = c("Gender", "Education"), estimate = "mm")$feature),
                          c("Gender", "Educational Attainment")),
                label = "cj() respects 'feature_order'")
    
    # expected warnings
    dat <- data.frame(
        y = rnorm(100),
        a = factor(rep(1:2, 50), labels = c("a", "b")),
        b = factor(rep(c(1:2, 1:2), each = 25), labels = c("a", "b"))
    )
    expect_warning(expect_error(cj(dat, y ~ a + b, label = "cj() fails for duplicate levels across features")))
    # expected errors
    expect_error(cj(immigration, ChosenImmigrant ~ Education + Gender, id = ~CaseID, feature_order = "Education", estimate = "mm"),
                 label = "cj() fails for missing feature names in 'feature_order'")
    expect_error(cj(immigration, ChosenImmigrant ~ Education + Gender, id = ~CaseID, feature_order = c("Education", "Gender", "foo"), estimate = "mm"),
                 label = "cj() fails for too many feature names in 'feature_order'")
    expect_error(cj(immigration, ChosenImmigrant ~ Education + Gender, id = ~CaseID, feature_order = c("Education", "foo"), estimate = "mm"),
                 label = "cj() fails for wrong feature names in 'feature_order'")
    expect_error(inherits(x <- cj(immigration, ChosenImmigrant ~ LanguageSkills, estimate = "diff", id = ~ CaseID), "cj_mm"),
                 label = "cj() fails on estimate = 'diff' w/o 'by'")
    immigration$tmp <- as.character(immigration$Gender)
    expect_error(cj(immigration, ChosenImmigrant ~ tmp, id = ~CaseID, estimate = "mm"),
                 label = "cj() fails when features aren't factors (character)")
    immigration$tmp <- as.numeric(immigration$Gender)
    expect_error(cj(immigration, ChosenImmigrant ~ tmp, id = ~CaseID, estimate = "mm"),
                 label = "cj() fails when features aren't factors (numeric)")
    expect_error(cj(immigration, ChosenImmigrant ~ tmp, id = ~CaseID, estimate = "mm", by = ~ nonfactor),
                 label = "cj() fails when by isn't a factor")
    immigration$tmp <- NULL
})

test_that("amce() works", {
    expect_true(inherits(x <- amce(immigration, ChosenImmigrant ~ Gender, id = ~ CaseID), "cj_amce"))
    expect_true(inherits(plot(x), "ggplot"))
    expect_true(inherits(amce(immigration, ChosenImmigrant ~ LanguageSkills, id = ~ CaseID, weights = ~ wts), "cj_amce"),
                label = "amce() works w/ 'weights'")
    expect_error(amce(immigration,  ~ LanguageSkills, id = ~ CaseID),
                 label = "amce() fails w/o LHS variable in formula")
})

test_that("amce() respects model specification for constraints", {
    x1 <- amce(immigration, ChosenImmigrant ~ Gender + LanguageSkills, id = ~ CaseID)
    x2 <- amce(immigration, ChosenImmigrant ~ Gender * LanguageSkills, id = ~ CaseID)
    expect_true(!identical(x1, x2), label = "amce() respects model specification")
})

test_that("amce_diffs() works", {
    expect_true(inherits(x <- amce_diffs(immigration, ChosenImmigrant ~ LanguageSkills, by = ~ Gender, id = ~ CaseID), "cj_diffs"))
    expect_true(inherits(plot(x), "ggplot"))
    expect_true(inherits(amce_diffs(immigration, ChosenImmigrant ~ LanguageSkills, id = ~ CaseID, by = ~ Gender, weights = ~ wts), "cj_diffs"),
                label = "amce_diffs() works w/ 'weights'")
    expect_error(amce(immigration,  ~ LanguageSkills, id = ~ CaseID),
                 label = "amce_diffs() fails w/o LHS variable in formula")
    expect_error(amce_diffs(immigration,  ~ LanguageSkills, by = ~ profile, id = ~ CaseID),
                 label = "amce_diffs() fails w/o factor 'by'")
})

test_that("amce_by_reference() works", {
    expect_true(inherits(x <- amce_by_reference(immigration, ChosenImmigrant ~ LanguageSkills + Gender, id = ~ CaseID, variable = ~Gender), "cj_amce"))
    expect_true(inherits(plot(x), "ggplot"))
    expect_true(inherits(amce_by_reference(immigration, ChosenImmigrant ~ LanguageSkills, id = ~ CaseID, variable = ~ Gender, weights = ~ wts), "cj_amce"),
                label = "amce_by_reference() works w/ 'weights'")
    expect_error(amce_by_reference(immigration,  ~ LanguageSkills, variable = ~ Gender, id = ~ CaseID),
                 label = "amce_by_reference() fails w/o LHS variable in formula")
    expect_error(amce_by_reference(immigration,  ~ LanguageSkills, id = ~ CaseID),
                 label = "amce_by_reference() fails w/o 'variable'")
    expect_error(amce_by_reference(immigration,  ~ LanguageSkills, variable = ~ profile, id = ~ CaseID),
                 label = "amce_by_reference() fails w/o factor 'variable'")
})

test_that("mm() works", {
    expect_true(inherits(x <- mm(immigration, ChosenImmigrant ~ Gender, id = ~ CaseID), "cj_mm"))
    expect_true(inherits(plot(x), "ggplot"))
    expect_true(inherits(mm(immigration, ChosenImmigrant ~ LanguageSkills, id = ~ CaseID, weights = ~ wts), "cj_mm"),
                label = "mm() works w/ 'weights'")
    expect_error(mm(immigration,  ~ LanguageSkills, id = ~ CaseID), label = "mm() fails w/o LHS variable in formula")
})

test_that("mm_diffs() works", {
    expect_true(inherits(x <- mm_diffs(immigration, ChosenImmigrant ~ LanguageSkills, by = ~ Gender, id = ~ CaseID), "cj_diffs"))
    expect_true(inherits(plot(x), "ggplot"))
    expect_true(inherits(mm_diffs(immigration, ChosenImmigrant ~ LanguageSkills, by = ~ Gender, id = ~ CaseID, weights = ~ wts), "cj_diffs"),
                label = "mm_diffs() works w/ 'weights'")
    expect_error(mm_diffs(immigration,  ~ LanguageSkills, id = ~ CaseID),
                 label = "mm_diffs() fails w/o 'by'")
    expect_error(mm_diffs(immigration,  ~ LanguageSkills, by = ~ Gender, id = ~ CaseID),
                 label = "mm_diffs() fails w/o LHS variable in formula")
    expect_error(mm_diffs(immigration,  ~ LanguageSkills, by = ~ profile, id = ~ CaseID),
                 label = "mm_diffs() fails w/o factor 'by'")
})

test_that("cj_table() works", {
    expect_true(inherits(cj_table(immigration, ~ Gender + Education), "data.frame"))
    expect_true("reference" %in% names(cj_table(immigration, ~ Gender + Education, include_reference = TRUE)))
})

test_that("cj_freqs() works", {
    expect_true(inherits(x <- cj_freqs(immigration, ~ Gender, id = ~ CaseID), "cj_freqs"))
    expect_true(inherits(plot(x), "ggplot"))
    expect_true(inherits(cj_freqs(immigration, ~ Gender, id = ~ CaseID, weights = ~ wts), "cj_freqs"),
                label = "freqs() works w/ 'weights'")
})

test_that("cj_props() works", {
    expect_true(inherits(x <- cj_props(immigration, ~ Gender, id = ~ CaseID), "cj_props"))
    expect_true(inherits(cj_props(immigration, ~ Gender, id = ~ CaseID, weights = ~ wts), "cj_props"),
                label = "props() works w/ 'weights'")
})

test_that("cj_anova() works", {
    expect_true(inherits(cj_anova(immigration, ChosenImmigrant ~ Education, id = ~CaseID, by = ~Gender), "anova"),
                label = "cj_anova() works")
    # cj_anova() currently doesn't work with 'weights' due to issues in **survey**
    #expect_true(inherits(cj_anova(immigration, ChosenImmigrant ~ Education, id = ~CaseID, by = ~Gender, weights = ~wts), "anova"),
    #                     label = "cj_anova() works w/ 'weights'")
    expect_error(cj_anova(immigration, ChosenImmigrant ~ Education, id = ~CaseID, by = ~Gender, weights = ~wts),
                 label = "cj_anova() fails w/ 'weights', as expected at the moment")
    expect_error(cj_anova(immigration,  ~ Education, id = ~ CaseID, by = ~Gender),
                 label = "cj_anova() fails w/o LHS variable in formula")
})
