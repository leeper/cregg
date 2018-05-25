context("Correct data structures returned")

data(hainmueller, package = "cregg")
hainmueller$wts <- stats::rbeta(nrow(hainmueller), 2, 5)*5

test_that("cj() works", {
    expect_true(inherits(x <- cj(hainmueller, ChosenImmigrant ~ LanguageSkills, estimate = "mm", id = ~ CaseID), "cj_mm"),
                label = "cj() works w/o 'by'")
    expect_true(inherits(x <- cj(hainmueller, ChosenImmigrant ~ LanguageSkills, estimate = "mm", id = ~ CaseID, by = ~ Gender), "cj_mm"),
                label = "cj() works w/ 'by'")
    expect_true(!identical(cj(hainmueller, ChosenImmigrant ~ LanguageSkills, estimate = "mm", id = ~ CaseID, by = ~ Gender, level_order = "ascending")$level,
                           cj(hainmueller, ChosenImmigrant ~ LanguageSkills, estimate = "mm", id = ~ CaseID, by = ~ Gender, level_order = "descending")$level),
                label = "cj() respects 'level_order'")
    expect_true(identical(levels(cj(hainmueller, ChosenImmigrant ~ Education + Gender, id = ~CaseID, feature_order = c("Gender", "Education"), estimate = "mm")$feature),
                          c("Gender", "Educational Attainment")),
                label = "cj() respects 'feature_order'")
    expect_error(cj(hainmueller, ChosenImmigrant ~ Education + Gender, id = ~CaseID, feature_order = "Education", estimate = "mm"),
                   label = "cj() fails for missing feature names in 'feature_order'")
    expect_error(cj(hainmueller, ChosenImmigrant ~ Education + Gender, id = ~CaseID, feature_order = c("Education", "Gender", "foo"), estimate = "mm"),
                   label = "cj() fails for too many feature names in 'feature_order'")
    expect_error(inherits(x <- cj(hainmueller, ChosenImmigrant ~ LanguageSkills, estimate = "diff", id = ~ CaseID), "cj_mm"),
                 label = "cj() fails on estimate = 'diff' w/o 'by'")
})

test_that("amce() works", {
    expect_true(inherits(x <- amce(hainmueller, ChosenImmigrant ~ Gender, id = ~ CaseID), "cj_amce"))
    expect_true(inherits(plot(x), "ggplot"))
    expect_true(inherits(amce(hainmueller, ChosenImmigrant ~ LanguageSkills, id = ~ CaseID, weights = ~ wts), "cj_amce"),
                label = "amce() works w/ 'weights'")
    expect_error(amce(hainmueller,  ~ LanguageSkills, id = ~ CaseID), label = "amce() fails w/o LHS variable in formula")
})

test_that("amce() works", {
    x1 <- amce(hainmueller, ChosenImmigrant ~ Gender + LanguageSkills, id = ~ CaseID)
    x2 <- amce(hainmueller, ChosenImmigrant ~ Gender * LanguageSkills, id = ~ CaseID)
    expect_true(!identical(x1, x2), label = "amce() respects model specification")
})

test_that("amce_diffs() works", {
    expect_true(inherits(x <- amce_diffs(hainmueller, ChosenImmigrant ~ LanguageSkills, by = ~ Gender, id = ~ CaseID), "cj_diffs"))
    expect_true(inherits(plot(x), "ggplot"))
    expect_true(inherits(amce_diffs(hainmueller, ChosenImmigrant ~ LanguageSkills, id = ~ CaseID, by = ~ Gender, weights = ~ wts), "cj_diffs"),
                label = "amce_diffs() works w/ 'weights'")
    expect_error(amce(hainmueller,  ~ LanguageSkills, id = ~ CaseID), label = "amce_diffs() fails w/o LHS variable in formula")
})

test_that("amce_by_reference() works", {
    expect_true(inherits(x <- amce_by_reference(hainmueller, ChosenImmigrant ~ LanguageSkills + Gender, id = ~ CaseID, variable = ~Gender), "cj_amce"))
    expect_true(inherits(plot(x), "ggplot"))
    expect_true(inherits(amce_by_reference(hainmueller, ChosenImmigrant ~ LanguageSkills, id = ~ CaseID, variable = ~ Gender, weights = ~ wts), "cj_amce"),
                label = "amce_by_reference() works w/ 'weights'")
    expect_error(amce_by_reference(hainmueller,  ~ LanguageSkills, variable = ~ Gender, id = ~ CaseID), label = "amce_by_reference() fails w/o LHS variable in formula")
    expect_error(amce_by_reference(hainmueller,  ~ LanguageSkills, id = ~ CaseID), label = "amce_by_reference() fails w/o 'variable'")
})

test_that("mm() works", {
    expect_true(inherits(x <- mm(hainmueller, ChosenImmigrant ~ Gender, id = ~ CaseID), "cj_mm"))
    expect_true(inherits(plot(x), "ggplot"))
    expect_true(inherits(mm(hainmueller, ChosenImmigrant ~ LanguageSkills, id = ~ CaseID, weights = ~ wts), "cj_mm"),
                label = "mm() works w/ 'weights'")
    expect_error(mm(hainmueller,  ~ LanguageSkills, id = ~ CaseID), label = "mm() fails w/o LHS variable in formula")
})

test_that("mm_diffs() works", {
    expect_true(inherits(x <- mm_diffs(hainmueller, ChosenImmigrant ~ LanguageSkills, by = ~ Gender, id = ~ CaseID), "cj_diffs"))
    expect_true(inherits(plot(x), "ggplot"))
    expect_true(inherits(mm_diffs(hainmueller, ChosenImmigrant ~ LanguageSkills, by = ~ Gender, id = ~ CaseID, weights = ~ wts), "cj_diffs"),
                label = "mm_diffs() works w/ 'weights'")
    expect_error(mm_diffs(hainmueller,  ~ LanguageSkills, id = ~ CaseID), label = "mm_diffs() fails w/o 'by'")
    expect_error(mm_diffs(hainmueller,  ~ LanguageSkills, by = ~ Gender, id = ~ CaseID), label = "mm_diffs() fails w/o LHS variable in formula")
})

test_that("freqs() works", {
    expect_true(inherits(x <- freqs(hainmueller, ~ Gender, id = ~ CaseID), "cj_freqs"))
    expect_true(inherits(plot(x), "ggplot"))
    expect_true(inherits(freqs(hainmueller, ~ Gender, id = ~ CaseID, weights = ~ wts), "cj_freqs"), label = "freqs() works w/ 'weights'")
})

test_that("props() works", {
    expect_true(inherits(x <- props(hainmueller, ~ Gender, id = ~ CaseID), "cj_props"))
    expect_true(inherits(props(hainmueller, ~ Gender, id = ~ CaseID, weights = ~ wts), "cj_props"), label = "props() works w/ 'weights'")
})

test_that("cj_anova() works", {
    expect_true(inherits(cj_anova(hainmueller, ChosenImmigrant ~ Education, id = ~CaseID, by = ~Gender), "anova"), label = "cj_anova() works")
    #expect_true(inherits(cj_anova(hainmueller, ChosenImmigrant ~ Education, id = ~CaseID, by = ~Gender, weights = ~wts), "anova"), label = "cj_anova() works w/ 'weights'")
    expect_error(cj_anova(hainmueller,  ~ Education, id = ~ CaseID, by = ~Gender), label = "cj_anova() fails w/o LHS variable in formula")
})
