context("Correct data structures returned")

data(hainmueller, package = "cregg")

test_that("cj() works", {
    expect_true(inherits(x <- cj(hainmueller, ChosenImmigrant ~ LanguageSkills, estimate = "mm", id = ~ CaseID), "cj_mm"),
                label = "cj() works w/o 'by'")
    expect_true(inherits(x <- cj(hainmueller, ChosenImmigrant ~ LanguageSkills, estimate = "mm", id = ~ CaseID, by = ~ Gender), "cj_mm"),
                label = "cj() works w/ 'by'")
    expect_error(inherits(x <- cj(hainmueller, ChosenImmigrant ~ LanguageSkills, estimate = "diff", id = ~ CaseID), "cj_mm"),
                 label = "cj() fails on estimate = 'diff' w/o 'by'")
})

test_that("amce() works", {
    expect_true(inherits(x <- amce(hainmueller, ChosenImmigrant ~ Gender, id = ~ CaseID), "cj_amce"))
    expect_true(inherits(plot(x), "ggplot"))
})

test_that("amce_diffs() works", {
    expect_true(inherits(x <- amce_diffs(hainmueller, ChosenImmigrant ~ LanguageSkills, by = ~ Gender, id = ~ CaseID), "cj_diffs"))
    expect_true(inherits(plot(x), "ggplot"))
})

test_that("amce_by_reference() works", {
    expect_true(inherits(x <- amce_by_reference(hainmueller, ChosenImmigrant ~ LanguageSkills + Gender, id = ~ CaseID, variable = ~Gender), "cj_amce"))
    expect_true(inherits(plot(x), "ggplot"))
})

test_that("mm() works", {
    expect_true(inherits(x <- mm(hainmueller, ChosenImmigrant ~ Gender, id = ~ CaseID), "cj_mm"))
    expect_true(inherits(plot(x), "ggplot"))
})

test_that("freqs() works", {
    expect_true(inherits(x <- freqs(hainmueller, ChosenImmigrant ~ Gender, id = ~ CaseID), "cj_freqs"))
    expect_true(inherits(plot(x), "ggplot"))
})

test_that("props() works", {
    expect_true(inherits(x <- props(hainmueller, ChosenImmigrant ~ Gender, id = ~ CaseID), "cj_props"))
})

test_that("cj_anova() works", {
    expect_true(inherits(cj_anova(hainmueller, ChosenImmigrant ~ Education, id = ~CaseID, by = ~Gender), "anova"))
})
