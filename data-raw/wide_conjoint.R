# script to produce the 'wide_conjoint' example dataset

# set seed for consistency
set.seed(19591119)

# dataset has 100 observations
N <- 100L

# create data
wide_conjoint <- data.frame(
  # identifier
  respondent = seq_len(N),
  # features
  ## Feature 1 - Task 1
  feature1a1 = sample(paste0("Feature1_level", letters[1:4]), N, TRUE),
  feature1b1 = sample(paste0("Feature1_level", letters[1:4]), N, TRUE),
  feature1c1 = sample(paste0("Feature1_level", letters[1:4]), N, TRUE),
  feature1d1 = sample(paste0("Feature1_level", letters[1:4]), N, TRUE),
  feature1a2 = sample(paste0("Feature1_level", letters[1:4]), N, TRUE),
  feature1b2 = sample(paste0("Feature1_level", letters[1:4]), N, TRUE),
  feature1c2 = sample(paste0("Feature1_level", letters[1:4]), N, TRUE),
  feature1d2 = sample(paste0("Feature1_level", letters[1:4]), N, TRUE),
  ## Feature 2
  feature2a1 = sample(paste0("Feature2_level", letters[5:10]), N, TRUE),
  feature2b1 = sample(paste0("Feature2_level", letters[5:10]), N, TRUE),
  feature2c1 = sample(paste0("Feature2_level", letters[5:10]), N, TRUE),
  feature2d1 = sample(paste0("Feature2_level", letters[5:10]), N, TRUE),
  feature2a2 = sample(paste0("Feature2_level", letters[5:10]), N, TRUE),
  feature2b2 = sample(paste0("Feature2_level", letters[5:10]), N, TRUE),
  feature2c2 = sample(paste0("Feature2_level", letters[5:10]), N, TRUE),
  feature2d2 = sample(paste0("Feature2_level", letters[5:10]), N, TRUE),
  ## Feature 3
  feature3a1 = sample(paste0("Feature3_level", letters[11:14]), N, TRUE),
  feature3b1 = sample(paste0("Feature3_level", letters[11:14]), N, TRUE),
  feature3c1 = sample(paste0("Feature3_level", letters[11:14]), N, TRUE),
  feature3d1 = sample(paste0("Feature3_level", letters[11:14]), N, TRUE),
  feature3a2 = sample(paste0("Feature3_level", letters[11:14]), N, TRUE),
  feature3b2 = sample(paste0("Feature3_level", letters[11:14]), N, TRUE),
  feature3c2 = sample(paste0("Feature3_level", letters[11:14]), N, TRUE),
  feature3d2 = sample(paste0("Feature3_level", letters[11:14]), N, TRUE),
  # choice outcomes
  choice_a = rep(c(1,2), length.out = N)[sample(seq_len(N), N)],
  choice_b = rep(c(1,2), length.out = N)[sample(seq_len(N), N)],
  choice_c = rep(c(1,2), length.out = N)[sample(seq_len(N), N)],
  choice_d = rep(c(1,2), length.out = N)[sample(seq_len(N), N)],
  # rating outcomes
  rating_a1 = rep(1:7, length.out = N)[sample(seq_len(N), N)],
  rating_a2 = rep(1:7, length.out = N)[sample(seq_len(N), N)],
  rating_b1 = rep(1:7, length.out = N)[sample(seq_len(N), N)],
  rating_b2 = rep(1:7, length.out = N)[sample(seq_len(N), N)],
  rating_c1 = rep(1:7, length.out = N)[sample(seq_len(N), N)],
  rating_c2 = rep(1:7, length.out = N)[sample(seq_len(N), N)],
  rating_d1 = rep(1:7, length.out = N)[sample(seq_len(N), N)],
  rating_d2 = rep(1:7, length.out = N)[sample(seq_len(N), N)],
  # task timings
  timing_a = rbeta(N, 2, 5) * 10,
  timing_b = rbeta(N, 2, 5) * 10,
  timing_c = rbeta(N, 2, 5) * 10,
  timing_d = rbeta(N, 2, 5) * 10,
  # covariates
  covariate1 = runif(N, -1, 1),
  covariate2 = sample(1:2, size = N, TRUE)
)

# output
devtools::use_data(wide_conjoint, overwrite = TRUE)
