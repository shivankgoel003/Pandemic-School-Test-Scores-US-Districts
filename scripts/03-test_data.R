#### Preamble ####
# Purpose: Data Validation and Testing
# Author: Navya Gupta, Shivank Goel, Vanshika Vanshika
# Date: 12th February 2024
# Contact: shivankg.goel@mail.utoronto.ca
# License: MIT
# Pre-requisites: 

#### Workspace setup ####
library(tidyverse)
library(testthat)

#### Test data ####


#Unit Testing Reference:https://testthat.r-lib.org/reference/test_that.html 


#Testing data types of column names in cleaned data
class(cleaned_state_scoredata$year) == "numeric"
class(cleaned_state_scoredata$share_inperson) == "numeric"
class(cleaned_state_scoredata$share_virtual) == "numeric"
class(cleaned_state_scoredata$share_hybrid) == "numeric"
class(cleaned_state_scoredata$participation) == "numeric"
class(cleaned_state_scoredata$pass) == "numeric"
class(cleaned_state_scoredata$max_cases) == "numeric"
class(cleaned_state_scoredata$cases_aug) == "numeric"
class(cleaned_state_scoredata$case_rate_per100k_zip) == "numeric"

class(cleaned_state_scoredata$state) == "character"
class(colorado$learning_model_gr_k5) == "character"
class(colorado$learning_model_gr68) == "character"
class(colorado$learning_model_gr912) == "character"
class(colorado$learning_model) == "character"

class(connecticut$learning_model_gr_k5) == "character"
class(connecticut$learning_model_gr68) == "character"
class(connecticut$learning_model_gr912) == "character"
class(connecticut$learning_model) == "character"

# Testing if all Enrollment Total values are positive and within the expected range
test_that("EnrollmentTotal values are within the expected range", {
  expect_true(all(state_sim$EnrollmentTotal > 0))
  expect_true(all(state_sim$EnrollmentTotal >= min(enrollment_range)))
  expect_true(all(state_sim$EnrollmentTotal <= max(enrollment_range)))
})

# Testing if Learning Model values are one of the expected categories
test_that("LearningModel values are valid", {
  valid_learning_models <- c("In-person", "Hybrid", "Virtual")
  expect_true(all(state_sim$LearningModel %in% valid_learning_models))
})

# Testing if the dates are within the expected range
test_that("Date range is valid", {
  expect_true(all(state_sim$TimePeriodStart >= start_date))
  expect_true(all(state_sim$TimePeriodEnd <= end_date))
  expect_true(all(state_sim$TimePeriodEnd > state_sim$TimePeriodStart))
})
