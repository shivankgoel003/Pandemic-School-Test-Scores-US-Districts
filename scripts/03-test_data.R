#### Preamble ####
# Purpose: Tests... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 11 February 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
library(tidyverse)
library(testthat)

#### Test data ####


#Unit Testing Reference:https://testthat.r-lib.org/reference/test_that.html 


#Testing data types
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
class(colorado$learning_model) == "character"
class(colorado$learning_model) == "character"
