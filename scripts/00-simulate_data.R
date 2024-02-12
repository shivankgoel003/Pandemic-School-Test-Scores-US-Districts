#### Preamble ####
# Purpose: Simulates the state score and learning model for various states
# Author: Navya Hooda, Shivank Goel, Vanshika Vanshika
# Data: 06 Febuary 2024
# Contact: shivankg.goel@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have installed the tidyverse, haven and tidyr packages. 



#### Workspace setup ####

library(haven)
library(tidyverse)

#### Simulate data ####
set.seed(9) 

start_date <- as.Date("2020-08-01")
end_date <- as.Date("2021-01-31")

simulated_dates <- seq(from = start_date, to = end_date, by = "month")

state_data <- tibble(
  StateName = c("Colorado", "Connecticut", "Massachusetts", "Minnesota", "Mississippi", "Ohio", 
                "Rhode Island", "Virginia", "West Virginia", "Wisconsin", "Wyoming"),
  StateAbbrev = c("CO", "CT", "MA", "MN", "MS", "OH", 
                  "RI", "VA", "WV", "WI", "WY")
)

state_abbrev <- state_data$StateAbbrev
names(state_abbrev) <- state_data$StateName

staff_count_range <- c(10, 100)
enrollment_range <- c(100, 5000)

# Sample state names for  dataset
sampled_state_names <- sample(state_data$StateName, length(simulated_dates), replace = TRUE)

# Simulate data for all states
state_sim <- tibble(
  StateName = sampled_state_names,
  StateAbbrev = state_abbrev[sampled_state_names], # Correct abbreviations
  DataLevel = "District",
  SchoolType = sample(c("Regular local school district"), length(simulated_dates), replace = TRUE),
  EnrollmentTotal = sample(1000:4000, length(simulated_dates), replace = TRUE),
  LearningModel = sample(c("In-person", "Hybrid", "Virtual"), length(simulated_dates), replace = TRUE),
  TimePeriodStart = simulated_dates,
  TimePeriodEnd = simulated_dates + 29 # Assuming each period is a month long
)
print(state_sim)

# Simulation of summary statistics
summary_stats <- state_sim %>%
  group_by(StateName) %>%
  summarise(
    AvgEnrollmentTotal = mean(EnrollmentTotal),
    EnrollmentTotalSD = sd(EnrollmentTotal),
    CountInPerson = sum(LearningModel == "In-person"),
    CountHybrid = sum(LearningModel == "Hybrid"),
    CountVirtual = sum(LearningModel == "Virtual"),
    PeriodsCount = n()
  ) %>%
  ungroup()

summary_stats <- state_sim %>%
  group_by(StateName) %>%
  summarise(
    AvgEnrollmentTotal = mean(EnrollmentTotal),
    CountInPerson = sum(LearningModel == "In-person"),
    CountHybrid = sum(LearningModel == "Hybrid"),
    CountVirtual = sum(LearningModel == "Virtual"),
    PeriodsCount = n()
  ) %>%
  ungroup()

print(summary_stats)

# Plotting the average total enrollment for each state
ggplot(summary_stats, aes(x = StateName, y = AvgEnrollmentTotal, fill = StateName)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(
    title = "Average Total Enrollment by State",
    x = "State",
    y = "Average Total Enrollment",
    fill = "State"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


