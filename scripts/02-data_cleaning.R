#### Preamble ####
# Purpose: Cleans the raw data for learning model and test scores for 11 US states
# Author: Navya Hooda, Shivank Goel, Vanshika Vanshika
# Date: 07 February 2024
# Contact: shivankg.goel@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(tidyverse)
library(haven)
library(janitor)
library(readxl)
library(dplyr)
library(readxl)

#reading data
statescoredata = read_dta("inputs/raw data/state_score_data.dta")
#schooling mode data
schoolmode = read_dta("inputs/raw data/schooling_mode_data.dta")
#Colorado
colorado = read_excel("inputs/raw data/learningmodel/Colorado_Districts_LearningModelData_Final.xlsx")
#Connecticut
connecticut = read_excel("inputs/raw data/learningmodel/Connecticut_Districts_LearningModelData_Final.xlsx")
#Massachusets
massachusets = read_excel("inputs/raw data/learningmodel/Massachusetts_Districts_LearningModelData_Final.xlsx")
#Minnesota 
minnesota = read_excel("inputs/raw data/learningmodel/Minnesota_Districts_LearningModelData_Final.xlsx")
#Mississippi
mississippi = read_excel("inputs/raw data/learningmodel/Mississippi_Schools_LearningModelData_Final.xlsx")
#Ohio
ohio = read_excel("inputs/raw data/learningmodel/Ohio_Districts_LearningModelData_Final.xlsx")
#Rhode Island
rhode = read_excel("inputs/raw data/learningmodel/RhodeIsland_Schools_LearningModelData_Final.xlsx")
#Virginia
virginia = read_excel("inputs/raw data/learningmodel/Virginia_Districts_LearningModelData_Final.xlsx")
#West Virginia
westvirginia = read_excel("inputs/raw data/learningmodel/WestVirginia_Districts_LearningModelData_Final.xlsx")
#Winscosin
winscosin = read_excel("inputs/raw data/learningmodel/Wisconsin_Schools_LearningModelData_Final.xlsx")
#Wyoming
wyoming = read_excel("inputs/raw data/learningmodel/Wyoming_Districts_LearningModelData_Final.xlsx")




#cleaning


# Simplified names
statescoredata = clean_names(statescoredata)
statescoredata1 = clean_names(statescoredata)
colorado = clean_names(colorado)
connecticut = clean_names(connecticut)
ohio = clean_names(ohio)
virginia = clean_names(virginia)
westvirginia = clean_names(westvirginia)
wyoming = clean_names(wyoming)
mississippi = clean_names(mississippi)
rhode = clean_names(rhode)
minnesota = clean_names(minnesota)
massachusets = clean_names(massachusets)
winscosin = clean_names(winscosin)

#columns of interest

statescoredata =  statescoredata |>
  select(state, year, share_inperson, share_virtual, share_hybrid, participation, pass)
statescoredata_ohio = statescoredata1 |>
  select(state, year, share_inperson, share_virtual, share_hybrid, participation,
         pass, cts_pass_below, cts_pass_proficient, cts_pass_advanced, unemployment) |>
  filter(year == "2019")

statescoredata_3 = statescoredata1 |>
   filter(year == "2019" & state == "RI" )


colorado = 
  colorado |>
  select(
    learning_model, learning_model_gr_k5, 
    learning_model_gr68, learning_model_gr912, time_period_start, time_period_end,
    enrollment_total,enrollment_in_person, enrollment_hybrid, enrollment_virtual,
    staff_count, staff_count_in_person)

connecticut = 
  connecticut |>
  select(
    learning_model, learning_model_gr_k5, 
    learning_model_gr68, learning_model_gr912, time_period_start, time_period_end,
    enrollment_total,enrollment_in_person, enrollment_hybrid, enrollment_virtual,
    staff_count, staff_count_in_person)

massachusets = 
  massachusets |>
  select(
    learning_model, learning_model_gr_k5, 
    learning_model_gr68, learning_model_gr912, time_period_start, time_period_end,
    enrollment_total,enrollment_in_person, enrollment_hybrid, enrollment_virtual,
    staff_count, staff_count_in_person)


ohio = 
  ohio |>
  select(
    learning_model, learning_model_gr_k5, 
    learning_model_gr68, learning_model_gr912, time_period_start, time_period_end,
    enrollment_total,enrollment_in_person, enrollment_hybrid, enrollment_virtual,
    staff_count, staff_count_in_person)

virginia = 
  virginia |>
  select(
    learning_model, learning_model_gr_k5, 
    learning_model_gr68, learning_model_gr912, time_period_start, time_period_end,
    enrollment_total,enrollment_in_person, enrollment_hybrid, enrollment_virtual,
    staff_count, staff_count_in_person)


westvirginia = 
  westvirginia |>
  select(
    learning_model, learning_model_gr_k5, 
    learning_model_gr68, learning_model_gr912, time_period_start, time_period_end,
    enrollment_total,enrollment_in_person, enrollment_hybrid, enrollment_virtual,
    staff_count, staff_count_in_person)

wyoming = 
  wyoming |>
  select(
    learning_model, learning_model_gr_k5, 
    learning_model_gr68, learning_model_gr912, time_period_start, time_period_end,
    enrollment_total,enrollment_in_person, enrollment_hybrid, enrollment_virtual,
    staff_count, staff_count_in_person)

mississippi = 
  mississippi |>
  select(
    learning_model, learning_model_gr_k5, 
    learning_model_gr68, learning_model_gr912, time_period_start, time_period_end,
    enrollment_total,enrollment_in_person, enrollment_hybrid, enrollment_virtual,
    staff_count, staff_count_in_person)

rhode = 
  rhode |>
  select(
    learning_model, learning_model_gr_k5, 
    learning_model_gr68, learning_model_gr912, time_period_start, time_period_end,
    enrollment_total,enrollment_in_person, enrollment_hybrid, enrollment_virtual,
    staff_count, staff_count_in_person)

winscosin = 
  winscosin |>
  select(
    learning_model, learning_model_gr_k5, 
    learning_model_gr68, learning_model_gr912, time_period_start, time_period_end,
    enrollment_total,enrollment_in_person, enrollment_hybrid, enrollment_virtual,
    staff_count, staff_count_in_person)

minnesota = 
  minnesota |>
  select(
    learning_model, learning_model_gr_k5, 
    learning_model_gr68, learning_model_gr912, time_period_start, time_period_end,
    enrollment_total,enrollment_in_person, enrollment_hybrid, enrollment_virtual,
    staff_count, staff_count_in_person)

#removing repeated categories
colorado <- colorado %>%
  mutate(learning_model = case_when(
    learning_model == "In-person" ~ "In-Person",
    learning_model == "In-Person" ~ "In-Person",
    TRUE ~ learning_model
  ))
connecticut <- connecticut %>%
  mutate(learning_model = case_when(
    learning_model == "In-person" ~ "In-Person",
    learning_model == "In-Person" ~ "In-Person",
    TRUE ~ learning_model
  ))
massachusets <- massachusets %>%
  mutate(learning_model = case_when(
    learning_model == "In-person" ~ "In-Person",
    learning_model == "In-Person" ~ "In-Person",
    TRUE ~ learning_model
  ))
minnesota <- minnesota %>%
  mutate(learning_model = case_when(
    learning_model == "In-person" ~ "In-Person",
    learning_model == "In-Person" ~ "In-Person",
    TRUE ~ learning_model
  ))

mississippi <- mississippi %>%
  mutate(learning_model = case_when(
    learning_model == "In-person" ~ "In-Person",
    learning_model == "In-Person" ~ "In-Person",
    TRUE ~ learning_model
  ))


ohio <- ohio %>%
  mutate(learning_model = case_when(
    learning_model == "In-person" ~ "In-Person",
    learning_model == "In-Person" ~ "In-Person",
    TRUE ~ learning_model
  ))
rhode <- rhode %>%
  mutate(learning_model = case_when(
    learning_model == "In-person" ~ "In-Person",
    learning_model == "In-Person" ~ "In-Person",
    TRUE ~ learning_model
  ))
virginia <- virginia %>%
  mutate(learning_model = case_when(
    learning_model == "In-person" ~ "In-Person",
    learning_model == "In-Person" ~ "In-Person",
    TRUE ~ learning_model
  ))
virginia <- virginia %>%
  mutate(learning_model = case_when(
    learning_model == "In-person" ~ "In-Person",
    learning_model == "In-Person" ~ "In-Person",
    TRUE ~ learning_model
  ))

westvirginia <- westvirginia %>%
  mutate(learning_model = case_when(
    learning_model == "In-person" ~ "In-Person",
    learning_model == "In-Person" ~ "In-Person",
    TRUE ~ learning_model
  ))

winscosin <- winscosin %>%
  mutate(learning_model = case_when(
    learning_model == "In-person" ~ "In-Person",
    learning_model == "In-Person" ~ "In-Person",
    TRUE ~ learning_model
  ))
wyoming <- wyoming %>%
  mutate(learning_model = case_when(
    learning_model == "In-person" ~ "In-Person",
    learning_model == "In-Person" ~ "In-Person",
    TRUE ~ learning_model
  ))



# Assuming your data frame 
print("Original Data:")
print(cleaned_state_scoredata)

filtered_data <- cleaned_state_scoredata %>%
  filter(state == "OH", year == 2021) %>%
  {print("Filtered Data:"); print(.); .}

# Check for NA values in the pass column
if (any(is.na(filtered_data$pass))) {
  print("Warning: Rows with NA pass values found. Removing them.")
}

# Remove rows with NA pass values
filtered_data <- filtered_data %>%
  filter(!is.na(pass))

print("Filtered Data after removing NA pass values:")
print(filtered_data)

result <- filtered_data %>%
  summarise(average_pass = mean(pass)) 

print("Result:")
print(result)




# Filter the data for the year 2021 and group by states to get average pass rates 

filtered_data <- cleaned_state_scoredata %>%
  filter(year == 2021)

# Group the filtered data by state and calculate the average pass value for each state
avg_pass_by_state <- filtered_data %>%
  group_by(state) %>%
  summarise(average_pass = mean(pass, na.rm = TRUE)*100)

# Save the results to a new file 
head(avg_pass_by_state)
write_csv(avg_pass_by_state, "outputs/data/averages_by_state.csv") 


# filter staff count info for each state and summarize

# Initialize an empty data frame to store aggregated results
aggregated_data <- data.frame(state = character(), average_staff_count = numeric(), stringsAsFactors = FALSE)

# Loop through each state file
states <- c("outputs/data/cleaned_learningmodel/ohio.csv", "outputs/data/cleaned_learningmodel/colorado.csv", "outputs/data/cleaned_learningmodel/connecticut.csv", "outputs/data/cleaned_learningmodel/minnesota.csv", "outputs/data/cleaned_learningmodel/mississippi.csv", "outputs/data/cleaned_learningmodel/rhode.csv", "outputs/data/cleaned_learningmodel/massachusets.csv", "outputs/data/cleaned_learningmodel/virginia.csv", "outputs/data/cleaned_learningmodel/westvirginia.csv", "outputs/data/cleaned_learningmodel/winscosin.csv", "outputs/data/cleaned_learningmodel/wyoming.csv")  # Replace with your actual file names
for (state_file in states) {
  # Read data for the current state
  data <- read.csv(state_file)
  
  # Filter the data for the year 2021 based on the "time_period_end" column
  filtered_data <- data %>%
    filter(substr(time_period_end, 1, 4) == "2021")
  
  # Calculate the average "staff count" for the filtered data
  average_staff_count <- mean(filtered_data$staff_count, na.rm = TRUE)
  
  # Extract state name from file name (assuming the file name format is "stateX.csv")
  state_name <- gsub(".csv", "", state_file)
  
  # Append state name and average staff count to the aggregated data frame
  aggregated_data <- rbind(aggregated_data, data.frame(state = state_name, average_staff_count = average_staff_count))
}

library(dplyr)
library(readr)

# Function to clean and process data for each state
clean_and_process_state_data <- function(state_file) {
  # Read data for the current state
  data <- read_csv(state_file)
  
  # Filter the data for the year 2021 based on the "time_period_end" column
  filtered_data <- data %>%
    filter(substr(time_period_end, 1, 4) == "2021")
  
  # Filter the data for learning model values of 'virtual'
  filtered_data <- filtered_data %>%
    filter(learning_model == "Virtual")
  
  # Calculate students per teacher for each observation
  filtered_data$students_per_teacher <- filtered_data$enrollment_total / filtered_data$staff_count
  
  # Calculate the average students per teacher for the state
  average_students_per_teacher <- mean(filtered_data$students_per_teacher, na.rm = TRUE)
  
  # Extract state abbreviation from file name (assuming the file name format is "stateX.csv")
  state_abbr <- gsub(".csv", "", basename(state_file))
  
  # Return state abbreviation and average students per teacher for the state
  return(data.frame(state = state_abbr, average_students_per_teacher = average_students_per_teacher))
}

# Initialize an empty data frame to store results for all states
results <- data.frame(state = character(), average_students_per_teacher = numeric(), stringsAsFactors = FALSE)

# Loop through each state file
states <- c("outputs/data/cleaned_learningmodel/ohio.csv", "outputs/data/cleaned_learningmodel/colorado.csv", "outputs/data/cleaned_learningmodel/connecticut.csv", "outputs/data/cleaned_learningmodel/minnesota.csv", "outputs/data/cleaned_learningmodel/mississippi.csv", "outputs/data/cleaned_learningmodel/rhode.csv", "outputs/data/cleaned_learningmodel/massachusets.csv", "outputs/data/cleaned_learningmodel/virginia.csv", "outputs/data/cleaned_learningmodel/westvirginia.csv", "outputs/data/cleaned_learningmodel/winscosin.csv", "outputs/data/cleaned_learningmodel/wyoming.csv")  # Replace with your actual file names
for (state_file in states) {
  # Clean and process data for the current state
  state_result <- clean_and_process_state_data(state_file)
  
  # Append results for the current state to the overall results data frame
  results <- rbind(results, state_result)
}

# Read state abbreviations from a file or define them manually
state_abbreviations <- c("OH", "CO", "CT", "MN", "MS", "RI", "MA", "VA", "WV", "WI", "WY")

# Replace file paths with state abbreviations
results$state <- state_abbreviations

# Write the results to a CSV file
write_csv(results, "outputs/data/average_students_per_teacher_by_state.csv")

# merged data for teacher ratio to pass rate 
# Read the data from the two files
avg_pass_by_state <- read.csv("outputs/data/averages_by_state.csv")
avg_students_per_teacher_by_state <- read.csv("outputs/data/average_students_per_teacher_by_state.csv")

# Merge the two data frames by the common state ID
merged_data <- merge(avg_pass_by_state, avg_students_per_teacher_by_state, by = "state")

# Write the merged data to a CSV file
write.csv(merged_data, "outputs/data/merged_data_by_state.csv", row.names = FALSE)




# Write the results to a CSV file
write_csv(results, "outputs/data/average_students_per_teacher_by_state.csv") 

write_csv(statescoredata, "outputs/data/cleaned_state_scoredata.csv")
write_csv(schoolmode, "outputs/data/cleanedschoolmode.csv")
write_csv(colorado, "outputs/data/cleaned_learningmodel/colorado.csv")
write_csv(connecticut, "outputs/data/cleaned_learningmodel/connecticut.csv")
write_csv(ohio, "outputs/data/cleaned_learningmodel/ohio.csv")
write_csv(massachusets, "outputs/data/cleaned_learningmodel/massachusets.csv")
write_csv(mississippi, "outputs/data/cleaned_learningmodel/mississippi.csv")
write_csv(minnesota, "outputs/data/cleaned_learningmodel/minnesota.csv")
write_csv(rhode, "outputs/data/cleaned_learningmodel/rhode.csv")
write_csv(virginia, "outputs/data/cleaned_learningmodel/virginia.csv")
write_csv(westvirginia, "outputs/data/cleaned_learningmodel/westvirginia.csv")
write_csv(winscosin, "outputs/data/cleaned_learningmodel/winscosin.csv")
write_csv(wyoming, "outputs/data/cleaned_learningmodel/wyoming.csv")

write_csv(statescoredata1, "outputs/data/all_scoredata.csv")
write_csv(statescoredata_ohio, "outputs/data/score_ohio.csv")
write_csv(statescoredata_3, "outputs/data/score_plain.csv")
