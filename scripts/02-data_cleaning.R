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
  select(state, year, share_inperson, share_virtual, share_hybrid, participation, pass,
         case_rate_per100k_zip,enrollment_total)

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



#writing data

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

