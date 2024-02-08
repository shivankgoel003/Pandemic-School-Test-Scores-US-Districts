#### Preamble ####
# Purpose: Cleans the raw plane data recorded by two observers..... [...UPDATE THIS...]
# Author: Navya h
# Date: 6 April 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]

#### Workspace setup ####
library(tidyverse)
library(haven)
library(janitor)
library(readxl)
library(dplyr)
library(readxl)

#reading data

#Colardo
colardo = read_excel("inputs/raw data/learningmodel/Colorado_Districts_LearningModelData_Final.xlsx")
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
nces_district = clean_names(nces_district)
nces_school = clean_names(nces_school)
nces_district_grade = clean_names(nces_district_grade)
colardo = clean_names(colardo)
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

colardo = 
  colardo |>
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



#writing data
write_csv(nces_district, "outputs/data/cleaned_nces_district")
write_csv(nces_district_grade, "outputs/data/cleaned_nces_district_grade")
write_csv(nces_school, "outputs/data/cleaned_nces_district_school")
write_csv(colardo, "outputs/data/cleaned_learningmodel/colardo")
write_csv(connecticut, "outputs/data/cleaned_learningmodel/connecticut")
write_csv(ohio, "outputs/data/cleaned_learningmodel/ohio")
write_csv(massachusets, "outputs/data/cleaned_learningmodel/massachusets")
write_csv(mississippi, "outputs/data/cleaned_learningmodel/mississippi")
write_csv(minnesota, "outputs/data/cleaned_learningmodel/minnesota")
write_csv(rhode, "outputs/data/cleaned_learningmodel/rhode")
write_csv(virginia, "outputs/data/cleaned_learningmodel/virginia")
write_csv(westvirginia, "outputs/data/cleaned_learningmodel/westvirginia")
write_csv(winscosin, "outputs/data/cleaned_learningmodel/winscosin")
write_csv(wyoming, "outputs/data/cleaned_learningmodel/wyoming")

