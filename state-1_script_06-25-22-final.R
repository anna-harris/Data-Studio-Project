#install packages
library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)
library(chron)
library(fasttime)

#install.packages("chron")

#set working directory
setwd("~/Dropbox/My Mac (Anna’s MacBook Pro)/Desktop/Data Studio/New Jersey")

#read csv
nj_ehist_data <- read_csv("ehist_Atlantic.csv")

#clean_names
nj_ehist_data_clean_names <- clean_names(nj_ehist_data)

#select columns
nj_ehist_selected <- select(nj_ehist_data_clean_names, voter_id, voter_legacy_id, voter_status, 
                            voter_registration_date, voter_first, voter_last, voter_sex, 
                           voter_dob, voter_phone, res_city, res_state, res_zipcode, 
                            election_name, election_date, election_type
                            )

#check type and class of col object
#typeof(nj_ehist_selected$voter_registration_date)

#class(nj_ehist_selected$voter_registration_date)

#change type
nj_ehist_selected[, c(9, 12)] <- lapply(nj_ehist_selected[, c(9, 12)], as.numeric)

#read csv
nj_vlist_data <- read_csv("vlist_Atlantic.csv")

#clean col names
nj_vlist_data_clean_names <- clean_names(nj_vlist_data)

#select cols
nj_vlist_selected <- select(nj_vlist_data_clean_names, display_id, reg_date, status, first, last)

#check col object type and class
#typeof(nj_vlist_selected$reg_date)

#class(nj_vlist_selected$reg_date)

#convert datetime to date
system.time(nj_vlist_selected$reg_date <- as.Date(nj_vlist_selected$reg_date))


#to uppercase for join

nj_vlist_upper <- nj_vlist_selected %>% 
  mutate(across(where(is.character), toupper)) 

nj_ehist_upper <- nj_ehist_selected %>% 
  mutate(across(where(is.character), toupper))

#class(nj_vlist_upper$reg_date)


#join vlist and ehist
combined <- nj_vlist_upper %>% full_join(nj_ehist_upper, by = c("display_id" = "voter_id"))

#people who appear on the voter list but have no voter history
never_voters <- combined %>%  filter(is.na(status) == TRUE | is.na(voter_status) == TRUE)

#read csv --> this data needs crime type
nj_corrections_data <- read_csv("OPRA #20334 - Release data.csv")


#clean_names
nj_corrections_data_clean_names <- clean_names(nj_corrections_data)

#to upper

nj_corrections_upper <- nj_corrections_data_clean_names %>% 
  mutate(across(where(is.character), toupper))

#convert characters to dates

nj_corrections_upper$released <- mdy(nj_corrections_upper$released)
nj_corrections_upper$dob <- mdy(nj_corrections_upper$dob)

#check col object type and class
#typeof(nj_corrections_upper$dob)

#class(nj_corrections_upper$dob)



combined_w_corrections <- nj_corrections_upper %>% full_join(combined, by = c("last", "first", "dob" = "voter_dob"))

typeof(combined$voter_dob)

test <- combined %>%  filter(display_id == "P4253156165")

test2 <- combined_w_corrections %>%  filter(is.na(sbi) == TRUE)

test3 <- nj_corrections_upper %>%  filter(is.na(sbi) == TRUE)

registered_formerly_incarcerated_individuals <- combined_w_corrections %>%  filter(!is.na(sbi) == TRUE & !is.na(display_id) == TRUE)

number_of_reregistered_voters <- registered_formerly_incarcerated_individuals %>% group_by (display_id) %>% summarize (count = n_distinct(display_id))

#matches <- registered_formerly_incarcerated_individuals %>% group_by(dob)

#matches <- matches %>% arrange((last), .by_group = TRUE)


#most_recent_release <- registered_formerly_incarcerated_individuals %>% filter(max(released))

inactive_registration <- registered_formerly_incarcerated_individuals %>%
  filter(status != "ACTIVE")

#find the most recent release date for registered formerly incarcerated people
assignment_2_dataset <-
  registered_formerly_incarcerated_individuals %>%
  
  group_by(display_id) %>% summarise(
    release_date = format(max(released), '%Y-%m-%d'),
    registration_date = format(max(reg_date), '%Y-%m-%d'),
    election_date = format(max(election_date), '%Y-%m-%d')
  ) %>%
  
  mutate(days_elapsed_between_release_and_registration =
           as.numeric(difftime(registration_date,
                               release_date,
                               units = "days"))) %>%
  
  filter(days_elapsed_between_release_and_registration >= 0) %>%
  mutate(average_time_elapsed_in_months = (mean(
    days_elapsed_between_release_and_registration
  )) / 30.5) %>%
  select(
    release_date,
    registration_date,
    election_date,
    days_elapsed_between_release_and_registration,
    average_time_elapsed_in_months
  )

write_csv(assignment_2_dataset, "assignment2_registration_and_release_data.csv")

 
#discrepancies <- subset(registered_formerly_incarcerated_individuals, registered_formerly_incarcerated_individuals$reg_date != registered_formerly_incarcerated_individuals$voter_registration_date)


#write_csv(registered_formerly_incarcerated_individuals, "registered_individuals.csv")




