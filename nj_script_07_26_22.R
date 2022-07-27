#load packages
library(data.table)
library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)
library(chron)
library(fasttime)
library(timetk)
library(stringi)
library(stringdist)

#set working directory
setwd("your_file_path")

-----------------------
#load election history file for first county  
#will load 21 files for 21 counties  

#read csv
nj_ehist_1 <- read_csv("ehist_Atlantic.csv") 


#keep only plausible cases
nj1_most_recent_vote <-  nj_ehist_1 %>%
  clean_names() %>%
  filter(voter_registration_date > voter_dob | #avoid typos
           voter_dob >= '1900-01-01' | #date of birth must be 1900 or later
           voter_registration_date >= '2016-01-01' | #voter registration date must 2016 or later
           election_date >= '1918-01-01' | #election date must be 1918 or later
           !(voter_status %in% c("INACTIVE CONFIRMATION", "INACTIVE CONFIRMATION NEED ID")) # remove inactive voters
  ) %>%
  rename(#recode variables
    first_name = voter_first,
    last_name = voter_last,
    middle_initial = voter_middle, 
    date_of_birth = voter_dob,
    sex = voter_sex 
  ) %>%
  select(#keep these columns
    voter_id,
    voter_status,
    voter_registration_date,
    last_name,
    first_name,
    middle_initial,
    date_of_birth,
    sex,
    res_street_num,
    res_street_name,
    res_city,
    residence_county_name,
    election_type, 
    election_date
  ) %>% #too many records
  #keep the only the most recent election date for each voter
  mutate(across(where(is.character), toupper),
         indicator = case_when(#indicator for election type
           election_type == "GENERAL" ~ 1,
           !election_type == "GENERAL" ~ 0 
           #if multiple records exist
           #general election records take precedence, keep the most recent
           #if no general election participation, keep most recent non primary, local, etc
         )) %>% group_by(voter_id) %>%  filter( n() >= 1 & sum(indicator) < 1 | n() >= 1 & sum(indicator) > 0) %>%
  slice(which.max(as.Date(election_date, '%Y-%m-%d')))

write_csv(nj1_most_recent_vote, "nj1_most_recent_vote.csv")

#files are very large; may want to save memory by removing after each write 
rm(nj_ehist_1, nj1_most_recent_vote)
-----------------
  
#read csv
nj_ehist_2 <- read_csv("ehist_Bergen.csv")


#keep only plausible cases
nj2_most_recent_vote <-  nj_ehist_2 %>%
  clean_names() %>%
  filter(voter_registration_date > voter_dob | #avoid typos
           voter_dob >= '1900-01-01' |
           voter_registration_date >= '2016-01-01' |
           election_date >= '1918-01-01' |
           !(voter_status %in% c("INACTIVE CONFIRMATION", "INACTIVE CONFIRMATION NEED ID")) # remove inactive voters
  ) %>%
  rename(
    first_name = voter_first,
    last_name = voter_last,
    middle_initial = voter_middle,
    date_of_birth = voter_dob,
    sex = voter_sex
  ) %>%
  select(
    voter_id,
    voter_status,
    voter_registration_date,
    last_name,
    first_name,
    middle_initial,
    date_of_birth,
    sex,
    res_street_num,
    res_street_name,
    res_city,
    residence_county_name,
    election_type,
    election_date
  ) %>%
  mutate(across(where(is.character), toupper),
         indicator = case_when(
           election_type == "GENERAL" ~ 1,
           !election_type == "GENERAL" ~ 0
         )) %>% group_by(voter_id) %>%  filter( n() >= 1 & sum(indicator) < 1 | n() >= 1 & sum(indicator) > 0) %>%
  slice(which.max(as.Date(election_date, '%Y-%m-%d')))

write_csv(nj2_most_recent_vote, "nj2_most_recent_vote.csv")

---------------
#read csv  
nj_ehist_3 <- read_csv("ehist_Burlington.csv")


#keep only plausible cases
nj3_most_recent_vote <-  nj_ehist_3 %>%
  clean_names() %>%
  filter(voter_registration_date > voter_dob | #avoid typos
           voter_dob >= '1900-01-01' |
           voter_registration_date >= '2016-01-01' |
           election_date >= '1918-01-01' |
           !(voter_status %in% c("INACTIVE CONFIRMATION", "INACTIVE CONFIRMATION NEED ID")) # remove inactive voters
  ) %>%
  rename(
    first_name = voter_first,
    last_name = voter_last,
    middle_initial = voter_middle,
    date_of_birth = voter_dob,
    sex = voter_sex
  ) %>%
  select(
    voter_id,
    voter_status,
    voter_registration_date,
    last_name,
    first_name,
    middle_initial,
    date_of_birth,
    sex,
    res_street_num,
    res_street_name,
    res_city,
    residence_county_name,
    election_type,
    election_date
  ) %>%
  mutate(across(where(is.character), toupper),
         indicator = case_when(
           election_type == "GENERAL" ~ 1,
           !election_type == "GENERAL" ~ 0
         )) %>% group_by(voter_id) %>%  filter( n() >= 1 & sum(indicator) < 1 | n() >= 1 & sum(indicator) > 0) %>%
  slice(which.max(as.Date(election_date, '%Y-%m-%d')))

write_csv(nj3_most_recent_vote, "nj3_most_recent_vote.csv")

----------------
#read csv  
nj_ehist_4 <- read_csv("ehist_Camden.csv")


#keep only plausible cases
nj4_most_recent_vote <-  nj_ehist_4 %>%
  clean_names() %>%
  filter(voter_registration_date > voter_dob | #avoid typos
           voter_dob >= '1900-01-01' |
           voter_registration_date >= '2016-01-01' |
           election_date >= '1918-01-01' |
           !(voter_status %in% c("INACTIVE CONFIRMATION", "INACTIVE CONFIRMATION NEED ID")) # remove inactive voters
  ) %>%
  rename(
    first_name = voter_first,
    last_name = voter_last,
    middle_initial = voter_middle,
    date_of_birth = voter_dob,
    sex = voter_sex
  ) %>%
  select(
    voter_id,
    voter_status,
    voter_registration_date,
    last_name,
    first_name,
    middle_initial,
    date_of_birth,
    sex,
    res_street_num,
    res_street_name,
    res_city,
    residence_county_name,
    election_type,
    election_date
  ) %>%
  mutate(across(where(is.character), toupper),
         indicator = case_when(
           election_type == "GENERAL" ~ 1,
           !election_type == "GENERAL" ~ 0
         )) %>% group_by(voter_id) %>%  filter( n() >= 1 & sum(indicator) < 1 | n() >= 1 & sum(indicator) > 0) %>%
  slice(which.max(as.Date(election_date, '%Y-%m-%d')))

write_csv(nj4_most_recent_vote, "nj4_most_recent_vote.csv")

-----------------------
#read csv  
nj_ehist_5 <- read_csv("ehist_Cape May.csv")


#keep only plausible cases
nj5_most_recent_vote <-  nj_ehist_5 %>%
  clean_names() %>%
  filter(voter_registration_date > voter_dob | #avoid typos
           voter_dob >= '1900-01-01' |
           voter_registration_date >= '2016-01-01' |
           election_date >= '1918-01-01' |
           !(voter_status %in% c("INACTIVE CONFIRMATION", "INACTIVE CONFIRMATION NEED ID")) # remove inactive voters
  ) %>%
  rename(
    first_name = voter_first,
    last_name = voter_last,
    middle_initial = voter_middle,
    date_of_birth = voter_dob,
    sex = voter_sex
  ) %>%
  select(
    voter_id,
    voter_status,
    voter_registration_date,
    last_name,
    first_name,
    middle_initial,
    date_of_birth,
    sex,
    res_street_num,
    res_street_name,
    res_city,
    residence_county_name,
    election_type,
    election_date
  ) %>%
  mutate(across(where(is.character), toupper),
         indicator = case_when(
           election_type == "GENERAL" ~ 1,
           !election_type == "GENERAL" ~ 0
         )) %>% group_by(voter_id) %>%  filter( n() >= 1 & sum(indicator) < 1 | n() >= 1 & sum(indicator) > 0) %>%
  slice(which.max(as.Date(election_date, '%Y-%m-%d')))

write_csv(nj5_most_recent_vote, "nj5_most_recent_vote.csv")

-----------
#read csv
nj_ehist_6 <- read_csv("ehist_Cumberland.csv")

#keep only plausible cases
nj6_most_recent_vote <-  nj_ehist_6 %>%
  clean_names() %>%
  filter(voter_registration_date > voter_dob | #avoid typos
           voter_dob >= '1900-01-01' |
           voter_registration_date >= '2016-01-01' |
           election_date >= '1918-01-01' |
           !(voter_status %in% c("INACTIVE CONFIRMATION", "INACTIVE CONFIRMATION NEED ID")) # remove inactive voters
  ) %>%
  rename(
    first_name = voter_first,
    last_name = voter_last,
    middle_initial = voter_middle,
    date_of_birth = voter_dob,
    sex = voter_sex
  ) %>%
  select(
    voter_id,
    voter_status,
    voter_registration_date,
    last_name,
    first_name,
    middle_initial,
    date_of_birth,
    sex,
    res_street_num,
    res_street_name,
    res_city,
    residence_county_name,
    election_type,
    election_date
  ) %>%
  mutate(across(where(is.character), toupper),
         indicator = case_when(
           election_type == "GENERAL" ~ 1,
           !election_type == "GENERAL" ~ 0
         )) %>% group_by(voter_id) %>%  filter( n() >= 1 & sum(indicator) < 1 | n() >= 1 & sum(indicator) > 0) %>%
  slice(which.max(as.Date(election_date, '%Y-%m-%d')))

write_csv(nj6_most_recent_vote, "nj6_most_recent_vote.csv")

--------------
#read csv  
nj_ehist_7 <- read_csv("ehist_Essex.csv")


#keep only plausible cases
nj7_most_recent_vote <-  nj_ehist_7 %>%
  clean_names() %>%
  filter(voter_registration_date > voter_dob | #avoid typos
           voter_dob >= '1900-01-01' |
           voter_registration_date >= '2016-01-01' |
           election_date >= '1918-01-01' |
           !(voter_status %in% c("INACTIVE CONFIRMATION", "INACTIVE CONFIRMATION NEED ID")) # remove inactive voters
  ) %>%
  rename(
    first_name = voter_first,
    last_name = voter_last,
    middle_initial = voter_middle,
    date_of_birth = voter_dob,
    sex = voter_sex
  ) %>%
  select(
    voter_id,
    voter_status,
    voter_registration_date,
    last_name,
    first_name,
    middle_initial,
    date_of_birth,
    sex,
    res_street_num,
    res_street_name,
    res_city,
    residence_county_name,
    election_type,
    election_date
  ) %>%
  mutate(across(where(is.character), toupper),
         indicator = case_when(
           election_type == "GENERAL" ~ 1,
           !election_type == "GENERAL" ~ 0
         )) %>% group_by(voter_id) %>%  filter( n() >= 1 & sum(indicator) < 1 | n() >= 1 & sum(indicator) > 0) %>%
  slice(which.max(as.Date(election_date, '%Y-%m-%d')))

write_csv(nj7_most_recent_vote, "nj7_most_recent_vote.csv")

---------------------------
#read csv
nj_ehist_8 <- read_csv("ehist_Gloucester.csv")

#keep only plausible cases
nj8_most_recent_vote <-  nj_ehist_8 %>%
  clean_names() %>%
  filter(voter_registration_date > voter_dob | #avoid typos
           voter_dob >= '1900-01-01' |
           voter_registration_date >= '2016-01-01' |
           election_date >= '1918-01-01' |
           !(voter_status %in% c("INACTIVE CONFIRMATION", "INACTIVE CONFIRMATION NEED ID")) # remove inactive voters
  ) %>%
  rename(
    first_name = voter_first,
    last_name = voter_last,
    middle_initial = voter_middle,
    date_of_birth = voter_dob,
    sex = voter_sex
  ) %>%
  select(
    voter_id,
    voter_status,
    voter_registration_date,
    last_name,
    first_name,
    middle_initial,
    date_of_birth,
    sex,
    res_street_num,
    res_street_name,
    res_city,
    residence_county_name,
    election_type,
    election_date
  ) %>%
  mutate(across(where(is.character), toupper),
         indicator = case_when(
           election_type == "GENERAL" ~ 1,
           !election_type == "GENERAL" ~ 0
         )) %>% group_by(voter_id) %>%  filter( n() >= 1 & sum(indicator) < 1 | n() >= 1 & sum(indicator) > 0) %>%
  slice(which.max(as.Date(election_date, '%Y-%m-%d')))

write_csv(nj8_most_recent_vote, "nj8_most_recent_vote.csv")

------------------------------
#read csv
nj_ehist_9 <- read_csv("ehist_Hudson.csv")


#keep only plausible cases
nj9_most_recent_vote <-  nj_ehist_9 %>%
  clean_names() %>%
  filter(voter_registration_date > voter_dob | #avoid typos
           voter_dob >= '1900-01-01' |
           voter_registration_date >= '2016-01-01' |
           election_date >= '1918-01-01' |
           !(voter_status %in% c("INACTIVE CONFIRMATION", "INACTIVE CONFIRMATION NEED ID")) # remove inactive voters
  ) %>%
  rename(
    first_name = voter_first,
    last_name = voter_last,
    middle_initial = voter_middle,
    date_of_birth = voter_dob,
    sex = voter_sex
  ) %>%
  select(
    voter_id,
    voter_status,
    voter_registration_date,
    last_name,
    first_name,
    middle_initial,
    date_of_birth,
    sex,
    res_street_num,
    res_street_name,
    res_city,
    residence_county_name,
    election_type,
    election_date
  ) %>%
  mutate(across(where(is.character), toupper),
         indicator = case_when(
           election_type == "GENERAL" ~ 1,
           !election_type == "GENERAL" ~ 0
         )) %>% group_by(voter_id) %>%  filter( n() >= 1 & sum(indicator) < 1 | n() >= 1 & sum(indicator) > 0) %>%
  slice(which.max(as.Date(election_date, '%Y-%m-%d')))

write_csv(nj9_most_recent_vote, "nj9_most_recent_vote.csv")

-------------------------------
#read csv
nj_ehist_10 <- read_csv("ehist_Hunterdon.csv")

#keep only plausible cases
nj10_most_recent_vote <-  nj_ehist_10 %>%
  clean_names() %>%
  filter(voter_registration_date > voter_dob | #avoid typos
           voter_dob >= '1900-01-01' |
           voter_registration_date >= '2016-01-01' |
           election_date >= '1918-01-01' |
           !(voter_status %in% c("INACTIVE CONFIRMATION", "INACTIVE CONFIRMATION NEED ID")) # remove inactive voters
  ) %>%
  rename(
    first_name = voter_first,
    last_name = voter_last,
    middle_initial = voter_middle,
    date_of_birth = voter_dob,
    sex = voter_sex
  ) %>%
  select(
    voter_id,
    voter_status,
    voter_registration_date,
    last_name,
    first_name,
    middle_initial,
    date_of_birth,
    sex,
    res_street_num,
    res_street_name,
    res_city,
    residence_county_name,
    election_type,
    election_date
  ) %>%
  mutate(across(where(is.character), toupper),
         indicator = case_when(
           election_type == "GENERAL" ~ 1,
           !election_type == "GENERAL" ~ 0
         )) %>% group_by(voter_id) %>%  filter( n() >= 1 & sum(indicator) < 1 | n() >= 1 & sum(indicator) > 0) %>%
  slice(which.max(as.Date(election_date, '%Y-%m-%d')))

write_csv(nj10_most_recent_vote, "nj10_most_recent_vote.csv")

-------------------
#read csv
nj_ehist_11 <- read_csv("ehist_Mercer.csv")

#keep only plausible cases
nj11_most_recent_vote <-  nj_ehist_11 %>%
  clean_names() %>%
  filter(voter_registration_date > voter_dob | #avoid typos
           voter_dob >= '1900-01-01' |
           voter_registration_date >= '2016-01-01' |
           election_date >= '1918-01-01' |
           !(voter_status %in% c("INACTIVE CONFIRMATION", "INACTIVE CONFIRMATION NEED ID")) # remove inactive voters
  ) %>%
  rename(
    first_name = voter_first,
    last_name = voter_last,
    middle_initial = voter_middle,
    date_of_birth = voter_dob,
    sex = voter_sex
  ) %>%
  select(
    voter_id,
    voter_status,
    voter_registration_date,
    last_name,
    first_name,
    middle_initial,
    date_of_birth,
    sex,
    res_street_num,
    res_street_name,
    res_city,
    residence_county_name,
    election_type,
    election_date
  ) %>%
  mutate(across(where(is.character), toupper),
         indicator = case_when(
           election_type == "GENERAL" ~ 1,
           !election_type == "GENERAL" ~ 0
         )) %>% group_by(voter_id) %>%  filter( n() >= 1 & sum(indicator) < 1 | n() >= 1 & sum(indicator) > 0) %>%
  slice(which.max(as.Date(election_date, '%Y-%m-%d')))

write_csv(nj11_most_recent_vote, "nj11_most_recent_vote.csv")

--------------------
#read csv
nj_ehist_12 <- read_csv("ehist_Middlesex.csv")


#keep only plausible cases
nj12_most_recent_vote <-  nj_ehist_12 %>%
  clean_names() %>%
  filter(voter_registration_date > voter_dob | #avoid typos
           voter_dob >= '1900-01-01' |
           voter_registration_date >= '2016-01-01' |
           election_date >= '1918-01-01' |
           !(voter_status %in% c("INACTIVE CONFIRMATION", "INACTIVE CONFIRMATION NEED ID")) # remove inactive voters
  ) %>%
  rename(
    first_name = voter_first,
    last_name = voter_last,
    middle_initial = voter_middle,
    date_of_birth = voter_dob,
    sex = voter_sex
  ) %>%
  select(
    voter_id,
    voter_status,
    voter_registration_date,
    last_name,
    first_name,
    middle_initial,
    date_of_birth,
    sex,
    res_street_num,
    res_street_name,
    res_city,
    residence_county_name,
    election_type,
    election_date
  ) %>%
  mutate(across(where(is.character), toupper),
         indicator = case_when(
           election_type == "GENERAL" ~ 1,
           !election_type == "GENERAL" ~ 0
         )) %>% group_by(voter_id) %>%  filter( n() >= 1 & sum(indicator) < 1 | n() >= 1 & sum(indicator) > 0) %>%
  slice(which.max(as.Date(election_date, '%Y-%m-%d')))

write_csv(nj12_most_recent_vote, "nj12_most_recent_vote.csv")

-----------------------------
#read csv
nj_ehist_13 <- read_csv("ehist_Monmouth.csv")


#keep only plausible cases
nj13_most_recent_vote <-  nj_ehist_13 %>%
  clean_names() %>%
  filter(voter_registration_date > voter_dob | #avoid typos
           voter_dob >= '1900-01-01' |
           voter_registration_date >= '2016-01-01' |
           election_date >= '1918-01-01' |
           !(voter_status %in% c("INACTIVE CONFIRMATION", "INACTIVE CONFIRMATION NEED ID")) # remove inactive voters
  ) %>%
  rename(
    first_name = voter_first,
    last_name = voter_last,
    middle_initial = voter_middle,
    date_of_birth = voter_dob,
    sex = voter_sex
  ) %>%
  select(
    voter_id,
    voter_status,
    voter_registration_date,
    last_name,
    first_name,
    middle_initial,
    date_of_birth,
    sex,
    res_street_num,
    res_street_name,
    res_city,
    residence_county_name,
    election_type,
    election_date
  ) %>%
  mutate(across(where(is.character), toupper),
         indicator = case_when(
           election_type == "GENERAL" ~ 1,
           !election_type == "GENERAL" ~ 0
         )) %>% group_by(voter_id) %>%  filter( n() >= 1 & sum(indicator) < 1 | n() >= 1 & sum(indicator) > 0) %>%
  slice(which.max(as.Date(election_date, '%Y-%m-%d')))

write_csv(nj13_most_recent_vote, "nj13_most_recent_vote.csv")

----------------------------
#read csv
nj_ehist_14 <- read_csv("ehist_Morris.csv")


#keep only plausible cases
nj14_most_recent_vote <-  nj_ehist_14 %>%
  clean_names() %>%
  filter(voter_registration_date > voter_dob | #avoid typos
           voter_dob >= '1900-01-01' |
           voter_registration_date >= '2016-01-01' |
           election_date >= '1918-01-01' |
           !(voter_status %in% c("INACTIVE CONFIRMATION", "INACTIVE CONFIRMATION NEED ID")) # remove inactive voters
  ) %>%
  rename(
    first_name = voter_first,
    last_name = voter_last,
    middle_initial = voter_middle,
    date_of_birth = voter_dob,
    sex = voter_sex
  ) %>%
  select(
    voter_id,
    voter_status,
    voter_registration_date,
    last_name,
    first_name,
    middle_initial,
    date_of_birth,
    sex,
    res_street_num,
    res_street_name,
    res_city,
    residence_county_name,
    election_type,
    election_date
  ) %>%
  mutate(across(where(is.character), toupper),
         indicator = case_when(
           election_type == "GENERAL" ~ 1,
           !election_type == "GENERAL" ~ 0
         )) %>% group_by(voter_id) %>%  filter( n() >= 1 & sum(indicator) < 1 | n() >= 1 & sum(indicator) > 0) %>%
  slice(which.max(as.Date(election_date, '%Y-%m-%d')))

write_csv(nj14_most_recent_vote, "nj14_most_recent_vote.csv")

------------------------------
#read csv
nj_ehist_15 <- read_csv("ehist_Ocean.csv")

#keep only plausible cases
nj15_most_recent_vote <-  nj_ehist_15 %>%
  clean_names() %>%
  filter(voter_registration_date > voter_dob | #avoid typos
           voter_dob >= '1900-01-01' |
           voter_registration_date >= '2016-01-01' |
           election_date >= '1918-01-01' |
           !(voter_status %in% c("INACTIVE CONFIRMATION", "INACTIVE CONFIRMATION NEED ID")) # remove inactive voters
  ) %>%
  rename(
    first_name = voter_first,
    last_name = voter_last,
    middle_initial = voter_middle,
    date_of_birth = voter_dob,
    sex = voter_sex
  ) %>%
  select(
    voter_id,
    voter_status,
    voter_registration_date,
    last_name,
    first_name,
    middle_initial,
    date_of_birth,
    sex,
    res_street_num,
    res_street_name,
    res_city,
    residence_county_name,
    election_type,
    election_date
  ) %>%
  mutate(across(where(is.character), toupper),
         indicator = case_when(
           election_type == "GENERAL" ~ 1,
           !election_type == "GENERAL" ~ 0
         )) %>% group_by(voter_id) %>%  filter( n() >= 1 & sum(indicator) < 1 | n() >= 1 & sum(indicator) > 0) %>%
  slice(which.max(as.Date(election_date, '%Y-%m-%d')))

write_csv(nj15_most_recent_vote, "nj15_most_recent_vote.csv")

------------------------
#read csv
nj_ehist_16 <- read_csv("ehist_Passaic.csv")

#keep only plausible cases
nj16_most_recent_vote <-  nj_ehist_16 %>%
  clean_names() %>%
  filter(voter_registration_date > voter_dob | #avoid typos
           voter_dob >= '1900-01-01' |
           voter_registration_date >= '2016-01-01' |
           election_date >= '1918-01-01' |
           !(voter_status %in% c("INACTIVE CONFIRMATION", "INACTIVE CONFIRMATION NEED ID")) # remove inactive voters
  ) %>%
  rename(
    first_name = voter_first,
    last_name = voter_last,
    middle_initial = voter_middle,
    date_of_birth = voter_dob,
    sex = voter_sex
  ) %>%
  select(
    voter_id,
    voter_status,
    voter_registration_date,
    last_name,
    first_name,
    middle_initial,
    date_of_birth,
    sex,
    res_street_num,
    res_street_name,
    res_city,
    residence_county_name,
    election_type,
    election_date
  ) %>%
  mutate(across(where(is.character), toupper),
         indicator = case_when(
           election_type == "GENERAL" ~ 1,
           !election_type == "GENERAL" ~ 0
         )) %>% group_by(voter_id) %>%  filter( n() >= 1 & sum(indicator) < 1 | n() >= 1 & sum(indicator) > 0) %>%
  slice(which.max(as.Date(election_date, '%Y-%m-%d')))

write_csv(nj16_most_recent_vote, "nj16_most_recent_vote.csv")

------------------
#read csv
nj_ehist_17 <- read_csv("ehist_Salem.csv")

#keep only plausible cases
nj17_most_recent_vote <-  nj_ehist_17 %>%
  clean_names() %>%
  filter(voter_registration_date > voter_dob | #avoid typos
           voter_dob >= '1900-01-01' |
           voter_registration_date >= '2016-01-01' |
           election_date >= '1918-01-01' |
           !(voter_status %in% c("INACTIVE CONFIRMATION", "INACTIVE CONFIRMATION NEED ID")) # remove inactive voters
  ) %>%
  rename(
    first_name = voter_first,
    last_name = voter_last,
    middle_initial = voter_middle,
    date_of_birth = voter_dob,
    sex = voter_sex
  ) %>%
  select(
    voter_id,
    voter_status,
    voter_registration_date,
    last_name,
    first_name,
    middle_initial,
    date_of_birth,
    sex,
    res_street_num,
    res_street_name,
    res_city,
    residence_county_name,
    election_type,
    election_date
  ) %>%
  mutate(across(where(is.character), toupper),
         indicator = case_when(
           election_type == "GENERAL" ~ 1,
           !election_type == "GENERAL" ~ 0
         )) %>% group_by(voter_id) %>%  filter( n() >= 1 & sum(indicator) < 1 | n() >= 1 & sum(indicator) > 0) %>%
  slice(which.max(as.Date(election_date, '%Y-%m-%d')))

write_csv(nj17_most_recent_vote, "nj17_most_recent_vote.csv")

----------------------
#read csv
nj_ehist_18 <- read_csv("ehist_Somerset.csv")

#keep only plausible cases
nj18_most_recent_vote <-  nj_ehist_18 %>%
  clean_names() %>%
  filter(voter_registration_date > voter_dob | #avoid typos
           voter_dob >= '1900-01-01' |
           voter_registration_date >= '2016-01-01' |
           election_date >= '1918-01-01' |
           !(voter_status %in% c("INACTIVE CONFIRMATION", "INACTIVE CONFIRMATION NEED ID")) # remove inactive voters
  ) %>%
  rename(
    first_name = voter_first,
    last_name = voter_last,
    middle_initial = voter_middle,
    date_of_birth = voter_dob,
    sex = voter_sex
  ) %>%
  select(
    voter_id,
    voter_status,
    voter_registration_date,
    last_name,
    first_name,
    middle_initial,
    date_of_birth,
    sex,
    res_street_num,
    res_street_name,
    res_city,
    residence_county_name,
    election_type,
    election_date
  ) %>%
  mutate(across(where(is.character), toupper),
         indicator = case_when(
           election_type == "GENERAL" ~ 1,
           !election_type == "GENERAL" ~ 0
         )) %>% group_by(voter_id) %>%  filter( n() >= 1 & sum(indicator) < 1 | n() >= 1 & sum(indicator) > 0) %>%
  slice(which.max(as.Date(election_date, '%Y-%m-%d')))

write_csv(nj18_most_recent_vote, "nj18_most_recent_vote.csv")

------------------
#read csv
nj_ehist_19 <- read_csv("ehist_Sussex.csv")

#keep only plausible cases
nj19_most_recent_vote <-  nj_ehist_19 %>%
  clean_names() %>%
  filter(voter_registration_date > voter_dob | #avoid typos
           voter_dob >= '1900-01-01' |
           voter_registration_date >= '2016-01-01' |
           election_date >= '1918-01-01' |
           !(voter_status %in% c("INACTIVE CONFIRMATION", "INACTIVE CONFIRMATION NEED ID")) # remove inactive voters
  ) %>%
  rename(
    first_name = voter_first,
    last_name = voter_last,
    middle_initial = voter_middle,
    date_of_birth = voter_dob,
    sex = voter_sex
  ) %>%
  select(
    voter_id,
    voter_status,
    voter_registration_date,
    last_name,
    first_name,
    middle_initial,
    date_of_birth,
    sex,
    res_street_num,
    res_street_name,
    res_city,
    residence_county_name,
    election_type,
    election_date
  ) %>%
  mutate(across(where(is.character), toupper),
         indicator = case_when(
           election_type == "GENERAL" ~ 1,
           !election_type == "GENERAL" ~ 0
         )) %>% group_by(voter_id) %>%  filter( n() >= 1 & sum(indicator) < 1 | n() >= 1 & sum(indicator) > 0) %>%
  slice(which.max(as.Date(election_date, '%Y-%m-%d')))

write_csv(nj19_most_recent_vote, "nj19_most_recent_vote.csv")

---------------------
#read csv
nj_ehist_20 <- read_csv("ehist_Union.csv")


#keep only plausible cases
nj20_most_recent_vote <-  nj_ehist_20 %>%
  clean_names() %>%
  filter(voter_registration_date > voter_dob | #avoid typos
           voter_dob >= '1900-01-01' |
           voter_registration_date >= '2016-01-01' |
           election_date >= '1918-01-01' |
           !(voter_status %in% c("INACTIVE CONFIRMATION", "INACTIVE CONFIRMATION NEED ID")) # remove inactive voters
  ) %>%
  rename(
    first_name = voter_first,
    last_name = voter_last,
    middle_initial = voter_middle,
    date_of_birth = voter_dob,
    sex = voter_sex
  ) %>%
  select(
    voter_id,
    voter_status,
    voter_registration_date,
    last_name,
    first_name,
    middle_initial,
    date_of_birth,
    sex,
    res_street_num,
    res_street_name,
    res_city,
    residence_county_name,
    election_type,
    election_date
  ) %>%
  mutate(across(where(is.character), toupper),
         indicator = case_when(
           election_type == "GENERAL" ~ 1,
           !election_type == "GENERAL" ~ 0
         )) %>% group_by(voter_id) %>%  filter( n() >= 1 & sum(indicator) < 1 | n() >= 1 & sum(indicator) > 0) %>%
  slice(which.max(as.Date(election_date, '%Y-%m-%d')))

write_csv(nj20_most_recent_vote, "nj20_most_recent_vote.csv")

-----------------------
#read csv
nj_ehist_21 <- read_csv("ehist_Warren.csv")


#keep only plausible cases
nj21_most_recent_vote <-  nj_ehist_21 %>%
  clean_names() %>%
  filter(voter_registration_date > voter_dob | #avoid typos
           voter_dob >= '1900-01-01' |
           voter_registration_date >= '2016-01-01' |
           election_date >= '1918-01-01' |
           !(voter_status %in% c("INACTIVE CONFIRMATION", "INACTIVE CONFIRMATION NEED ID")) # remove inactive voters
  ) %>%
  rename(
    first_name = voter_first,
    last_name = voter_last,
    middle_initial = voter_middle,
    date_of_birth = voter_dob,
    sex = voter_sex
  ) %>%
  select(
    voter_id,
    voter_status,
    voter_registration_date,
    last_name,
    first_name,
    middle_initial,
    date_of_birth,
    sex,
    res_street_num,
    res_street_name,
    res_city,
    residence_county_name,
    election_type,
    election_date
  ) %>%
  mutate(across(where(is.character), toupper),
         indicator = case_when(
           election_type == "GENERAL" ~ 1,
           !election_type == "GENERAL" ~ 0
         )) %>% group_by(voter_id) %>%  filter( n() >= 1 & sum(indicator) < 1 | n() >= 1 & sum(indicator) > 0) %>%
  slice(which.max(as.Date(election_date, '%Y-%m-%d')))

write_csv(nj21_most_recent_vote, "nj21_most_recent_vote.csv")

----------------------------
  
  #read voter files into a list and merge to save time
  data_join <-
  list.files(path = "your_file_path",
             # Identify all CSV files
             pattern = "\\_most_recent_vote.csv$",
             full.names = TRUE) %>%
  # Store all files in list
  lapply(read_csv, col_types = cols(res_street_num = "c")) %>%  
  bind_rows() %>%
  filter(# remove inactive voters
    !(voter_status %in% c("INACTIVE CONFIRMATION", "INACTIVE CONFIRMATION NEED ID"))
    ) %>%
    # remove unnecessary columns
  select(-c(    
    indicator,
    res_street_num,
    res_street_name,
    res_city
  )
) 

write_csv(data_join, "data_join.csv")

#per the New Jersey Dept of Corrections all individuals in the dataset were convicted of felonies
#read full corrections data -->
nj_corrections_full_dataset2 <-
  read_csv("OPRA #20367- releases 2016 - 2022.csv") %>% 
  #standardize column names
  clean_names() %>% #clean data
  mutate(
    across(where(is.character), toupper), #if character column make all uppercase
    release_date = mdy(release_date), #convert to mdy date
    date_of_birth = mdy(birth_date), #convert to mdy date
    date_of_birth = ifelse(
      date_of_birth  > "2003-12-31", #for dates later than 2003 fix formatting
      format(date_of_birth,
             "19%y-%m-%d"), #supplement prefix "19" for "20" for these dates
      format(date_of_birth)
    ),
    date_of_birth = as.Date(date_of_birth, '%Y-%m-%d'), #set character to date type
    #make an indicator column
    indicator = ifelse(grepl("[0-9]", county_of_commitment), # if the field contains numeric characters
                       1, #assign 1
                       0),#if the field contains letter characters assign 0
    #make a county name column
    county_name =
      ifelse(
        str_detect(county_of_commitment,
                   "^UN|^US|^OS", #if the original field does not contain the following letter sequences
                   negate = TRUE) & indicator < 1, #or any numbers
        substr(county_of_commitment, 1, 3), #place the first three letters in the county name column
        ifelse(
          str_detect(county_of_commitment,
                     "^UN|^US|^OS") & indicator < 1, #if the original field contains the following letter sequences and no numbers
          substr(county_of_commitment, 1, 2), #place the first two letters in the county name column
          "" #if the original field contains numbers, write "NA" in the county name column
        )
        
      ),
    #make a court type column
    court_type =
      ifelse(
        str_detect(county_of_commitment,
                   "^UN|^US|^OS", #if the original field does not contain the following letter sequences
                   negate = TRUE) & indicator < 1, #or any numbers
        substr(county_of_commitment, 4, 5), #place the last two letters in the court type column
        ifelse(
          str_detect(county_of_commitment,
                     "^UN|^US|^OS") & indicator < 1, #if the original field contains the following letter sequences and no numbers
          substr(county_of_commitment, 3, 4), #place the last two letters in the court type column
          "" #if the original field contains numbers, write "NA" in the court type column
        )
        
      ),
    #replace county prefix in county name column with county full name
    county_name = stri_replace_all_regex(
      county_name,
      pattern = c(
        "CAM",
        "MID",
        "MER",
        "MON",
        "^UN", #only substitute this prefix if it appears at the beginning of the letter sequence
        "CUM",
        "^BER", #only substitute this prefix if it appears at the beginning of the letter sequence
        "ATL",
        "^ESX", #only substitute this prefix if it appears at the beginning of the letter sequence
        "SOM",
        "SAL",
        "PAS",
        "HUD",
        "GLO",
        "CAP",
        "^SUS", #only substitute this prefix if it appears at the beginning of the letter sequence
        "MOR",
        "WAR",
        "BUR",
        "OCN",
        "HUN",
        "OS",
        "NA",
        "^US" #only substitute this prefix if it appears at the beginning of the letter sequence
      ),
      replacement = c(
        "CAMDEN",
        "MIDDLESEX",
        "MERCER",
        "MONMOUTH",
        "UNION",
        "CUMBERLAND",
        "BERGEN",
        "ATLANTIC",
        "ESSEX",
        "SOMERSET",
        "SALEM",
        "PASSAIC",
        "HUDSON",
        "GLOUCESTER",
        "CAPE MAY",
        "SUSSEX",
        "MORRIS",
        "WARREN",
        "BURLINGTON",
        "OCEAN",
        "HUNTERDON",
        "OUT OF STATE",
        "",
        "UNITED STATES"
      ),
      vectorize = FALSE
    ),
    #replace court prefix in court type column with court full name where known
    court_type = stri_replace_all_regex(
      court_type,
      pattern = c("SU", "MU", "NA", "N", "OS", "D"),
      replacement = c("SUPERIOR", "MUNICIPAL", "", "N", "OUT OF STATE", "DISTRICT"),
      vectorize = FALSE
    ),#remove punctuation from offence code column
    offence_code = str_replace_all(offence_code, "[[:punct:]]", ""),
    offense_level = stri_replace_all_regex(
      offence_code,#level of felony is either the last number or the last letter in the string
      pattern = c(".*1$", ".*2$", ".*3$", ".*4$", ".*A$", ".*B$", ".*C$", ".*D$", "PV"),
      replacement = c("1st Degree", "2nd Degree", "3rd Degree", "4th Degree", 
                      "1st Degree", "2nd Degree", "3rd Degree", "4th Degree", "Parole Violation"),
      vectorize = FALSE
    )
  ) %>%
  filter(
    !( #remove those who are deceased or whose New Jersey state residency is questionable
      movement_reason %in% c("DECEASED",
"RETURN INTERSTATE COMPACT",
 "OIS TRANSFER",                          
"INTERNATIONAL RELEASE",                  
"INTERSTATE TRANSFER"                
)
    ) 
  ) %>% #remove unnecessary columns
  select(-c(county_of_commitment, indicator, birth_date, offence_code, offense_desc, court_type)) %>%
  group_by(sbi) %>%
  mutate(
    min_release = min(release_date),
    max_release = max(release_date)
  )

#safekeeping
write_csv(nj_corrections_full_dataset2, "nj_corrections_16_22_dataset.csv")

#read in parole data
nj_parole_dataset2 <-
  read_csv("OPRA_07_19_20_parole.csv") %>% 
  clean_names() %>% #standardize column names
  #clean data
  filter(!str_detect(dob, "09$|17$")) %>%#a handful of individuals are too old to be included in the dataset. Remove them.
  group_by(sbi)%>%
  mutate(
    across(where(is.character), toupper),#if character column make all uppercase
    sbi =  paste0("000", sbi),#add three leading zeros to match unique ids in nj corrections file
    date_of_birth = dmy(dob),# release_date = mdy(release_date), #convert to mdy date
    date_of_birth = ifelse(
      date_of_birth  > "2003-12-31", #for dates later than 2003 fix formatting
      format(date_of_birth,
             "19%y-%m-%d"), #supplement prefix "19" for "20" for these dates
      format(date_of_birth)
    ),
    date_of_birth = as.Date(date_of_birth, '%Y-%m-%d'),
    supr_start = dmy(supr_start),
    supr_end_expire = dmy(supr_end_expire),#fix dates
    supr_end_expire = format(supr_end_expire, "20%y-%m-%d"),
    supr_end_expire = as.Date(supr_end_expire),
    min_start = min(supr_start), #find minimum start date of parole/supervision
    max_end = max(supr_end_expire), #find maximum end date of parole/supervision
    county = gsub("N/A", "", county)
  ) %>%
  filter(
    !(
      sbi %in% c("unique_corrections_id1", " unique_corrections_id2", " unique_corrections_id3", " unique_corrections_id4", " unique_corrections_id5", " unique_corrections_id6", " unique_corrections_id7",
                 " unique_corrections_id8", " unique_corrections_id9", " unique_corrections_id10")  
    )
  ) %>% 
  filter(
    
    !(
      supervision_status %in%
        c(
          "CLSD: RECOMMITED", #remove people who are re-incarcerated
          "CLSD: PDS ENTRY ERROR" ) 
    )
  ) %>%
  filter(#again, no dead people
    !(supervision_status %like%  ".*\\DIED$")
  ) %>%
  select(#drop unnecessary columns
    -c("p_num", "dob", "supr_start", "supr_end_expire" )
  )

#correct typos
nj_parole_dataset2$max_end[nj_parole_dataset2$sbi == "unique_corrections_id11"] <- "2016-07-20" 
nj_parole_dataset2$max_end[nj_parole_dataset2$sbi == "unique_corrections_id12"] <- "2018-11-07" 
nj_parole_dataset2$max_end[nj_parole_dataset2$sbi == "unique_corrections_id13"] <- "2019-01-10" 
nj_parole_dataset2$max_end[nj_parole_dataset2$sbi == "unique_corrections_id14"] <- "2017-01-07"
nj_parole_dataset2$max_end[nj_parole_dataset2$sbi == "unique_corrections_id15"] <- "2018-01-03"
nj_parole_dataset2$max_end[nj_parole_dataset2$sbi == "unique_corrections_id16"] <- "2019-06-04"
nj_parole_dataset2$max_end[nj_parole_dataset2$sbi == "unique_corrections_id17"] <- "2020-06-19"
nj_parole_dataset2$max_end[nj_parole_dataset2$sbi == "unique_corrections_id18"] <- "2020-01-13"
nj_parole_dataset2$max_end[nj_parole_dataset2$sbi == "unique_corrections_id19"] <- "2020-04-20"


write_csv(nj_parole_dataset2, "nj_parole_df_cleaned3.csv")

#how many unique records in my df?
length(unique(data_join$voter_id)) #1717854 unique records
length(unique(nj_corrections_full_dataset2$sbi)) #41321 unique records
length(unique(nj_parole_dataset2$sbi)) # 26141 unique records

#combine df of returned citizens listed on voter rolls with parole dataset
new_df8 <-
  left_join(nj_corrections_full_dataset2, #pull in supervision status and parole start date and end date
            nj_parole_dataset2 %>% dplyr::select(supervision_status, min_start, max_end),
            by = "sbi")

write_csv(new_df8, "parolees_matched_main_corrections_file.csv")

#exact matches
#combine voter file df and corrections df
test_fulldataset <-
  data_join %>% full_join(new_df8, #match voter and corrections df by first and last name and dob
                          by = c("last_name", "first_name", "date_of_birth"))

#check unique record count...
length(unique(test_fulldataset$sbi))#41322 unique records
length(unique(test_fulldataset$voter_id))#1717855 unique records


#code df for analysis
df3_fulldataset <- test_fulldataset %>%
  mutate(#indicator for returned citizen
    returned_citizen = ifelse(!(is.na(sbi) == TRUE), #if the person has a corrections id, they're a returned citizen
                              1,
                              0),
    registered = ifelse(#indicator for registered
      !(is.na(voter_id) == TRUE) & (is.na(sbi) == TRUE),#if the person has a voter id, but corrections id, they're registered
      1,
      ifelse(#if the person is a returned citizen and they have a voter id on file and their release date 
        #is less than their voter registration date, they are registered
        returned_citizen > 0 &
          !(is.na(voter_id) == TRUE) &
          min_release < voter_registration_date,
        1,
        0
      )
    ),
    parole = ifelse(#parole indicator
      returned_citizen > 0 &#if the person is a returned citizen
        #but doesn't have a corresponding record from the state parole board dataset
        #they need to be registered to vote and 
        #they need to have a release date that is less than their voter registration date
        registered > 0 & min_release < voter_registration_date &
        (is.na(min_start) == TRUE) & movement_reason %in% c(#their reason for release needs to be in this list
          "PAROLE",
          "INTENSIVE SUPERVISION PROGRAM",
          "CONTINUED ON PAROLE",
          "CONTINUED COMMUNITY SUPERVISION",
          "COURT ORDER RELEASE"
        ) ,
      1,
      ifelse(#otherwise if the person is a returned citizen
        returned_citizen > 0 &#registered to vote
          registered > 0 & !(is.na(min_start) == TRUE) &#but there is a corresponding record from the parole board
          min_release < min_start &#their release date needs to be less than the start date of their parole
          min_release < voter_registration_date,#and their release date also needs to be less than their voter registration date
        1,
        ifelse(#if the person is a returned citizen
          returned_citizen > 0 & registered < 1 &#who is not registered to vote
            (is.na(min_start) == TRUE) & movement_reason %in% c(#and doesn't have a corresponding parole board file
              "PAROLE",#their reason for release needs to be in this list
              "INTENSIVE SUPERVISION PROGRAM",
              "CONTINUED ON PAROLE",
              "CONTINUED COMMUNITY SUPERVISION",
              "COURT ORDER RELEASE"
            ),
          1,
          ifelse(#if the person is a returned citizen, who isn't registered, and corresponding parole board record
            returned_citizen > 0 &
              registered < 1 & !(is.na(min_start) == TRUE) &
              min_release < min_start,#their release date needs to be less than their parole start date
            1,
            0
          )
          
        )
        
      )
      
    ),
    
    turnout = ifelse(#if the person is a return citizen
      returned_citizen > 0 &
        parole > 0 & min_release < voter_registration_date &#who was paroled with a relese date less than their registration date
        voter_registration_date < election_date,#their registration date needs to be smaller than their election date
      1,
      ifelse(
        returned_citizen > 0 &#if the person is a returned citizen, who was not paroled
          parole < 1 & min_release < voter_registration_date &#their release date still needs to be less than their registration date
          voter_registration_date < election_date,#and their registration date needs to be smaller than their election date
        1,
        0
        
      )
    ),
    before_effective_date = ifelse(#to be listed in the before period, the individual needs to be a returned citizen
      returned_citizen > 0 & min_release < "2020-03-17",#whose release date is less than March 17, 2020
      1,
      ifelse(returned_citizen > 0 & min_release >= "2020-03-17",#otherwise, they need to be a returned citizen
             #whose release date is greater than or equal to March 17, 2020
             0,
             0)
    )
  )  


#deduping df3_fulldataset by key identifiers, sbi and voter id, keep NAs
test6 <-#for percentages to be reliable the date must be cleaned and removed of as many duplicates as possible
  df3_fulldataset %>% filter (., is.na(sbi) == FALSE |
                                is.na(voter_id) == FALSE) %>% distinct(.) %>%
  subset(#keep all distinct records with a voter id or corrections id
    select = -c(#remove columns which can cause duplicates
      movement_reason,
      offense_level,
      supervision_status,
      voter_status,
      release_date,
      middle_initial,
      residence_county_name,
      county_name
    )
  )
#still deduping; be extra certain
test8 <- unique(test6)

write_csv(test8, "test8.csv")

#drop columns from df to make viewing easier
test9 <- test8 %>%
  subset(select = c(returned_citizen, registered, parole, turnout, before_effective_date) )

#for percentage data, after unique output is printed go to the df and filter visually to confirm which percentage goes 
#with which group

#find percentage of parolees registered to vote overall, before, and after...
simple <- test9 %>% 
  group_by(parole, registered)%>% #tally records by parole status and turnout status
  mutate(
    sub_group_tally1 = n()
  ) %>%
  ungroup() %>%
  group_by(parole) %>%#tally records by parole status 
  mutate(
    sub_group_tally2 = n()
  ) %>%
  ungroup() %>% 
  group_by(before_effective_date, parole, registered) %>%#tally records by before effective date, parole status, and registration status
  mutate(
    sub_group_tally3 = n()
  ) %>%
  ungroup() %>% 
  group_by(before_effective_date, parole) %>%#tally records by before effective date and parole status
  mutate(
    sub_group_tally4 = n()
  ) %>%
  ungroup() %>% 
  mutate(#overall number of parolees registered to vote
    parolees_registered_to_vote = ifelse(
      parole > 0, #be a parolee; if they turned out; calculate percentage for that subgroup
      (sub_group_tally1/sub_group_tally2)*100,
      ""
   ),#before and after number of parolees registered to vote
   parolees_registered_to_vote_before_inclusive = ifelse(
     parole > 0, #be a parolee; if they turned out; calculate percentage for that subgroup
     (sub_group_tally3/sub_group_tally4)*100,
     ""
   )
   
  )  

#percentage of parolees registered to vote overall full time frame?
print(unique(simple$parolees_registered_to_vote))
# "" "7.17209121029039" "92.8279087897096"; look at simple to confirm correct groups

#percentage of parolees registered to vote in before and after periods?
print(unique(simple$parolees_registered_to_vote_before_inclusive))
#   ""  "8.22327435833541" "3.39892665474061" "96.6010733452594" "91.7767256416646"
#look at simple to confirm correct groups


#find turnout before and after....
turnout_bytime <- test8 %>% 
  group_by(parole, turnout)%>% #tally records by parole status and turnout status
  mutate(
    sub_group_tally1 = n()
  ) %>%
  ungroup() %>%
  group_by(parole) %>%#tally records by parole status 
  mutate(
    sub_group_tally2 = n()
  ) %>%
  ungroup() %>% 
  group_by(before_effective_date, parole, turnout) %>%#tally records by before effective date, parole status, and turnout status
  mutate(
    sub_group_tally3 = n()
  ) %>%
  ungroup() %>% 
  group_by(before_effective_date, parole) %>%#tally records by before effective date and parole status
  mutate(
    sub_group_tally4 = n()
  ) %>%
  ungroup() %>% 
  mutate(
    parolees_turnedout_before_inclusive =  ((sub_group_tally3/sub_group_tally4)*100)
    
  )

#look at turnout by time to confirm percentages
print(unique(turnout_bytime$parolees_turnedout_before_inclusive))
#6.75003085  6.55370047   0.01546023  1.96779964 




#how long did it take individuals to register to vote after being released in
#before the effective date
#time to register before by parole status
before_time_to_register2 <- test8 %>%
  filter(#registered voter; registered after being released
    !(is.na(voter_id)) & min_release < voter_registration_date & before_effective_date > 0
  ) %>%
  group_by(sbi) %>% #how long between release and registration?
  mutate(days_elapsed_between_release_and_registration =
           as.numeric(difftime(voter_registration_date,
                               min_release,
                               units = "days"))) %>% # in days?
  
  filter(days_elapsed_between_release_and_registration >= 0) %>%
  mutate(time_elapsed_in_months = (
    days_elapsed_between_release_and_registration #time difference in months
   / 30.5) ) 

#median time to register before effective date by parole status....
grouped_before_time_to_register <- before_time_to_register2 %>%
  group_by(
    parole
  ) %>% 
  filter(
    before_effective_date > 0#before period
  )%>% 
  mutate(#the registration data is skewed. Use the median instead of the average.
    grouped_median = median(time_elapsed_in_months)
  )

print(unique(grouped_before_time_to_register$grouped_median))
#21.21311 27.75410

#how much time did it take to register after release in the after effective period?
#time to register after by parole status
after_time_to_register <- test8 %>%
  filter(
    !(is.na(voter_id)) & min_release < voter_registration_date & before_effective_date < 1
  ) %>%
  group_by(sbi) %>%
  mutate(days_elapsed_between_release_and_registration =
           as.numeric(difftime(voter_registration_date,
                               min_release,
                               units = "days"))) %>%
  
  filter(days_elapsed_between_release_and_registration >= 0) %>%
  mutate(time_elapsed_in_months = (
    days_elapsed_between_release_and_registration
   / 30.5) ) 

#how much time did it take to register by parole status in the after effective date time period?
grouped_after_time_to_register <- after_time_to_register %>%
  group_by(
    parole
  ) %>%
  filter(
    before_effective_date < 1#after period
  )%>%
  mutate(#the registration data is skewed. Use the median instead of the average.
    grouped_median = median(time_elapsed_in_months)
  )

print(unique(grouped_after_time_to_register$grouped_median))
#2.327869 3.262295

#take all percentages and plot in excel to make life easy
#take screen shots of minimalist graphs and place in google slides
#use google slides to polish
---------------------------
#vizs

#bubbles
#plum chart

#look at over percentages and find correct numbers...
print(unique(simple$parolees_registered_to_vote))
#overall: 7.2% 92.8%

#look at before and after percentages and find correct numbers...
print(unique(simple$parolees_registered_to_vote_before_inclusive))
#after: 3.4%, 96.6%; before: 8.2%, 91.2%

#define coordinates
DF <- expand.grid(-30:30,-30:30) #define coordinates

#percentage of parolees registered to vote overall

#to calculate inner circle threshold take sqrt((observation count * percent)/pi)
#test; confirm formula for calculating R threshold correct    
sqrt(
  (2821*0.072)/pi
)
#8.040681

#create plum chart (circular waffle chart)
DF2 <- DF %>% mutate(R = sqrt(DF$Var1 ^ 2 + DF$Var2 ^ 2)) %>%
  filter(R <= 30) %>% #set radius less than 30
  mutate(
    returned_citizens = sample( #this label will be remove from eventual viz
      c("unregistered (92.88%)", "registered (7.2%)"), #add categories with percentages calculated from returned citizens df
      2821,
      replace = TRUE, #no repeats
      prob = c(0.928, 0.072) # set probability
    ),
    returned_citizens = case_when(R < 8.04 ~ "registered (7.2%)", # restrict registered to "inner circle"
                                  R >= 8.04 ~ "unregistered (92.8%)") # unregistered to "outer circle"
  )


#plot bubbles
ggplot(DF2, aes(x = Var1, y = Var2, color = returned_citizens)) + #plot plum chart
  geom_point(size = 0.9) + coord_fixed() + theme_void() + #get rid of noise
  scale_color_manual(values = c("turquoise4", "lightgrey")) # set colors



#percentage of parolees registered to vote in before period

#to calculate inner circle threshold take sqrt((observation count * percent)/pi)
#test; confirm formula for calculating R threshold correct    
sqrt(
  (2821*0.082)/pi
)
#8.580914

#create plum chart (circular waffle chart)
DF2 <- DF %>% mutate(R = sqrt(DF$Var1 ^ 2 + DF$Var2 ^ 2)) %>%
  filter(R <= 30) %>% #set radius less than 30
  mutate(
    returned_citizens = sample( #this label will be remove from eventual viz
      c("unregistered (92.88%)", "registered (7.2%)"), #add categories with percentages calculated from returned citizens df
      2821,
      replace = TRUE, #no repeats
      prob = c(0.918, 0.082) # set probability
    ),
    returned_citizens = case_when(R < 8.58 ~ "registered (7.2%)", # restrict registered to "inner circle"
                                  R >= 8.58 ~ "unregistered (92.8%)") # unregistered to "outer circle"
  )


#test; confirm formula for calculating R threshold correct    
sqrt(
  (2821*0.082)/pi
)

#plot bubbles
ggplot(DF2, aes(x = Var1, y = Var2, color = returned_citizens)) + #plot plum chart
  geom_point(size = 0.9) + coord_fixed() + theme_void() + #get rid of noise
  scale_color_manual(values = c("turquoise4", "lightgrey")) # set colors

#percentage of parolees registered to vote in after period

#to calculate inner circle threshold take sqrt((observation count * percent)/pi)
#test; confirm formula for calculating R threshold correct    
sqrt(
  (2821*0.034)/pi
)
#5.52543

#create plum chart (circular waffle chart)
DF2 <- DF %>% mutate(R = sqrt(DF$Var1 ^ 2 + DF$Var2 ^ 2)) %>%
  filter(R <= 30) %>% #set radius less than 30
  mutate(
    returned_citizens = sample( #this label will be remove from eventual viz
      c("unregistered (92.88%)", "registered (7.2%)"), #add categories with percentages calculated from returned citizens df
      2821,
      replace = TRUE, #no repeats
      prob = c(0.966, 0.034) # set probability
    ),
    returned_citizens = case_when(R < 5.52 ~ "registered (7.2%)", # restrict registered to "inner circle"
                                  R >= 5.52 ~ "unregistered (92.8%)") # unregistered to "outer circle"
  )


#plot bubbles
ggplot(DF2, aes(x = Var1, y = Var2, color = returned_citizens)) + #plot plum chart
  geom_point(size = 0.9) + coord_fixed() + theme_void() + #get rid of noise
  scale_color_manual(values = c("turquoise4", "lightgrey")) # set colors


#take screen shots of circle graphs and use preview to crop
#place cropped circles in google slides
#to size look at outer circle counts which represent overall pop for that time period:
#overall 5131 parolees
#before 4013
#after 1118
#4013 is the base
4013/5131
#approx 78 percent; round up to 80
1118/4013
#approx 28 percent; round up, pretend 33
#in google slides; before (4013) == 3 ticks
#after(1118) == 1 tick
#overall(5031 == 4 ticks)
