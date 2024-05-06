# script for Holmusk take home challenge 
  # author: Molly Grant (Bergquist-O'Sullivan)
  # date: 8 May 2024 
  # R version 3.6.2 (2019-12-12)
  # running under: OS X Snow Leopard 13.6.5

######################################
#### load packages ####
library(dplyr)
library(survey)
library(lavaan)
library(psych)
library(naniar)
library(haven)
library(GPArotation)
library(ggplot2)
library(hrbrthemes)
library(tidyverse)
library(foreign)
library(Amelia)
library(mice)
library(naniar)
library(stats)
library(tidyr)

######################################
#### data set up ####
  #### read in data
setwd("/Users/mollybergquist/Desktop") # set working directory
bill_amount <- read.csv("bill_amount.csv") 
bill_id <- read.csv("bill_id.csv") 
clinical_data <- read.csv("clinical_data.csv")
demographics <- read.csv("demographics.csv")

  #### view structure of data
str(bill_amount)
str(bill_id)
str(clinical_data)
str(demographics)

  #### check for duplicates
table(duplicated(bill_amount$bill_id)) # note: bill_id are all unique
table(duplicated(bill_id$patient_id)) # note: some patients (patient_id) have multiple entries but all bill_id are unique
table(duplicated(clinical_data$id)) # note: some patients have multiple entries (need to inspect further)
table(duplicated(demographics$patient_id)) # note: patient_id are all unique

  #### inspect the duplicates in 'clinical_data'
duplicated_clinical <- clinical_data %>%
  group_by(id) %>%
  filter(n()>1) %>%
  ungroup() # extract duplicates into a new data frame to inspect
View(duplicated_clinical)
table(duplicated(duplicated_clinical$id)) # note: 400 instances where the same "id" value appears more than once. appears to be due to multiple hospital visits. 

  #### note: proceeding with only one entry per patient (i.e., most recent data point) to satisfy independence assumption in later analyses. future analyses should spend more time dealing with duplicates
clinical_data$date_of_admission <-  as.Date(clinical_data$date_of_admission, format = "%d/%m/%y") #change to date format
clinical_data_filtered <- clinical_data %>%
  group_by(id) %>%
  filter(rank(desc(date_of_admission)) == 1) %>%
  ungroup() # select the entry from duplicates that is the latest data point

  #### join bill_amount and bill_id into data frame 'bill_data'
bill_data <- dplyr::full_join(bill_amount, bill_id, by = "bill_id", suffix = c("", ".x")) # join by bill_id
bill_data <- bill_data %>% 
  group_by(patient_id) %>% 
  mutate(row_num = row_number()) %>% 
  ungroup() # add a row number within each patient_id group

bill_data <- bill_data %>% 
  pivot_wider(
    names_from = row_num,
    values_from = c(bill_id, amount, date_of_admission),
    names_glue = "{.value}_{row_num}"
  ) # pivot the data to wide format to be able join to other data sets

# notes: There are 3,000 unique patient_id in 'bill_data'. each date_of_admission corresponds to a bill_id and amount. the maximum number of bills/amounts/admission dates is 16.

  #### join 'clinical_data_filtered' and 'demographics' into data frame 'clinical_data_full'
names(clinical_data_filtered)[names(clinical_data_filtered) == "id"] <- "patient_id" # rename column to match other data
clinical_data_full <- dplyr::full_join(clinical_data_filtered, demographics, by = "patient_id", suffix = c("", ".x")) # join 

  #### join all data 
data_full <- dplyr::left_join(clinical_data_full, bill_data, by = "patient_id", suffix = c("", ".x")) # join 
data_full <- data_full %>% dplyr::select(-contains(".x")) # remove any duplicate columns

######################################
##### data cleaning and re-coding ####

subset <- data_full[c("patient_id", "gender", "race", "resident_status", "date_of_birth",
                      "trt_the", 
                      "trt_anx", "trt_con", "trt_adt", "trt_ssr", "trt_oth")] # subset variables of interest for the RQ

  # explore missing values
View(miss_case_summary(subset)) # no missing data at the case level
View(miss_var_summary(subset)) # no missing data at the variable level

  #re-code variables
table(subset$gender) # needs re-coding
subset$gender_update[subset$gender == "f"] <- "Female"
subset$gender_update[subset$gender == "Female"] <- "Female"
subset$gender_update[subset$gender == "m"] <- "Male"
subset$gender_update[subset$gender == "Male"] <- "Male"
table(subset$gender, subset$gender_update)
table(subset$gender_update, exclude = NULL)
prop.table(table(subset$gender_update, exclude = NULL))*100

table(subset$race) # needs re-coding
subset$race_update[subset$race == "chinese"] <- "Chinese"
subset$race_update[subset$race == "Chinese"] <- "Chinese"
subset$race_update[subset$race == "India"] <- "Indian"
subset$race_update[subset$race == "Indian"] <- "Indian"
subset$race_update[subset$race == "Malay"] <- "Malay"
subset$race_update[subset$race == "Others"] <- "Others"
table(subset$race, subset$race_update , exclude = NULL)
table(subset$race_update, exclude = NULL)
prop.table(table(subset$race_update, exclude = NULL))*100

table(subset$date_of_birth) # decided to re-code for analyses
subset$current_date <- Sys.Date()
subset$date_of_birth_update <- as.numeric(difftime(subset$current_date, subset$date_of_birth, units ="days")) /(365.25/12)  # create age in months
subset$date_of_birth_update <- subset$date_of_birth_update / 12 # create age in years 
describe(subset$date_of_birth_update)
subset$date_of_birth_cat[subset$date_of_birth_update < 45 ] <- "Under 45" # recategorise age for later analyses
subset$date_of_birth_cat[subset$date_of_birth_update >= 45 & subset$date_of_birth_update <= 60] <- "45-60"
subset$date_of_birth_cat[subset$date_of_birth_update > 60 & subset$date_of_birth_update <= 75] <- "60-75"
subset$date_of_birth_cat[subset$date_of_birth_update > 75 ] <- "Over 75"

table(subset$resident_status) # does not need re-coding
prop.table(table(subset$resident_status))*100

subset[ ,6:16] <- lapply(subset[ ,6:16], as.factor) # convert variables to factors for analysis
str(subset)

  # create an 'any treatment' variable
subset$anytreat <- ifelse(subset$trt_adt == 0 & subset$trt_con == 0 & subset$trt_anx == 0 & subset$trt_ssr == 0 & subset$trt_oth == 0 & subset$trt_the == 0, 0, 1)
table(subset$anytreat)
prop.table(table(subset$anytreat))*100

######################################
#### analyses ####
  # cross-tab of any treatment and psychotherapy
table(subset$anytreat, subset$trt_the, exclude = NULL)
prop.table(table(subset$anytreat, subset$trt_the,exclude = NULL))*100

  # descriptive statistics
table(subset$gender_update, subset$trt_the, exclude = NULL)
table(subset$race_update, subset$trt_the, exclude = NULL)
table(subset$resident_status, subset$trt_the, exclude = NULL)
table(subset$date_of_birth_cat, subset$trt_the, exclude = NULL)

  # chi-sqaure
chisq.test(subset$gender_update, subset$trt_the, correct = FALSE)
chisq.test(subset$race_update, subset$trt_the, correct = FALSE)
chisq.test(subset$resident_status, subset$trt_the, correct = FALSE)
chisq.test(subset$date_of_birth_cat, subset$trt_the, correct = FALSE)
######################################
