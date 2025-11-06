########################
#Gillian 
#11/6/2025
#Cleaning Messy Data
########################

#Today’s exercise you will take data entered into excel from a mangrove propagule growth experiment and write a function to clean the data and prepare it for analysis. This is a common task you could be given as we will often be given a spreadsheet from an experiment. Often there are errors, typos, wrong data types that need to be carefully dealt with prior to analyzing the data.

#Experimental Design:

#2 Locations 3 Treatments (Control, Treatment 1, Treatment 2) 25 Propagules in each Location/Treatment block

#Measure the propagule height for the 25 propagules (repeated measures of same propagule) once a week for 6 months

#The Function will:
  
  #read in data from an excel spreadsheet (found in the /data directory)

#return a dataframe that is ready for analysis

#Hints: inspect the data first and come up with a plan


#load libraries
library(tidyverse)
library(summarytools)
library(readxl)
library(janitor)


#load data
mang <- read_excel("data/messy_mangrove_data.xlsx")

#look at summary
view(dfSummary(mang))

#do it outside the function

#pivot longer 

long_mang <- mang %>%
  pivot_longer(
    cols = starts_with("Week"),    # select all columns that start with "Week"
    names_to = "Week",             # new column name for the week
    values_to = "Value"            # new column name for the values
  )


#now make values column numeric and not characters 
long_mang <- long_mang %>%
  mutate(Value = as.numeric(Value))


#now I want to make an average value from the week before and week after to fill the NA slot 

# Make sure Week is numeric
long_mang <- long_mang %>%
  mutate(Week = as.numeric(gsub("Week ", "", Week)))

#Some rows in Values just say missing instead of NA so I need to fix that 
long_mang <- long_mang %>%
  mutate(
    # Ensure Value is character so "missing" can be detected
    Value = as.character(Value),
    
    # Convert "missing" to NA
    Value = na_if(Value, "missing"),
    
    # Convert to numeric
    Value = as.numeric(Value)
  )


long_mang_NA <- long_mang %>%
  arrange(Propagule_ID, Week) %>%      # make sure data is ordered by ID and week
  group_by(Propagule_ID) %>%
  mutate(
    Value = ifelse(
      is.na(Value),
      (lag(Value) + lead(Value)) / 2,   # average previous and next
      Value
    )
  ) %>%
  ungroup()


#now I need to make sure everything in the location column is cappsed the same 
long_mang_NA <- long_mang_NA %>%
  mutate(
    Location = str_to_title(Location) , # standardizes capitalization
    
    # Standardize Treatment
    Treatment = case_when(
      Treatment %in% c("Treatment 1", "T1") ~ "Treatment 1",
      Treatment %in% c("Treatment 2", "T2") ~ "Treatment 2",
      Treatment %in% c("Control", "C","CTRL", "control") ~ "Control",
      TRUE ~ Treatment  # leave anything else as-is
    )
  )






clean_mangrove_data <- function(filepath) {
  
  # Load data
  mang <- read_excel(filepath)
  
  # Pivot longer
  long_mang <- mang %>%
    pivot_longer(
      cols = starts_with("Week"),
      names_to = "Week",
      values_to = "Value"
    ) %>%
    # Convert Week to numeric
    mutate(Week = as.numeric(gsub("Week ", "", Week))) %>%
    # Handle "missing" values
    mutate(
      Value = as.character(Value),
      Value = na_if(Value, "missing"),
      Value = as.numeric(Value)
    ) %>%
    # Standardize Location and Treatment
    mutate(
      Location = str_to_title(Location),
      Treatment = case_when(
        Treatment %in% c("Treatment 1", "T1") ~ "Treatment 1",
        Treatment %in% c("Treatment 2", "T2") ~ "Treatment 2",
        Treatment %in% c("Control", "C", "CTRL", "control") ~ "Control",
        TRUE ~ Treatment
      )
    ) %>%
    # estimate NAs by averaging previous and next weeks
    arrange(Propagule_ID, Week) %>%
    group_by(Propagule_ID) %>%
    mutate(
      Value = ifelse(
        is.na(Value),
        (lag(Value) + lead(Value)) / 2,
        Value
      )
    ) %>%
    ungroup()
  
  return(long_mang)
}



mang <- clean_mangrove_data("data/messy_mangrove_data.xlsx")
