library(readxl)
library(plotly)
library(janitor)
library(tidyverse)



# definitions
attributes_ls <- c("Email", "What pronoun do you identify with?", "Age", "Postcode", "How many people under (5) years are you signing up for?", "How many people in the age range(5-10) are you signing up for?", "How many people in the age range(11-20) are you signing up for?", "How many people in the age range(21-30) are you signing up for?", "How many people in the age range(31-50) are you signing up for?", "How many people in the age range(51-70) are you signing up for?", "How many people in this age range(71+) are you signing up for?", "Have you previously attended an organised clean-up event (hosted by any organisation)?", "On a scale of 1-5 how invested are you in environmental impact?(1 being the least invested)", "How did you find out about this event?" ) 
colnames_ls <- c("email", "pronoun", "age", "postcode", "age_under_5", "age_5_to_10","age_11_to_20","age_21_to_30","age_31_to_50","age_51_to_70","age_over_71", "previous_attendance", "pre_environmental_impact", "find_out_event", "origin"  )
total_colnames_ls <- c("email", "pronoun", "age", "postcode", "age_under_5", "age_5_to_10","age_11_to_20","age_21_to_30","age_31_to_50","age_51_to_70","age_over_71", "previous_attendance", "pre_environmental_impact", "find_out_event", "age_group", "location", "year", "postcode_event",  "origin")
merge_colnames_ls <- c("location", "year")
age_range_options <- c("age_under_5", "age_5_to_10", "age_11_to_20", "age_21_to_30", "age_31_to_50", "age_51_to_70", "age_over_71")
numeric_ls <- c("age_under_5", "age_5_to_10", "age_11_to_20", "age_21_to_30", "age_31_to_50", "age_51_to_70", "age_over_71", "age")
factor_ls <- c("pronoun", "previous_attendance", "find_out_event",  "pre_environmental_impact")
character_ls <- c("email", "postcode")


# Helper Cleaners - Datatype conversion

## Converting NULL's


nullConversion = function(x){
    x %>% 
    rowwise() %>% 
    mutate_all(function(x) ifelse(is.null(unlist(x)), NA, as.character(unlist(x))))
}

## converts required values to numeric
valid_numeric <- function(input) {
  input[vapply(input, is.null, TRUE)] = NA
  input = unlist(input)
  input_numeric = as.numeric(input)
  input_numeric = ifelse(input_numeric >= 0, input_numeric, NA)    
  
  return (input_numeric)
}


# validates postcode values
valid_postcode <- function(input) {
  input[vapply(input, is.null, TRUE)] = NA
  input = unlist(input)
  input_numeric = as.numeric(input)
  input_numeric = ifelse(input_numeric >= 1000 & input_numeric <=9999, input_numeric, NA)
  
  return (as.character(input_numeric))
}


## Processing one dataset
preprocessing_fn <- function(pre_event_all = dataframe_ls,excel_name_ls = excel_ls_name) {

  pre_event_data = subset(pre_event_all, select = attributes_ls)
  colnames(pre_event_data) <- colnames_ls
  
  pre_event_data_numeric = as.data.frame(lapply(nullConversion(pre_event_data[, numeric_ls]), valid_numeric))
  
  pre_event_data = pre_event_data[, !(names(pre_event_data) %in% numeric_ls)]
  
  pre_event_data$postcode = valid_postcode(pre_event_data$postcode)
  
  pre_event_data = cbind(pre_event_data, pre_event_data_numeric)
  
  # creates new column for correct age bin of eventbrite responders
  pre_event_data$age_group <- cut(pre_event_data$age, breaks = c(0, 5, 10, 20, 30, 50, 70, Inf), labels = age_range_options,  right = FALSE, include.lowest = TRUE) %>% 
    factor(age_range_options)

 # Convert invested enviro impact to factor
  pre_event_data$pre_environmental_impact = factor(pre_event_data$pre_environmental_impact, levels = c(1,2,3,4,5))
  
  ### Adding extra entries of people as rows ###
  section = (pre_event_data %>% select(postcode, age_range_options, age_group))
  for (num in 1:length(age_range_options)) {
    single_age_section = section %>% filter(!!sym(age_range_options[num]) > 0)
    for (row in 1:nrow(single_age_section)) {
      duplicates <- single_age_section[row, age_range_options[num]]
      for (duplicate_no in 1:duplicates) {
        pre_event_data = pre_event_data %>% add_row(postcode = 
                                                      single_age_section[row, "postcode"],
                                                    age_group = age_range_options[num])      
      }
    }    
  }
  
  # adds location and year as columns
  name_split = strsplit(excel_name_ls, "_", fixed = TRUE)
  date = name_split[[1]][3]
  date_split = strsplit(date, ".", fixed = TRUE)
  pre_event_data <- pre_event_data %>% mutate(location = name_split[[1]][1])
  pre_event_data <- pre_event_data %>% mutate(postcode_event = name_split[[1]][2])
  
  pre_event_data <- pre_event_data %>% mutate(year = date_split[[1]][3])
  
  return (pre_event_data)
  
}


## Processing multiple datasets

preprocessing_multiple_fn <- function(dataframe_ls, excel_ls_name) {
  # Changed [1] to [[1]]
  if (length(dataframe_ls) == 1){
    combined_data = preprocessing_fn(pre_event_all = dataframe_ls[[1]], excel_name_ls = excel_ls_name[[1]])
  }else{
    combined_data <- preprocessing_fn(dataframe_ls[[1]], excel_ls_name[[1]])
    i = 2
    for (excel_name in excel_ls_name[2:length(excel_ls_name)]) {
      # Changed [i] to [[i]]
      combined_data <- rbind(combined_data, preprocessing_fn(dataframe_ls[[i]], excel_name))
      i = i + 1
    }
  }
  return (combined_data)
}



