library(readxl)
library(plotly)
library(janitor)
library(tidyverse)



# definitions
attributes_ls <- c("Email", "What pronoun do you identify with?", "Age", "Postcode", "How many people under (5) years are you signing up for?", "How many people in the age range(5-10) are you signing up for?", "How many people in the age range(11-20) are you signing up for?", "How many people in the age range(21-30) are you signing up for?", "How many people in the age range(31-50) are you signing up for?", "How many people in the age range(51-70) are you signing up for?", "How many people in this age range(71+) are you signing up for?", "Have you previously attended an organised clean-up event (hosted by any organisation)?", "On a scale of 1-5 how invested are you in environmental impact?(1 being the least invested)", "How did you find out about this event?" ) 
colnames_ls <- c("email", "pronoun", "age", "postcode", "age_under_5", "age_5_to_10","age_11_to_20","age_21_to_30","age_31_to_50","age_51_to_70","age_over_71", "previous_attendance", "invested_environmental_impact", "find_out_event" )
total_colnames_ls <- c("email", "pronoun", "age", "postcode", "age_under_5", "age_5_to_10","age_11_to_20","age_21_to_30","age_31_to_50","age_51_to_70","age_over_71", "previous_attendance", "invested_environmental_impact", "find_out_event", "age_group", "location", "year")
merge_colnames_ls <- c("location", "year")
age_range_options <- c("age_under_5", "age_5_to_10", "age_11_to_20", "age_21_to_30", "age_31_to_50", "age_51_to_70", "age_over_71")
numeric_ls <- c("age_under_5", "age_5_to_10", "age_11_to_20", "age_21_to_30", "age_31_to_50", "age_51_to_70", "age_over_71", "age", "invested_environmental_impact")
factor_ls <- c("pronoun", "previous_attendance", "find_out_event")
character_ls <- c("email", "postcode")


# Helper Cleaners - Datatype conversion

## Converting NULL's


nullConversion = function(x){
  x %>% 
    mutate_all(function(x) ifelse(is.null(unlist(x)), NA, unlist(x)))
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
preprocessing_fn <- function(pre_event_all = dataframe_ls[[1]],excel_name_ls = excel_ls_name[[1]]) {

  pre_event_data = subset(pre_event_all, select = attributes_ls)
  colnames(pre_event_data) <- colnames_ls
  
  pre_event_data_numeric = as.data.frame(lapply(nullConversion(pre_event_data[, numeric_ls]), valid_numeric))
  
  pre_event_data = pre_event_data[, !(names(pre_event_data) %in% numeric_ls)]
  
  pre_event_data$postcode = valid_postcode(pre_event_data$postcode)
  
  pre_event_data = cbind(pre_event_data, pre_event_data_numeric)
  
  # creates new column for correct age bin of eventbrite responders
  pre_event_data$age_group <- cut(pre_event_data$age, breaks = c(0, 5, 10, 20, 30, 50, 70, Inf), labels = age_range_options,  right = FALSE, include.lowest = TRUE)
  
  
  # adds location and year as columns
  name_split = strsplit(excel_name_ls, "_", fixed = TRUE)
  date = name_split[[1]][2]
  date_split = strsplit(date, ".", fixed = TRUE)
  pre_event_data <- pre_event_data %>% mutate(location = name_split[[1]][1])
  pre_event_data <- pre_event_data %>% mutate(year = date_split[[1]][3])

  return (pre_event_data)
  
}


## Processing multiple datasets

preprocessing_multiple_fn <- function(dataframe_ls, excel_ls_name) {
  # Changed [1] to [[1]]
  if (length(dataframe_ls) == 1){
    combined_data = preprocessing_fn(dataframe_ls[[1]], excel_ls_name[[1]])
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







# Viz functions
PreEventPlot <- function(data, varNames) {
  
  title = paste(varNames[1], "Barplot") %>% str_to_title()
  yaxislabel = paste("Number of Responses") %>% str_to_title()
  if(length(varNames) == 1) {
    # This line fixes it
    
    if(varNames == ("age_group")){
      # Mukund's Code
      xaxislabel = ("Age Groups")
      legendlabel = paste(varNames[2])
      data2=select(data, -email, -pronoun, -previous_attendance, -find_out_event, -age_group)
      glimpse(data2)
      
      data2[is.na(data2)]  <- 0
      
      glimpse(data2)
      
      data2_agesums<- colSums(data2[3:9])
      age_count<- c(data2_agesums)
      age_df<- data.frame(age_count)
      
      age_df %>%
        rownames_to_column("groups") %>%
        ggplot(aes(x = groups, y = age_count)) + 
        geom_bar(stat = "identity", fill='skyblue1')+
        ggtitle(sub("_", " ", title))+ 
        theme(plot.title=element_text(hjust=0.5)) + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5))  +
        theme_minimal() + 
        scale_x_discrete(limits = c("age_under_5","age_5_to_10","age_11_to_20", 
                                    "age_21_to_30","age_31_to_50",  "age_51_to_70", "age_over_71"),
                         labels = c("Age Under 5", "Age 5 to 10", "Age 11 to 20", "Age 21 to 30", "Age 31 to 50", 
                                    "Age 51 to 70", "Age Over 71")) +
        labs(y=str_to_title(yaxislabel), x = str_to_title(xaxislabel), fill = legendlabel)
      
      
    }else{
      
      data = data %>% dplyr::rename(var1 = varNames[[1]])
      xaxislabel = paste(varNames[1])
      legendlabel = paste(varNames[2])
      data  %>% ggplot(aes(x = reorder(var1, var1, function(x)-length(x)))) + 
        theme_minimal() +
        geom_bar(fill="skyblue1") + 
        labs(y=str_to_title(yaxislabel), x = str_to_title(xaxislabel)) + 
        ggtitle(sub("_", " ", title))
    }
    
    
  } else {
    # This line fixes it
    data = data %>% dplyr::rename(var1 = varNames[[1]], var2 = varNames[[2]])
    xaxislabel = paste(varNames[1])
    legendlabel = paste(varNames[2])
    data %>% ggplot(aes(x = reorder(var1, var1, function(x)-length(x)), fill = var2)) + 
      geom_bar() + labs(y= str_to_title(yaxislabel), x = str_to_title(sub("_", " ", xaxislabel)))+labs(fill=legendlabel)+
      ggtitle(sub("_", " ", title)) + 
      theme(plot.title=element_text(hjust=0.5)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
      theme_minimal()
    
  }
  
  
}


