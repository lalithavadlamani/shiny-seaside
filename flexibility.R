library('readxl')
library(tidyverse)
library(stringr)
library(readr)
library(stringr)


data <- read_excel("C:/Users/Lalitha Vadlamani/Desktop/180 DC/S2/post_event_responses_v3.xlsx")
data
names(data)


learning_kpi <- function(data){

  colnames(data) <- tolower(colnames(data))
  indx <- grepl('learning',colnames(data))
  learning_data <- data[indx][,1]
  
  learning_options <- parse_number(colnames(learning_data))
  
  colnames(learning_data) <- c('chosen_learning_options')
  
  learning_data$learning_options_per_row <- str_count(learning_data$chosen_learning_options, ',') + 1
  
  learning_data$learning_per_per_row <- (learning_data$learning_options_per_row / learning_options)*100
  
  l_kpi <- mean(learning_data$learning_per_per_row)
  
  return(l_kpi)
  
}

learning_kpi(data)

community_kpi <- function(data){

  colnames(data) <- tolower(colnames(data))
  indx <- grepl('community',colnames(data))
  community_data <- data[indx][,1]
  community_options <- parse_number(colnames(community_data))
  
  colnames(community_data) <- c('chosen_community_options')
  
  community_data$community_options_per_row <- str_count(community_data$chosen_community_options, ',') + 1
  
  community_data$community_per_per_row <- (community_data$community_options_per_row / community_options)*100
  
  c_kpi <- mean(community_data$community_per_per_row,na.rm = TRUE)
  
  return(c_kpi)

}

community_kpi(data)

action_kpi <- function(data){
  
  colnames(data) <- tolower(colnames(data))
  indx <- grepl('action',colnames(data))
  action_data <- data[indx]
  number_of_actions <- length(colnames(action_data))
  
  doing_before = 0 
  doing_after = 0 
  likely_do = 0 
  unlikely_do = 0 
  
  for (i in 1:number_of_actions){
  doing_before <- doing_before + sum(str_count(unlist(action_data[,i]),'Doing before the event'))
  doing_after <- doing_after + sum(str_count(unlist(action_data[,i]),'Started doing after the event'))
  likely_do <- likely_do + sum(str_count(unlist(action_data[,i]),'Likely to do in the next 3 months'))
  unlikely_do <- unlikely_do + sum(str_count(unlist(action_data[,i]),'Unlikely to do in the next 3 months'))
  }
  
  
  doing_before_per <- round((doing_before / (nrow(action_data) * ncol(action_data) ))*100,2)
  doing_after_per <- round((doing_after / (nrow(action_data) * ncol(action_data) ))*100,2)
  likely_do_per <- round((likely_do / (nrow(action_data) * ncol(action_data) ))*100,2)
  unlikely_do_per <- round((unlikely_do / (nrow(action_data) * ncol(action_data) ))*100,2)
  
  percentages <- matrix(c(doing_before_per,doing_after_per,likely_do_per,unlikely_do_per),ncol=4,byrow = TRUE)
  colnames(percentages) <- c('Doing before event','Doing after event','Likely to do','Unlikely to do')
  rownames(percentages) <- c('Percentage')
  
  return(as.table(percentages))
}

action_kpi(data)