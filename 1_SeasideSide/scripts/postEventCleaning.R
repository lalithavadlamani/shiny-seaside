# library(readxl)
# library(tidyverse)
# library(stringr)
# library(readr)
# library(dplyr)


# data <- read_excel("C:/Users/souri/OneDrive/Desktop/Work/180 Degrees/shiny-seaside/2_Data_Wrangling_Functions/Lalitha/Post event/post_event_responses_v3.xlsx")


full_event_analysis <- function(data,excel_name_ls){
  colnames(data) <- tolower(colnames(data))
  indx_l <- grepl('learning',colnames(data))
  learning_data <- data[indx_l][,1]
  
  learning_options <- parse_number(colnames(learning_data))
  
  colnames(learning_data) <- c('chosen_learning_options')
  
  learning_data$learning_options_per_row <- str_count(learning_data$chosen_learning_options, ',') + 1
  
  learning_data$learning_per_per_row <- (learning_data$learning_options_per_row / learning_options)
  learning_data$learning_per_per_row[is.na(learning_data$learning_per_per_row)] <- 0
  
  indx_c <- grepl('community',colnames(data))
  community_data <- data[indx_c][,1]
  community_options <- parse_number(colnames(community_data))
  
  colnames(community_data) <- c('chosen_community_options')
  
  community_data$community_options_per_row <- str_count(community_data$chosen_community_options, ',') + 1
  
  community_data$community_per_per_row <- (community_data$community_options_per_row / community_options)
  community_data$community_per_per_row[is.na(community_data$community_per_per_row)] <- 0
  
  
  indx_a <- grepl('action',colnames(data))
  action_data <- data[indx_a]
  number_of_actions <- length(colnames(action_data))
  rows <- nrow(action_data)
  action_data_df <- action_data
  
  
  for (i in 1:rows){
    action_data_df[i,'action-Doing_before_the_event_per'] <- (sum(str_count(unlist(action_data[i,]),'Doing before the event')) / number_of_actions) 
    action_data_df[i,'action-Doing_after_the_event_per'] <- (sum(str_count(unlist(action_data[i,]),'Started doing after the event')) / number_of_actions) 
    action_data_df[i,'action-likely_to_do_per'] <- (sum(str_count(unlist(action_data[i,]),'Likely to do in the next 3 months')) / number_of_actions) 
    action_data_df[i,'action-unlikely_to_do_per'] <- (sum(str_count(unlist(action_data[i,]),'Unlikely to do in the next 3 months')) / number_of_actions) 
  }
  
  mat = matrix(ncol = 0, nrow = 0)
  final_df=data.frame(mat)
  
  indx <- grepl('email',colnames(data))
  final_df<- data[indx]
  final_df$learning_kpi <- learning_data$learning_per_per_row
  final_df$community_kpi <- community_data$community_per_per_row 
  final_df$action_Doing_before_the_event <- action_data_df$`action-Doing_before_the_event_per`
  final_df$action_Doing_after_the_event <- action_data_df$`action-Doing_after_the_event_per`
  final_df$action_likely_to_do <- action_data_df$`action-likely_to_do_per`
  final_df$action_unlikely_to_do <- action_data_df$`action-unlikely_to_do_per`
  final_df$env_habits <- data$`On a scale of 1 to 5, where 1 is not engaged at all and 5 is very engaged, how engaged are you in making changes that positively impact the environment?`
  final_df$action_kpi <- (final_df$action_Doing_after_the_event + final_df$action_likely_to_do) / (final_df$action_Doing_after_the_event + final_df$action_likely_to_do + final_df$action_unlikely_to_do)
  
  name_split = strsplit(excel_name_ls, "_", fixed = TRUE)
  date = name_split[[1]][3]
  date_split = strsplit(date, ".", fixed = TRUE)
  final_df <- final_df %>% mutate(location = name_split[[1]][1])
  final_df <- final_df %>% mutate(postcode_event = name_split[[1]][2])
  final_df <- final_df %>% mutate(year = date_split[[1]][3])
  
  
  return(final_df)
}


# full_event_analysis(data,'Bondi_2026_03.02.2021_Post.xlsx')



postprocessing_multiple_fn <- function(dataframe_ls, excel_ls_name) {
  if (length(dataframe_ls) == 1){
    combined_data = full_event_analysis(data = dataframe_ls[[1]], excel_name_ls = excel_ls_name[[1]])
  }else{
    combined_data <- full_event_analysis(dataframe_ls[[1]], excel_ls_name[[1]])
    i = 2
    for (excel_name in excel_ls_name[2:length(excel_ls_name)]) {
      # Changed [i] to [[i]]
      combined_data <- rbind(combined_data, full_event_analysis(dataframe_ls[[i]], excel_name))
      i = i + 1
    }
  }
  return (combined_data)
}


# dataframe_ls = list(data, data, data)
# excel_ls_name = list('Bondi_2026_03.02.2021_Post','Bondi_2025_03.03.2013_Post','Bondi_2026_03.02.2020_Post' )
# postprocessing_multiple_fn(dataframe_ls =dataframe_ls , excel_ls_name = excel_ls_name)