library('readxl')
library(plyr)
library(tidyverse)
library(stringr)
library(readr)
library(tidyr)

df1 <- read_excel("C:/Users/Lalitha Vadlamani/Desktop/180 DC/S2/post_event_responses_v3.xlsx")
df2 <- read_excel("C:/Users/Lalitha Vadlamani/Desktop/180 DC/S2/post_event_responses_v3_1.xlsx")
df3 <- read_excel("C:/Users/Lalitha Vadlamani/Desktop/180 DC/S2/post_event_responses_v3_2.xlsx")

list_of_df<- list(df1,df2,df3)

location_wise_analysis_for_a_year <- function(list_of_df){

pre_process <- function(df){
  colnames(df) <- tolower(colnames(df))
  indx_l <- grepl('learning',colnames(df))
  df$learning_options <- parse_number(colnames(df[indx_l][,1]))
  indx_c <- grepl('community',colnames(df))
  df$community_options <- parse_number(colnames(df[indx_c][,1]))
  df <- df 
  
}

processed_dfs <- lapply(list_of_df,pre_process)

combine_data <- function(list_of_df) {
  
  data <- ldply(list_of_df,data.frame)
  return(data)
  
}

data <- combine_data(processed_dfs)

kpi_analysis <- function(data){

learning_kpi <- function(data){
  indx <- grepl('learning',colnames(data))
  learning_data  <- data[indx]
  learning_data_temp = subset(learning_data, select = -c(learning_options))
  
  learning_data_temp <- learning_data_temp %>% unite('chosen_learning_options',na.rm=TRUE)
  learning_data$chosen_learning_options <- learning_data_temp$chosen_learning_options
  
  learning_data <- learning_data[c('chosen_learning_options','learning_options')]
  
  learning_data$learning_options_per_row <- str_count(learning_data$chosen_learning_options, ',') + 1 
  
  learning_data$learning_per_per_row <- (learning_data$learning_options_per_row / learning_data$learning_options)*100
  
  l_kpi <- mean(learning_data$learning_per_per_row)
  
  return(l_kpi)
}


community_kpi <- function(data) {
  
  indx <- grepl('community',colnames(data))
  community_data  <- data[indx]
  community_data_temp = subset(community_data, select = -c(community_options))
  
  community_data_temp <- community_data_temp %>% unite('chosen_community_options',na.rm=TRUE)
  community_data$chosen_community_options <- community_data_temp$chosen_community_options
  
  community_data <- community_data[c('chosen_community_options','community_options')]
  
  community_data$community_options_per_row <- str_count(community_data$chosen_community_options, ',') + 1 
  
  community_data$community_per_per_row <- (community_data$community_options_per_row / community_data$community_options)*100
  
  c_kpi <- mean(community_data$community_per_per_row)
  
  return(c_kpi)
}




action_kpi <- function(data){
  
  indx <- grepl('action',colnames(data))
  action_data <- data[indx]
  number_of_actions <- length(colnames(action_data))
  
  doing_before = 0 
  doing_after = 0 
  likely_do = 0 
  unlikely_do = 0 
  
  for (i in 1:number_of_actions){
    doing_before <- doing_before + sum(str_count(unlist(action_data[,i],),'Doing before the event'),na.rm=TRUE)
    doing_after <- doing_after + sum(str_count(unlist(action_data[,i]),'Started doing after the event'),na.rm=TRUE)
    likely_do <- likely_do + sum(str_count(unlist(action_data[,i]),'Likely to do in the next 3 months'),na.rm=TRUE)
    unlikely_do <- unlikely_do + sum(str_count(unlist(action_data[,i]),'Unlikely to do in the next 3 months'),na.rm=TRUE)
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


return(list(learning_kpi(data),community_kpi(data),action_kpi(data)))
}


unique_locations <- unique(data[c('location')])
no_of_unique_locations <- nrow(unique_locations)

all_loc_analysis <- vector()
loc_names <- c()
mat = matrix(ncol = 0, nrow = 0)
final_df=data.frame(mat)

for (i in 1:no_of_unique_locations){
  
  location_name = unique_locations[i,1]
  final_df[i,'location'] <- location_name
  # loc_names <- c(loc_names,location_name)
  data_required <- subset(data ,location == location_name)
  
  loc_analysis <- kpi_analysis(data_required)
  
  # all_loc_analysis <- c(all_loc_analysis,list(loc_analysis))
  
  final_df[i,'learning_kpi'] <- loc_analysis[1]
  final_df[i,'community_kpi'] <- loc_analysis[2]
  temp <- loc_analysis[[3]]
  final_df[i,'action_doing_before_the_event'] <- temp[,'Doing before event']
  final_df[i,'action_doing_after_the_event'] <- temp[,'Doing after event']
  final_df[i,'action_likely_to_do'] <- temp[,'Likely to do']
  final_df[i,'action_unlikely_to_do'] <- temp[,'Unlikely to do']
  final_df[i,'env_habits'] <- mean(data_required$on.a.scale.of.1.5..how.would.you.rate.your.environmental.habits.after.the.activity.,na.rm=TRUE)
  
}

# names(all_loc_analysis) <- loc_names

return(final_df)
}

location_wise_analysis_for_a_year(list_of_df)
