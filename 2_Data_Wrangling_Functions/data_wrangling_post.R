# # Read me 
# 
#    Variable name                  Value stored 
#   
# 1.  clean_data                    Datrame of cleaned data i.e. with only required columns for analysis from the original file 
# 2.  first_activity_table          Table with proportion values of people who responded yes/no to the question 'Is this your first waste education activity?
# 3.  biggest_takeaway_kpi_table    Table with KPI % for the question 'What were your biggest takeaways from the activity?'
# 4.  env_habits_table              Table with proportion values of people who choose 1-5 for the question 'On a scale of 1-5, how would you rate your environmental habits after the activity?'
# 5.  preferred_channel_table       Table with proportion values of people who prefer the corresponsing channel to find about upcoming events
# 6.  action_kpi_table              Table with percentages of Action KPI for different categories 
# ############################################################################################

library('readxl')
library(tidyverse)
library(stringr)

data <- read_excel("C:/Users/julia/OneDrive/Desktop/shiny-seaside/post-event_responses_v2.xlsx")

# Check point
class(data)
names(data)


# Function to clean data : { remove unnecessary columns; rename columns }
preprocess <- function(data){
  df <- data[-c(1,12:22)] 
  colnames(df) <- c('first_activity','takeaway','use_hand2','use_lp','compost','drop_waste','contact_council','use_skills','env_habits','channel_preferred')
  return(df)
}

df <- preprocess(data) # Cleaned data 
clean_data <- df 

# Function to return percentage of people who responded yes/no to the question Is this your first waste education activity?
FA <- function(column){
  val <- table(column)
  return(val)
}
 
first_activity_table <- prop.table(FA(df$first_activity))
# first_activity_table

# Function to return KPI values for the question biggest takeaway 
count_takeaways <- function(column){
  action1 <- sum(str_count(column, "Changed my waste management habits"))
  learning1 <- sum(str_count(column, "Learnt more about behaviours I can change to reduce my waste"))
  community1 <- sum(str_count(column, "Had a fun time"))
  learning2 <- sum(str_count(column, "Became more aware about local groups advocating for waste reduction"))
  learning3 <- sum(str_count(column, "Learnt about complexity of recycling e.g. through Defy Design workshop activity"))
  community2 <- sum(str_count(column, "Connected with people and colleagues"))
  
  # Average counts
  action_count = action1  
  learning_count = (learning1 + learning2 + learning3) / 3
  community_count = (community1 + community2) / 2
  
  total_people = length(column)
  
  # Percentages 
  action_per = round((action_count/total_people) * 100,2)
  learning_per = round((learning_count/total_people) * 100,2)
  community_per = round((community_count/total_people) * 100,2)
  
   percentages <- matrix(c(action_per,learning_per,community_per),ncol=3,byrow = TRUE)
   colnames(percentages) <- c('Action','Learning','Community')
   rownames(percentages) <- c('Percentage')
   return(as.table(percentages))
}
 
biggest_takeaway_kpi_table <- count_takeaways(df$takeaway)

#Function to return count of people for each number on the scale 1-5 for environmental habits 
env_habits_scale <- function(column){
  val <- table(column)
  return (val)
} 

env_habits_table <- prop.table(env_habits_scale(df$env_habits))

# Function for How would you prefer to find out about Seaside Scavenge events?
channel <- function(column){
  val <- table(column)
  return(val)
}

preferred_channel_table <- prop.table(channel(df$channel_preferred))


# Function for Action kPI 
action_kpi <- function(df){
  uh2 <- table(df$use_hand2)
  ulp <- table(df$use_lp)
  cp <- table(df$compost)
  dw <- table(df$drop_waste)
  cc <- table(df$contact_council)
  us <- table(df$use_skills)
  
  doing_before_event <- (uh2[names(uh2)=='Doing before the event'] +
                        ulp[names(ulp)=='Doing before the event'] +
                        cp[names(cp)=='Doing before the event'] +
                        dw[names(dw)=='Doing before the event'] +
                        cc[names(cc)=='Doing before the event'] +
                        us[names(us)=='Doing before the event'] ) / 6
  
  doing_after_event <- (uh2[names(uh2)=='Started doing after the event'] +
                       ulp[names(ulp)=='Started doing after the event'] +
                       cp[names(cp)=='Started doing after the event'] +
                       dw[names(dw)=='Started doing after the event'] +
                       cc[names(cc)=='Started doing after the event'] +
                       us[names(us)=='Started doing after the event'] ) / 6
  
  likely_to_do <- (uh2[names(uh2)=='Likely to do in the next 3 months'] +
                  ulp[names(ulp)=='Likely to do in the next 3 months'] +
                  cp[names(cp)=='Likely to do in the next 3 months'] +
                  dw[names(dw)=='Likely to do in the next 3 months'] +
                  cc[names(cc)=='Likely to do in the next 3 months'] +
                  us[names(us)=='Likely to do in the next 3 months'] ) / 6
  
  unlikely_to_do <- (uh2[names(uh2)=='Unlikely to do in the next 3 months'] +
                    ulp[names(ulp)=='Unlikely to do in the next 3 months'] +
                    cp[names(cp)=='Unlikely to do in the next 3 months'] +
                    dw[names(dw)=='Unlikely to do in the next 3 months'] +
                    cc[names(cc)=='Unlikely to do in the next 3 months'] +
                    us[names(us)=='Unlikely to do in the next 3 months']) / 6
 
  total_people = nrow(df)
  
  # Percentages 
  doing_before_event_per = round((doing_before_event/total_people) * 100,2)
  doing_after_event_per = round((doing_after_event/total_people) * 100,2)
  likely_to_do_per = round((likely_to_do/total_people) * 100,2)
  unlikely_to_do_per = round((unlikely_to_do/total_people) * 100,2)
  
  percentages <- matrix(c(doing_before_event_per,doing_after_event_per,likely_to_do_per,unlikely_to_do_per),ncol=4,byrow = TRUE)
  colnames(percentages) <- c('Doing before event','Doing after event','Likely to do','Unlikely to do')
  rownames(percentages) <- c('Percentage')
  return(as.table(percentages))
} 

action_kpi_table <- action_kpi(df)

print(action_kpi_table)

print(biggest_takeaway_kpi_table)
