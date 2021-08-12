library('readxl')
library(tidyverse)
library(stringr)

data <- read_excel("C:/Users/Lalitha Vadlamani/Desktop/180 DC/S2/post-event_responses_v2.xlsx")

# Check point
class(data)
names(data)


# Function to clean data : { remove unnecessary columns; rename columns }
preprocess <- function(data){
  df <- data[-c(1,12:22)] 
  colnames(df) <- c('first_activity','takeaway','use_hand2','use_lp','compost','drop_waste','contact_council','use_skills','env_habits','channel_preferred')
  return(df)
}

# Checkpoint: Uncomment below line 
#df <- preprocess(data)

# Function to return percentage of people who responded yes/no to the question Is this your first waste education activity?
FA <- function(column){
  val <- table(column)
  yes <- val[names(val)=='Yes']
  no <- val[names(val)=='No']
  
  yes_per = (yes/length(column)) * 100
  no_per = (no/length(column)) * 100 
  
  return(c(yes_per,no_per))
}
# Checkpoint: Uncomment below line 
#FA(df$first_activity)

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
  learning_count = (learning2 + learning2 + learning3) / 3
  community_count = (community1 + community2) / 2
  
  total_people = length(column)
  
  # Percentages 
  action_per = round((action_count/total_people) * 100,2)
  learning_per = round((learning_count/total_people) * 100,2)
  community_per = round((community_count/total_people) * 100,2)
  
  return(c(action_per,learning_per,community_per))
}
# Checkpoint: Uncomment below line 
#count_takeaways(df$takeaway)

#Function to return count of people for each number on the scale 1-5 for environmental habits 
env_habits_scale <- function(column){
  val <- table(column)
  one <- val[names(val)==1]
  two <- val[names(val)==2]
  three <- val[names(val)==3]
  four <- val[names(val)==4]
  five <- val[names(val)==5]
  return(c(one,two,three,four,five))
} # Note: To club to 2 or 3 categories accordingly 
# Checkpoint: Uncomment below line 
#env_habits_scale(df$env_habits)

# Function for How would you prefer to find out about Seaside Scavenge events?
channel <- function(column){
  val <- table(column)
  cw <- val[names(val)=='Council Website']
  email <- val[names(val)=='Email']
  eb <- val[names(val)=='Eventbrite']
  fb <- val[names(val)=='Facebook']
  insta <- val[names(val)=='Instagram']
  pb <- val[names(val)=='Passing By']
  ssw <- val[names(val)=='Seaside Scavenge website']
  
  return(c(cw,email,eb,fb,insta,pb,ssw))
}
# Checkpoint: Uncomment below line 
#channel(df$channel_preferred)

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
                        us[names(us)=='Doing before the event'] ) 
  
  doing_after_event <- (uh2[names(uh2)=='Started doing after the event'] +
                       ulp[names(ulp)=='Started doing after the event'] +
                       cp[names(cp)=='Started doing after the event'] +
                       dw[names(dw)=='Started doing after the event'] +
                       cc[names(cc)=='Started doing after the event'] +
                       us[names(us)=='Started doing after the event'] ) 
  
  likely_to_do <- (uh2[names(uh2)=='Likely to do in the next 3 months'] +
                  ulp[names(ulp)=='Likely to do in the next 3 months'] +
                  cp[names(cp)=='Likely to do in the next 3 months'] +
                  dw[names(dw)=='Likely to do in the next 3 months'] +
                  cc[names(cc)=='Likely to do in the next 3 months'] +
                  us[names(us)=='Likely to do in the next 3 months'] ) 
  
  unlikely_to_do <- (uh2[names(uh2)=='Unlikely to do in the next 3 months'] +
                    ulp[names(ulp)=='Unlikely to do in the next 3 months'] +
                    cp[names(cp)=='Unlikely to do in the next 3 months'] +
                    dw[names(dw)=='Unlikely to do in the next 3 months'] +
                    cc[names(cc)=='Unlikely to do in the next 3 months'] +
                    us[names(us)=='Unlikely to do in the next 3 months']) 
 
   return(c(doing_before_event,doing_after_event,likely_to_do,unlikely_to_do))
} # Note: Need to modify to return percentages

# Checkpoint : Uncomment below line
#action_kpi(df)

# -------------------------------------------------------------------------------------------------------------
# Note: First draft only :3

#Add your thoughts or changes required below:
# 
# 
# 
# 
# 
# 
# 
# 
# 
#
#
