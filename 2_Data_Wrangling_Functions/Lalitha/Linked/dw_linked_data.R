library('readxl')
library(plyr)
library(tidyverse)
library(stringr)
library(readr)
library(tidyr)
library(dplyr)

post <- read_excel("C:/Users/Lalitha Vadlamani/Desktop/180 DC/S2/post_event_responses_v3.xlsx")
pre <- read_excel("C:/Users/Lalitha Vadlamani/Desktop/180 DC/S2/pre-event_responses_v2.xlsx")

colnames(pre)
colnames(post)

linked <- function(pre,post){
linked_data <- merge(x=pre,y=post,by.x = 'Email',by.y = 'Email Address') 


indx <- grepl('scale',colnames(linked_data))
comparison_data <- linked_data[indx]

colnames(comparison_data) <- c('Before activity','After activity')

comparison_data[is.na(comparison_data)] <- 1

comparison_data$percentage_change <- round(((as.numeric(comparison_data$`After activity`) - as.numeric(comparison_data$`Before activity`)) / as.numeric(comparison_data$`Before activity`))*100,2) 

comparison_data$email <- linked_data$Email


return(list(comparison_data,linked_data))

}

linked(pre,post)
