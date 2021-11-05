library(ggplot2)
library('readxl')
library(reshape2)

data <- read_excel("C:/Users/Lalitha Vadlamani/Desktop/180 DC/S2/Stratified/stratified_input_data.xlsx")

data


bleh <- function(data,kpi_name,demographic_name){
  
  colnames(data) <- tolower(colnames(data))
  varnames_sorted <- sort(colnames(data))
  name_kpi = paste('KPI -',kpi_name)
  plot <- ggplot(data, aes(x = year, y = value, colour = demographic)) + 
    geom_point(color='black') + 
    geom_line() + 
    xlab('Year') + 
    ylab(name_kpi) + 
    guides(color = guide_legend(title = demographic_name)) + 
    theme_classic()
  
  return(plot)
  
}

bleh(data,'Action','Pronoun')


