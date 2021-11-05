library(ggplot2)
library('readxl')
library(reshape2)

data <- read_excel("C:/Users/souri/OneDrive/Desktop/Work/180 Degrees/shiny-seaside/3_Visualisation_Functions/Stratified/stratified_input_data.xlsx")

data


yearlyStratifiedVizPlot <- function(data,kpi_name,demographic_name){
  
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

yearlyStratifiedVizPlot(data,'Action','Pronoun')

yearlyStratifiedVizPlot(data, kpi_name = input$stratifiedEventKPI, demographic_name = input$stratifiedVarColour)


