library(ggplot2)
library(readxl)
library(reshape2)
library(tidyverse)

data <- read_excel("C:/Users/Lalitha Vadlamani/Desktop/180 DC/S2/kpi_over_years_viz/year_dummy.xlsx")
data <- read_excel("C:/Users/souri/OneDrive/Desktop/Work/180 Degrees/shiny-seaside/3_Visualisation_Functions/kpi_over_years_viz/year_dummy.xlsx") 

postEventYearPlot <- function(data){

  # colnames(data) <- tolower(colnames(data))
  # varnames_sorted <- sort(colnames(data))
  # var4 <- varnames_sorted[4]
  # 
  # df <- reshape2::melt(data, id.var = var4)
  
  df = data %>% gather(-Year, key = "KPI", value = "Score") 

  plot <- ggplot(df, aes(x = Year, y = Score, colour = KPI)) + 
    geom_point(color='black') + 
    geom_line() + 
    xlab('Year') + 
    ylab('KPI Score') +
    scale_y_continuous(limits=c(0,1),breaks = seq(0, 1, by = 0.1)) +
    ggtitle("Yearly KPI Scores") +
    guides(color = guide_legend(title = "KPI")) + 
    # scale_color_discrete(labels = c('Action','Learning','Community')) + 
    theme_classic()
  
  return(plot)
}


postEventYearPlot(data) %>% plotly::ggplotly()
