library(ggplot2)
library('readxl')
library(reshape2)

data <- read_excel("C:/Users/Lalitha Vadlamani/Desktop/180 DC/S2/kpi_over_years_viz/year_dummy.xlsx")


all_year_plot <- function(data){

  colnames(data) <- tolower(colnames(data))
  varnames_sorted <- sort(colnames(data))
  var4 <- varnames_sorted[4]
  
  df <- reshape2::melt(data, id.var = var4)
  
  plot <- ggplot(df, aes(x = year, y = value, colour = variable)) + 
    geom_point(color='black') + 
    geom_line() + 
    xlab('Year') + 
    ylab('KPI value') +
    scale_y_continuous(limits=c(0,1),breaks = seq(0, 1, by = 0.1)) +
    ggtitle("KPI's over the years") +
    guides(color = guide_legend(title = "KPI")) + 
    scale_color_discrete(labels = c('Action','Learning','Community')) + 
    theme_classic()
  
  return(plot)
}


all_year_plot(data)
