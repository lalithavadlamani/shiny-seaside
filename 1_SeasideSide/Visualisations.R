








# Prevent - Barplot 

PreEventPlot <- function(data, varNames) {
  
  title = paste(varNames[1], "Barplot") %>% str_to_title()
  yaxislabel = paste("Number of Responses") %>% str_to_title()
  if(length(varNames) == 1) {
    # This line fixes it
    
    if(varNames == ("age_group")){
      # Mukund's Code
      xaxislabel = ("Age Groups")
      legendlabel = paste(varNames[2])
      data2=select(data, -email, -pronoun, -previous_attendance, -find_out_event, -age_group)
      glimpse(data2)
      
      data2[is.na(data2)]  <- 0
      
      glimpse(data2)
      
      data2_agesums<- colSums(data2[3:9])
      age_count<- c(data2_agesums)
      age_df<- data.frame(age_count)
      
      age_df %>%
        rownames_to_column("groups") %>%
        ggplot(aes(x = groups, y = age_count)) + 
        geom_bar(stat = "identity", fill='skyblue1')+
        ggtitle(sub("_", " ", title))+ 
        theme(plot.title=element_text(hjust=0.5)) + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5))  +
        theme_minimal() + 
        scale_x_discrete(limits = c("age_under_5","age_5_to_10","age_11_to_20", 
                                    "age_21_to_30","age_31_to_50",  "age_51_to_70", "age_over_71"),
                         labels = c("Age Under 5", "Age 5 to 10", "Age 11 to 20", "Age 21 to 30", "Age 31 to 50", 
                                    "Age 51 to 70", "Age Over 71")) +
        labs(y=str_to_title(yaxislabel), x = str_to_title(xaxislabel), fill = legendlabel)
      
      
    }else{
      
      data = data %>% dplyr::rename(var1 = varNames[[1]])
      xaxislabel = paste(varNames[1])
      legendlabel = paste(varNames[2])
      data  %>% ggplot(aes(x = reorder(var1, var1, function(x)-length(x)))) + 
        theme_minimal() +
        geom_bar(fill="skyblue1") + 
        labs(y=str_to_title(yaxislabel), x = str_to_title(xaxislabel)) + 
        ggtitle(sub("_", " ", title))
    }
    
    
  } else {
    # This line fixes it
    data = data %>% dplyr::rename(var1 = varNames[[1]], var2 = varNames[[2]])
    xaxislabel = paste(varNames[1])
    legendlabel = paste(varNames[2])
    data %>% ggplot(aes(x = reorder(var1, var1, function(x)-length(x)), fill = var2)) + 
      geom_bar() + labs(y= str_to_title(yaxislabel), x = str_to_title(sub("_", " ", xaxislabel)))+labs(fill=legendlabel)+
      ggtitle(sub("_", " ", title)) + 
      theme(plot.title=element_text(hjust=0.5)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
      theme_minimal()
    
  }
  
  
}





# Map 
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(rgdal)
require(plyr)



postcodes_loc <- read_csv("australian_postcodes.csv")
test <- read_csv("pre_event_cleaned_dummy.csv")


map_ages <- function(df){
  postcodes_loc <- read_csv("australian_postcodes.csv")
  df$postcode = as.character(df$postcode)
  df_joined <- left_join(df, postcodes_loc, by = c("postcode" = "postcode"))
  # df_joined <- df_joined %>% dplyr::distinct(...1, .keep_all = TRUE)
  
  df_clean = df_joined[!is.na(df_joined$age),]
  df_clean$age_group <- factor(df_clean$age_group, levels = c("age_under_5", "age_11_to_20", "age_21_to_30",
                                                              "age_31_to_50", "age_51_to_70",  "age_over_71"))
  df_clean$age_group <- mapvalues(df_clean$age_group, 
                                  from = c("age_under_5", "age_11_to_20", "age_21_to_30",
                                           "age_31_to_50", "age_51_to_70", "age_over_71"), 
                                  to = c("Under 5", "11 to 20", "21 to 30",
                                         "31 to 50", "51 to 70", "Over 71"))
  df_clean.df <- split(df_clean, df_clean$age_group)
  
  l <- leaflet() %>% addTiles()
  
  names(df_clean.df) %>%
    purrr::walk(function(df) {
      l <<- l %>%
        addMarkers(data=df_clean.df[[df]],
                   lng=~long, lat=~lat,
                   label=~age,
                   popup=~age,
                   group = df,
                   clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                   labelOptions = labelOptions(noHide = F,
                                               direction = 'auto'))
    })
  
  l %>%
    addLayersControl(
      overlayGroups = names(df_clean.df),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    htmlwidgets::onRender("
          function() {
              $('.leaflet-control-layers-list').prepend('<label style=\"text-align:center\">Age Groups</label>');
          }
      ")
}