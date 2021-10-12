








# Prevent - Barplot 

# PreEventPlot <- function(data, varNames) {
#   
#   title = paste(varNames[1], "Barplot") %>% str_to_title()
#   yaxislabel = paste("Number of Responses") %>% str_to_title()
#   if(length(varNames) == 1) {
#     # This line fixes it
#     
#     if(varNames == ("age_group")){
#       # Mukund's Code
#       xaxislabel = ("Age Groups")
#       legendlabel = paste(varNames[2])
#       data2=select(data, -email, -pronoun, -previous_attendance, -find_out_event, -age_group)
#       glimpse(data2)
#       
#       data2[is.na(data2)]  <- 0
#       
#       glimpse(data2)
#       
#       data2_agesums<- colSums(data2[3:9])
#       age_count<- c(data2_agesums)
#       age_df<- data.frame(age_count)
#       
#       age_df %>%
#         rownames_to_column("groups") %>%
#         ggplot(aes(x = groups, y = age_count)) + 
#         geom_bar(stat = "identity", fill='skyblue1')+
#         ggtitle(sub("_", " ", title))+ 
#         theme(plot.title=element_text(hjust=0.5)) + 
#         theme(axis.text.x = element_text(angle = 90, vjust = 0.5))  +
#         theme_minimal() + 
#         scale_x_discrete(limits = c("age_under_5","age_5_to_10","age_11_to_20", 
#                                     "age_21_to_30","age_31_to_50",  "age_51_to_70", "age_over_71"),
#                          labels = c("Age Under 5", "Age 5 to 10", "Age 11 to 20", "Age 21 to 30", "Age 31 to 50", 
#                                     "Age 51 to 70", "Age Over 71")) +
#         labs(y=str_to_title(yaxislabel), x = str_to_title(xaxislabel), fill = legendlabel)
#       
#       
#     }else{
#       
#       data = data %>% dplyr::rename(var1 = varNames[[1]])
#       xaxislabel = paste(varNames[1])
#       legendlabel = paste(varNames[2])
#       data  %>% ggplot(aes(x = reorder(var1, var1, function(x)-length(x)))) + 
#         theme_minimal() +
#         geom_bar(fill="skyblue1") + 
#         labs(y=str_to_title(yaxislabel), x = str_to_title(xaxislabel)) + 
#         ggtitle(sub("_", " ", title))
#     }
#     
#     
#   } else {
#     # This line fixes it
#     data = data %>% dplyr::rename(var1 = varNames[[1]], var2 = varNames[[2]])
#     xaxislabel = paste(varNames[1])
#     legendlabel = paste(varNames[2])
#     data %>% ggplot(aes(x = reorder(var1, var1, function(x)-length(x)), fill = var2)) + 
#       geom_bar() + labs(y= str_to_title(yaxislabel), x = str_to_title(sub("_", " ", xaxislabel)))+labs(fill=legendlabel)+
#       ggtitle(sub("_", " ", title)) + 
#       theme(plot.title=element_text(hjust=0.5)) +
#       theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
#       theme_minimal()
#     
#   }
#   
#   
# }


PreEventPlot <- function(data, varNames, additional = c("horizontal", "text", "proportions", "missing")) {
  # data = preEventData()
  title = paste(varNames[1], "Barplot") %>% str_to_title()
  yaxislabel = paste("Number of Responses") %>% str_to_title()
  
  if(length(varNames) == 1) {      
    data = data %>% dplyr::rename(var1 = varNames[[1]])
    
    if (is.factor(data$var1)){
      levelsVar1 = levels(data$var1) %>% str_replace_all("_"," ") %>% str_to_title() %>% append("missing")
    }else{
      levelsVar1 = NA
    }
    levelsVar2 = NA
    
    data = data %>% mutate(var1 = str_replace_all(var1, "_", " ") %>% str_to_title())
    if("missing" %in% additional){
      data = data %>% filter(!is.na(var1))
    }else{
      data = data %>% mutate(var1 = ifelse(is.na(var1), "missing",var1))
      
    }
    xaxislabel = paste(varNames[1])
    data = data %>% group_by(var1) %>% dplyr::count()
    g = data 
    g = helperAdditional1(g, additional = additional)
    

    
    g = g  + 
      xlab(str_to_title(gsub("_", " ", xaxislabel))) + ylab(str_to_title(gsub("_", " ", yaxislabel))) +
      ggtitle(gsub("_", " ", title)) +
      theme(plot.title=element_text(hjust=0.5)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
      theme_minimal()
    
  }else{
    data = data %>% dplyr::rename(var1 = varNames[[1]], var2 = varNames[[2]])
    
    if (is.factor(data$var1)){
      levelsVar1 = levels(data$var1) %>% str_replace_all("_"," ") %>% str_to_title() %>% append("missing")
    }else{
      levelsVar1 = NA
    }

    if (is.factor(data$var2)){
      levelsVar2 = levels(data$var2) %>% str_replace_all("_"," ") %>% str_to_title() 
    }else{
      levelsVar2 = NA
    }
    
    data = data %>% mutate(var1 = str_replace_all(var1, "_", " ") %>% str_to_title()) %>% mutate(var2 = str_replace_all(var2, "_", " ") %>% str_to_title())
    if("missing" %in% additional){
      data = data %>% filter(!is.na(var1), !is.na(var2))
    }else{
      data = data %>% mutate(var1 = ifelse(is.na(var1), "missing",var1), ifelse(is.na(var2), "missing",var2))
    }
    xaxislabel = paste(varNames[1])
    legendlabel = paste(varNames[2])
    data = data %>% group_by(var1,var2) %>% dplyr::count()
    data = data %>% group_by(var1) %>% group_modify(~ .x %>% mutate(n2 = sum(n)) )
    
    if (!is.na(levelsVar2)){
      data = data %>% mutate(var2 = factor(var2, levelsVar2))
    }
    
    g = data
    
    g = helperAdditional(g, additional = additional)
    
    g = g  + 
      xlab(str_to_title(gsub("_", " ", xaxislabel))) + ylab(str_to_title(gsub("_", " ", yaxislabel))) +
      labs(fill = gsub("_", " ",legendlabel) %>% str_to_title())+
      ggtitle(gsub("_", " ", title)) +
      theme(plot.title=element_text(hjust=0.5)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
      theme_minimal()
  }
  if("horizontal" %in% additional){
    g = g + coord_flip()
  }
  if (!is.na(levelsVar1)){
    g = g + scale_x_discrete(labels = levelsVar1)
  }
  
  g
  
}


helperAdditional = function(g, additional){
  if("proportions" %in% additional){
    g = g %>% ggplot(aes(x =  reorder(var1, n2), y = n/n2, fill = var2 ))  + geom_bar(position = "fill", stat = "identity") 
    if("numeric_text" %in% additional){
      g = g + geom_text(aes(label= round(n/n2,2) ), position = position_stack(vjust = 0.5), size = 3)
    }
  }else{
    g = g %>% ggplot(aes(x =  reorder(var1, n2), y = n, fill = var2 ))  + geom_bar(stat = "identity") 
    if("numeric_text" %in% additional){
      g = g + geom_text(aes(label=n), position = position_stack(vjust = 0.5), size = 3) 
    }
  }
  
  return(g)
}

helperAdditional1 = function(g, additional){
  if("proportions" %in% additional){
    g = g %>% ggplot(aes(x =  reorder(var1, n), y = n/n ))  + 
      geom_bar(stat = "identity", fill='skyblue1', position = "fill") 
    
    if("numeric_text" %in% additional){
      g = g+ geom_text(aes(label= round(n/n) ), position = position_stack(vjust = 0.5), size = 3)
    }
    
  }else{
    g = g %>% ggplot(aes(x =  reorder(var1, n), y = n))  + 
      geom_bar(stat = "identity", fill='skyblue1') 
    
    if("numeric_text" %in% additional){
      g = g + geom_text(aes(label=n), position = position_stack(vjust = 0.5), size = 3) 
    }
  }
  
  return(g)
}


# g = PreEventPlot(as.data.frame(data),varNames = c("age_group", "pronoun"), additional = c("horizontal", "missing"))







# Map 
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(rgdal)
require(plyr)


map_one_variable <- function(df, var1){
  postcodes_loc <- read_csv("australian_postcodes.csv")
  df$postcode = as.character(df$postcode)
  df$`...1` = seq(nrow(df)) 
  df_joined <- left_join(df, postcodes_loc, by = c("postcode" = "postcode"))
  df_joined <- df_joined %>% dplyr::distinct(...1, .keep_all = TRUE)
  
  df_joined[[var1]] <- df_joined[[var1]] %>% replace_na('Missing')
  
  df_clean <- df_joined
  #df_clean$age_group <- factor(df_clean$age_group, levels = c("age_under_5", "age_11_to_20", "age_21_to_30",
  #   "age_31_to_50", "age_51_to_70",  "age_over_71"))
  # String cleaning for output
  df_clean <- df_clean %>% 
    mutate(age_group = str_replace_all(age_group, "age_", "")) %>%
    mutate(age_group = str_replace_all(age_group, "_", " ")) %>%
    mutate(age_group = str_to_sentence(age_group))
  
  clean_var <- var1 %>% 
    str_replace_all("_", " ") %>%
    str_to_title()
  
  
  df_clean$age_group <- factor(df_clean$age_group, levels = c("Under 5", "11 to 20", "21 to 30",
                                                              "31 to 50", "51 to 70",  "Over 71", "Missing"))
  
  df_clean[[var1]] <- factor(df_clean[[var1]])
  df_clean[[var1]] <- forcats::fct_relevel(df_clean[[var1]], "Missing", after = Inf)
  print(df_clean[[var1]])
  
  df_clean.df <- split(df_clean, df_clean[[var1]])
  
  l <- leaflet() %>% addTiles()
  
  names(df_clean.df) %>%
    purrr::walk(function(df) {
      l <<- l %>%
        addMarkers(data=df_clean.df[[df]],
                   lng=~long, lat=~lat,
                   popup = paste("Location: ", df_clean.df[[df]]$locality %>% str_to_title(), 
                                 #"<br>", "Age: ", df_clean.df[[df]]$age, 
                                 sep = ""),
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
    htmlwidgets::onRender(paste("
          function() {
              $('.leaflet-control-layers-list').prepend('<label style=\"text-align:center\">", clean_var,"</label>');
          }
      ", sep = ""))
}



map_0_var <- function(df){
  postcodes_loc <- read_csv("australian_postcodes.csv")
  df$postcode = as.character(df$postcode)
  df$`...1` = seq(nrow(df)) 
  df_joined <- left_join(df, postcodes_loc, by = c("postcode" = "postcode"))
  df_joined <- df_joined %>% dplyr::distinct(...1, .keep_all = TRUE)
  
  m <- leaflet(df_joined) %>%
    addTiles() %>%
    addMarkers(~long, ~lat, popup = paste("Location: ", df_joined$locality %>% str_to_title(),
                                          sep = ""), clusterOptions = markerClusterOptions)
  m
}