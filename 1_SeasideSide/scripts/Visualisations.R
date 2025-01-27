# PreEvent

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
    g = helperAdditionalOneVariable(g, additional = additional)



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

  g = g + 
    ggthemes::scale_fill_tableau(
      palette = "Tableau 10",
      type = "regular",
      direction = 1,
      na.value = "grey50"
    )
  
  
  g

}




PreEventPlotYear <- function(data, varNames, additional = c("horizontal", "text", "proportions", "missing")) {
  # data = preEventData()
  title = paste(varNames[1], "Barplot") %>% str_to_title()
  yaxislabel = paste("Number of Responses") %>% str_to_title()
 
    data = data %>% dplyr::rename(var1 = varNames[[1]], var2 = varNames[[2]])
    
    
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
    
    g = helperAdditionalYear(g, additional = additional)
    
    g = g  +
      xlab(str_to_title(gsub("_", " ", xaxislabel))) + ylab(str_to_title(gsub("_", " ", yaxislabel))) +
      labs(fill = gsub("_", " ",legendlabel) %>% str_to_title())+
      ggtitle(gsub("_", " ", title)) +
      theme(plot.title=element_text(hjust=0.5)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
      theme_minimal()
  
  if("horizontal" %in% additional){
    g = g + coord_flip()
  }

  
  g = g + 
    ggthemes::scale_fill_tableau(
      palette = "Tableau 10",
      type = "regular",
      direction = 1,
      na.value = "grey50"
    )
  
  
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


helperAdditionalYear = function(g, additional){
  if("proportions" %in% additional){
    g = g %>% ggplot(aes(x = var1, y = n/n2, fill = var2 ))  + geom_bar(position = "fill", stat = "identity")
    if("numeric_text" %in% additional){
      g = g + geom_text(aes(label= round(n/n2,2) ), position = position_stack(vjust = 0.5), size = 3)
    }
  }else{
    g = g %>% ggplot(aes(x =  var1, y = n, fill = var2 ))  + geom_bar(stat = "identity")
    if("numeric_text" %in% additional){
      g = g + geom_text(aes(label=n), position = position_stack(vjust = 0.5), size = 3)
    }
  }
  
  return(g)
}

helperAdditionalOneVariable = function(g, additional){
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















# Post



map_kpi <- function(df, kpi){
  postcodes_loc <- read_csv("australian_postcodes.csv") %>% group_by(postcode) %>% slice(1)
  df$postcode_event = as.character(df$postcode_event)
  df$`...1` = seq(nrow(df))
  df_joined <- left_join(df, postcodes_loc, by = c("postcode_event" = "postcode"))
  df_joined <- df_joined %>% dplyr::distinct(...1, .keep_all = TRUE)


  if (kpi == "action"){
    kpi_col = "action_kpi"
  }
  if (kpi == "community"){
    kpi_col = "community_kpi"
  }
  if (kpi == "learning"){
    kpi_col = "learning_kpi"
  }
  if (kpi == "average KPI"){
    kpi_col = "Average KPI"
  }

  # df_joined <- df_joined %>% dplyr::group_by(postcode_event, lat, long, locality) %>%
  #   dplyr::summarise(
  #     vis_kpi = mean(get(kpi_col)),
  #     count = n()
  #   )
  
  df_joined <- df_joined %>% dplyr::group_by(postcode_event, lat, long, locality) %>%
    dplyr::summarise(
      vis_kpi = mean(get(kpi_col)),
      count = n()
    )

  # Colour Palette
  pal <- colorNumeric("viridis", df_joined$vis_kpi, n = 5)

  # Rescaling count column to 0-50
  df_joined$count_scaled <- df_joined$count %>% scale(center = FALSE, scale = max(df_joined$count, na.rm = TRUE)/50)

  # Map
  m <- leaflet(df_joined) %>%
    addTiles() %>%
    addCircleMarkers(~long, ~lat,
                     popup = paste("Location: ", df_joined$locality %>% str_to_title(),
                                   "<br>", "Average KPI: ", round(df_joined$vis_kpi, 2),
                                   "<br>", "Number of Participants: ", df_joined$count, 
                                   "<br>", "Postcode: ", df_joined$postcode_event, 
                                   sep = ""),
                     color = ~pal(vis_kpi), fillOpacity = 0.5
                     # ,radius = ~count_scaled
    )
  
  if (nrow(df_joined) == 1){
    m = m
  }else{
    m = m %>% addLegend(pal = pal, values = ~vis_kpi, opacity = 1.0,
              title = paste((kpi_col %>% str_split("_"))[[1]][1] %>% str_to_title(), 'KPI'),
              labFormat = labelFormat(transform = function(x) round(x, 2)))
  }
  m
}













# Yearly KPI

postEventYearPlot <- function(data){
  data = data %>% group_by(year) %>% mutate_all(mean, na.rm = TRUE) %>% dplyr::slice(1) %>% gather(-year, key = "KPI", value = "score")
  df = data %>% gather(-year, key = "KPI", value = "score")
  # Added 
  data$KPI = stri_trans_totitle(data$KPI)
  if (length(levels(factor(data$year))) == 1){
  
    plot <- ggplot(data, aes(x=KPI, y = score)) + 
      geom_bar(stat='identity',fill='skyblue') +
      ggtitle(paste('KPI Scores for',levels(factor(data$year)))) +
      theme_minimal() +
      xlab("KPI") +
      ylab("Score")

  } else{
    plot <- data %>% ggplot(aes(x = year, y = score, colour = KPI )) +
      # geom_point(color='black') +
      geom_line() +
      xlab('Year') +
      ylab('KPI Score') +
      scale_x_continuous(breaks = seq(min(data$year), max(data$year)) ) +
      # scale_y_continuous(limits=c(0,1),breaks = seq(0, 1, by = 0.1)) +
      ggtitle("Yearly KPI Scores") +
      guides(color = guide_legend(title = "KPI")) +
      theme_classic()
    }

  
  plot = plot + 
    ggthemes::scale_color_tableau(
      palette = "Tableau 10",
      type = "regular",
      direction = 1,
      na.value = "grey50"
    )
  
  return(plot)
}


# Postevent barplot

# c("horizontal", "numeric_text")
PostEventPlot <- function(df ,additional = c("horizontal", "numeric_text")) {

  g <-ggplot(data = df, aes(x=KPI, y = score))  +
    geom_bar(stat='identity', fill="skyblue") +
    ggtitle("KPI Scores") +
    theme_minimal() +
    xlab("KPI") +
    ylab("Score")

  if("horizontal" %in% additional){
    g = g + coord_flip()

  }

  if("numeric_text" %in% additional){
    g = g + geom_text(aes(label= round(score,2)), position = position_stack(vjust = 0.5), size = 3)
  }

  g
}

# PostEventPlot(df, additional = c("horizontal", "text"))



# Stratified



yearlyStratifiedVizPlot <- function(data,kpi_name,demographic_name){
  
  colnames(data) <- tolower(colnames(data))
  varnames_sorted <- sort(colnames(data))
  name_kpi = paste(kpi_name,'KPI')
  demographic_name = stri_trans_totitle(str_replace_all(demographic_name,'_',' ')) # Added
  data$demographic <- gsub("_"," ",as.character(data$demographic)) # Added 
  data$demographic = stri_trans_totitle(data$demographic) # Added 
  data$kpi = stri_trans_totitle(data$kpi) # Added 
  # Added 
  if( length(levels(factor(data$year))) == 1){
    
     if (grepl("Age", data$demographic[1], fixed = TRUE) == TRUE){
      age_string <- levels(factor(data$demographic))
      age_num <- parse_number(age_string)
      sorted_age_string <- age_string[order(age_num)]
      
      plot <- ggplot(data, aes(x = demographic, y = value)) +
        geom_bar(stat='identity',fill='skyblue') +
        scale_x_discrete(limits = sorted_age_string ) +
        ggtitle(paste(levels(factor(data$kpi)),'KPI Score for',levels(factor(data$year)))) +
        theme_minimal() +
        xlab("Demographic") +
        ylab("Score")
      
    } else{
    plot <- ggplot(data, aes(x = demographic, y = value)) +
      geom_bar(stat='identity',fill='skyblue') +
      ggtitle(paste(levels(factor(data$kpi)),'KPI Score for',levels(factor(data$year)))) +
      theme_minimal() +
      xlab("Demographic") +
      ylab("Score")
    }
     } else{
    plot <- ggplot(data, aes(x = year, y = value, colour = demographic)) + 
      geom_line() + 
      scale_x_continuous(breaks = seq(min(data$year), max(data$year)) ) +
      xlab('Year') + 
      ylab(name_kpi) + 
      guides(color = guide_legend(title = demographic_name)) + 
      theme_classic() + 
      ggtitle("Yearly KPI Scores") 
    }
  # ------
  plot = plot + 
    ggthemes::scale_color_tableau(
      palette = "Tableau 10",
      type = "regular",
      direction = 1,
      na.value = "grey50"
    )
  
  return(plot)
  
}








stratifiedEventPlot <- function(data,
                                varNames, 
                                demographicName = "Demographic",
                                kpi = "KPI",
                                additional = c("horizontal", "text", "proportions", "missing")) {
  
  title = paste(kpi,"-", demographicName %>%  str_replace_all("_", " ") %>%  str_to_title() , "KPI's") 
  yaxislabel = paste(kpi ,"Scores") %>% str_to_title()
  

  data = data %>% dplyr::rename(var1 = varNames[[1]])
  
  if (is.factor(data$var1)){
    levelsVar1 = levels(data$var1) %>% str_replace_all("_"," ") %>% str_to_title() %>% append("missing")
  }else{
    levelsVar1 = NA
  }
  
  data = data %>% mutate(var1 = str_replace_all(var1, "_", " ") %>% str_to_title())
  if("missing" %in% additional){
    data = data %>% filter(!is.na(var1))
  }else{
    data = data %>% mutate(var1 = ifelse(is.na(var1), "missing",var1))
    
  }
  xaxislabel = paste(varNames[1])
  data = data %>% group_by(var1) %>% 
    dplyr::summarise(n = mean(KPI))
  
  data = data %>% mutate(n = round(n,2))
  g = helperAdditionalOneVariableStratified(g = data, additional = additional)
  
  
  
  g = g  +
    xlab(str_to_title(gsub("_", " ", xaxislabel))) + ylab(str_to_title(gsub("_", " ", yaxislabel))) +
    ggtitle(gsub("_", " ", title)) +
    theme(plot.title=element_text(hjust=0.5)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    theme_minimal()
  
  
  
  if("horizontal" %in% additional){
    g = g + coord_flip()
  }
  if (!is.na(levelsVar1)){
    g = g + scale_x_discrete(labels = levelsVar1)
  }
  
  g = g + 
    ggthemes::scale_fill_tableau(
      palette = "Tableau 10",
      type = "regular",
      direction = 1,
      na.value = "grey50"
    )
  
  
  g + 
    xlab(demographicName  %>% str_replace_all("_", " ")%>%  str_to_title()) +
    guides(fill=guide_legend(title=demographicName  %>% str_replace_all("_", " ")%>%  str_to_title())) + 
    theme(legend.position="none")
  
}


helperAdditionalOneVariableStratified = function(g, additional){
  if("proportions" %in% additional){
    g = g %>% ggplot(aes(x =  reorder(var1, n), y = n/n, fill = var1 ))  +
      geom_bar(stat = "identity", position = "fill")
    
    if("numeric_text" %in% additional){
      g = g+ geom_text(aes(label= round(n/n, 2) ), position = position_stack(vjust = 0.5), size = 3)
    }
    
  }else{
    g = g %>% ggplot(aes(x =  reorder(var1, n), y = n, fill = var1))  +
      geom_bar(stat = "identity")
    
    if("numeric_text" %in% additional){
      g = g + geom_text(aes(label=n), position = position_stack(vjust = 0.5), size = 3)
    }
  }
  
  return(g)
}
# 
