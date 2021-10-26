require(stringr)
require(tidyverse)

# Example input
#test_input = c("Bondi Junction_2026_03.02.2021_Pre_Participants", "Turramurra_2026_04.01.2020_Pre_Participants",
#               "Camperdown_2026_04.01.2020_Pre_Participants", "Lithgow_2026_04.01.2019_Pre_Participants")

setClass(Class="NameData", slots = c(locations = "character", 
                                     dates = "character", 
                                     form_type = "character", 
                                     event_type = "character"))

# Function: Extracts values from file name
# Input: files$name from google drive
# Output: Class with list of values

name_processing_to_class <- function(files) {
  
  # Initialisng lists
  locations = vector()
  dates = vector()
  form_type = vector()
  event_type = vector()
  
  i <- 1
  for (file_name in test_input){
    values = strsplit(file_name, '_')
    locations[i] = values[[1]][1]
    dates[i] = values[[1]][2]
    form_type[i] = values[[1]][3]
    event_type[i] = values[[1]][4]
    i = i + 1
  }
  return(new("NameData", 
             locations = locations, 
             dates = dates, 
             form_type = form_type, 
             event_type = event_type))
}

#name_data <- name_processing(test_input)

# Function: Extracts values from file name into a DF

name_processing <- function(files) {
  
  
  df = NULL
  for (file_name in files){
    values = strsplit(file_name, '_')
    df = rbind(df, data.frame(values[[1]][1],
                              values[[1]][2],
                              values[[1]][3],
                              values[[1]][4],
                              values[[1]][5],
                              file_name))

  }
  colnames(df) <- c("location", "postcode","date", "form_type", "event_type", "file_name")
  df = df %>% 
    mutate(date = as.Date(date, format = "%d.%m.%Y"))
  df$year <- as.numeric(format(df$date, "%Y"))
  return(df)
}

#df <- name_processing(test_input)

# Function: Takes in filter values and outputs the names of the files
# Use optional arguments

filter_data <- function(df, location_filter = vector(), year_filter = vector(), 
                        form_type_filter = vector(), event_type_filter = vector(),
                        date_filter = vector()){
  
  filter_df <- df %>% filter( (location %in% location_filter | 
                               year %in% year_filter |
                               event_type %in% event_type_filter |
                               date %in% date_filter) & ((form_type %in% form_type_filter))
                             )
  
  return(filter_df)
}


# lubridate::as_date(df$date, format = "%d.%m.%y")

# Examples

#filter_df <- filter_data(df, location_filter = c("Lithgow", "Camperdown"))
#filter_df
#
#filter_df <- filter_data(df, location_filter = c("Lithgow", "Camperdown"),
#                         year_filter = c("2021", "2020"))
#filter_df
#
#filter_df <- filter_data(df, year_filter = c("2020"))
#filter_df$file_name
  


