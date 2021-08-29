# Example input
test_input = c("Bondi Junction_03.02.2021_Pre_Participants", "Turramurra_04.01.2020_Pre_Participants",
               "Camperdown_04.01.2020_Pre_Participants", "Lithgow_04.01.2019_Pre_Participants")

setClass(Class="NameData", slots = c(locations = "character", 
                                     dates = "character", 
                                     form_type = "character", 
                                     event_type = "character"))

# Function: Extracts values from file name
# Input: files$name from google drive
# Output: Class with list of values

name_processing <- function(files) {
  
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

name_data <- name_processing(test_input)

# Function: Extracts values from file name into a DF

name_processing <- function(files) {
  
  
  df = NULL
  for (file_name in files){
    values = strsplit(file_name, '_')
    df = rbind(df, data.frame(values[[1]][1],
                              values[[1]][2],
                              values[[1]][3],
                              values[[1]][4]))

  }
  colnames(df) <- c("location", "date", "form_type", "event_type")
  df$date
  return(df)
}

df <- name_processing(test_input)

# Function: Takes in filter values and outputs the names of the files
# Use optional arguments

filter_data <- function(df, location, date, form_type, event_type){
  
}


# lubridate::as_date(df$date, format = "%d.%m.%y")
