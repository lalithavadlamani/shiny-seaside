require(googledrive)
require(stringr)


path = "https://drive.google.com/drive/folders/1PGMilQ7u0zDQ-KJbplDHxG5I-en5IMds"
email = "sourish.iyengar@gmail.com"
drive_token = googledrive::drive_auth(email = c(email), path = path)

sheetsToken = gs4_auth(token = drive_token())

files = drive_ls(path)

x <- files$name



# Function: Extracts values from file name

# Example input
test_input = c("Bondi Junction_03.02.2021_Pre_Participants", "Turramurra_04.01.2020_Pre_Participants",
      "Camperdown_04.01.2020_Pre_Participants", "Lithgow_04.01.2020_Pre_Participants")

setClass(Class="Name Data")

name_processing <- function(files) {
  
  # Initialisng lists
  locations = list()
  dates = list()
  form_type = list()
  event_type = list()
  
  for (file_name in test_input){
    values = strsplit(file_name, '_')
    append(locations, values[[1]][1])
  }
  return(new("Name Data", 
             locations = locations, 
             dates = dates, 
             form_type = form_type, 
             event_type = event_type))
}

name_processing(test_input)

