library(googledrive)
library(googlesheets4)

path = "https://drive.google.com/drive/folders/18bzvs5LO1C2x4Elx6z3BAjKXmAyRD68B"

email = "allanwu2001@gmail.com"

#driveToken = googledrive::drive_auth(email = c(email), path = path)
#sheetsToken = gs4_auth(token = drive_token())

# Only need to modify this line
files = drive_ls(path, recursive = T, type = "spreadsheet")


