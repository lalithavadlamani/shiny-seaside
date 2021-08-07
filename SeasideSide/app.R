library(tidyverse)
library(DT)
library(plotly)


library(shiny)
library(shinydashboard)
library(googlesheets4)
library(googledrive)


driveToken = googledrive::drive_auth(email = c("sourish.iyengar@gmail.com"), path = "https://drive.google.com/drive/folders/1PGMilQ7u0zDQ-KJbplDHxG5I-en5IMds")
sheetsToken = gs4_auth(token = drive_token())



files = drive_ls( "https://drive.google.com/drive/folders/1PGMilQ7u0zDQ-KJbplDHxG5I-en5IMds")



ui <- dashboardPage(
    
    # Colour
    skin = "blue",
    
    # Application title
    dashboardHeader(title = "Seashine"),
    
    # Sidebar Menu
    dashboardSidebar(    
        sidebarMenu(
        menuItem("Pre-event", tabName = "preEvent", icon = icon("")),
        menuItem("Post-event", tabName = "postEvent", icon = icon("")),
        menuItem("Stratified-Analysis", tabName = "stratifiedAnalysis", icon = icon(""))
        )
    ),
    
    # Main Body
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            column(4,
                box(
                    status = "primary", 
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    title = "Controls",
                    width = 12,

                    # Panel 1 sidebar
                    sidebarPanel(
                        width = 12,
                        selectizeInput("sheet", "Choose Survey", choices = files$name)
                    ),
                )
            ),
            
            column(8,
                box(title = "Data", 
                    status = "primary", 
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 12,
                    
                    # Panel 1 Data
                    DT::DTOutput("googleSheetData", height = 250)
                    )
            )
            
        )
    )
    
    
    
)


server <- function(input, output, server) {
    
    
    output$googleSheetData = renderDT({read_sheet(drive_get(input$sheet))})

}

# Run the application 
shinyApp(ui = ui, server = server)
