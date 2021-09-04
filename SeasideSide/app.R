library(tidyverse)
library(DT)
library(plotly)


library(shiny)
library(shinydashboard)
library(googlesheets4)
library(googledrive)


source("filename_cleaning.R")



# path = "https://drive.google.com/drive/folders/1PGMilQ7u0zDQ-KJbplDHxG5I-en5IMds"
path = "https://drive.google.com/drive/u/0/folders/1Yl_VatRKZD7HeA-CrZHaCtZXXVt2rwY5"
# driveToken = googledrive::drive_auth(email = c("sourish.iyengar@gmail.com"), path = path)
# sheetsToken = gs4_auth(token = drive_token())
# files = drive_ls(path)
email = "sourish.iyengar@gmail.com"


# Prevent Choices (Variables)
preVars = list(Age = "age_group",Gender = "pronoun", "Previous Attendance" = "previous_attendance", "How did you hear about the event?" = "find_out_event") 



ui <- dashboardPage(
    
    # Colour
    skin = "blue",
    
    # Application title
    dashboardHeader(title = "Seashine"),
    
    # Sidebar Menu
    dashboardSidebar(    
        sidebarMenu(id = "sidebar",
        menuItem("Pre-event", tabName = "preEvent", icon = icon("")),
        menuItem("Post-event", tabName = "postEvent", icon = icon("")),
        menuItem("Stratified-Analysis", tabName = "stratifiedAnalysis", icon = icon(""))
        )
    ),
    
    # Main Body
    dashboardBody(
        
        
        # User inputted email and drive link
        fluidRow(
            column(12,
                box("File Information",
                    textInput("emailID", "Paste drive email address:", value = email ,width = NULL, placeholder = email),
                    textInput("driveID", "Paste folder drive link:", value = path,width = NULL, placeholder = path),
                    width = 12
                )
            )
        ),
        

        # Pre Event Panel
        ## Row 1
        fluidRow(
            
            # Panel 1 sidebar
            column(4,
                box(
                    status = "primary", 
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    title = "Controls",
                    width = 12,

                    # Inputs
                    sidebarPanel(
                        width = 12,
                        # Year by Year Analysis 
                        selectizeInput("analysisType", "Analysis:", choices = c("Event","Yearly", "Location")),
                        # Survey choice - Choices populated in server
                        selectizeInput("sheet", "Choose Survey", choices = NULL, multiple = FALSE),
                        
                        # Prevent visualisation
                        conditionalPanel("input.sidebar == 'preEvent'",
                            selectizeInput("preVar1", "Demographic", choices = preVars,selected  = "pronoun", multiple = TRUE,  options = list(maxItems = 2))                        )
                    ),
                )
            ),
            
            # Panel 1 Age
            column(8,
                   conditionalPanel("input.sidebar == 'preEvent'",
                       box(title = "Demographic Analysis", 
                           status = "primary", 
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           
                           plotly::plotlyOutput("preViz", height = 300)
                       )
                   )
            ),
            
            # Panel 1 Data
            column(12,
                conditionalPanel("input.sidebar == 'preEvent'",   
                    box(title = "Data",
                        status = "danger", 
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed = TRUE,
                        width = 12,
        
                        DT::DTOutput("googleSheetData", height = 250)
                        )
                )
            )
            
        )
        
    )
    
    
    
)


server <- function(input, output, session) {
    
    # Pulling Google Sheets
    sheetInitialisation = reactive({
        path = input$driveID
        driveToken = googledrive::drive_auth(email = c(input$emailID), path = input$driveID)
        sheetsToken = gs4_auth(token = drive_token())
        files = drive_ls(path)
    })
    metaData = reactive({name_processing(sheetInitialisation()$name)})
    
    # Updating Survey Choices
    observeEvent(input$driveID,{
                 if (!(is.null(input$emailID) & is.null(input$driveID))){
                     
                     files = sheetInitialisation()
                     choices = files$name
                     updateSelectizeInput(session,
                                          "sheet", 
                                          choices = choices, 
                                          selected = choices[1],
                                          server = TRUE)
                 }
             }
        )
    observeEvent(input$analysisType,{
                 if (!(is.null(input$emailID) & is.null(input$driveID))){
                     
                     metaData = metaData()
                     if(input$analysisType == "Event"){
                         choices = paste(metaData$location, metaData$date)
                         #choices = files$name
                     }else if (input$analysisType == "Yearly"){
                         choices = metaData$date %>% 
                             lubridate::as_date(format = "%d.%m.%y") %>% 
                             lubridate::year() %>% 
                             unique() %>% 
                             sort()
                         
                     }else{
                         choices = metaData$location
                     }
                     updateSelectizeInput(session,
                                          "sheet", 
                                          choices = choices, 
                                          selected = choices[1],
                                          server = TRUE)
                 }
    })
    
    
    preEventData = reactive({
 
        if (input$analysisType == "Event"){
            location = str_split(input$sheet," ")[[1]][[1]]
            date = str_split(input$sheet," ")[[1]][[2]] %>% lubridate::as_date()
            
            file_names = filter_data(metaData(), location_filter = location, event_type_filter = "Pre")
            file_names = filter_data(file_names, date_filter = date, event_type_filter = "Pre")$file_name
            
        }else if(input$analysisType == "Yearly"){
            file_names = filter_data(metaData(), year_filter = input$sheet, event_type_filter = "Pre")$file_name            
        }else{
            file_names = filter_data(metaData(), location_filter = input$sheet, event_type_filter = "Pre")$file_name            
        }
        # browser()
        # XYZ
        # data = file_names %>% lapply(drive_get) %>% lapply(read_sheet)
        read_sheet(drive_get(file_names))

        })

    # Panel 1
    # Prevent Data Table

    output$googleSheetData = renderDT({
            datatable(
                preEventData(),
                options = list(pageLength=10, scrollX='400px')
            )
        })

    
    # PreEvent Visualisations 
    
    output$preViz = renderPlotly({
        # input$preVar1
        
        if (length(input$preVar1)==1){
            if (!("Age" %in% input$preVar1)){
               g=  preEventData() %>% select(varViz = input$preVar1) %>% 
                   ggplot(aes(x= reorder(varViz, varViz, function(x)-length(x)), fill = varViz)) + 
                   geom_bar() +
                   theme_minimal() +
                   xlab(input$preVar1) + 
                   theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) 
               ggplotly(g)
            }
        }else{
            if (!("Age" %in% input$preVar1)){
                g=  preEventData() %>% select(varViz1 = input$preVar1[[1]], varViz2 = input$preVar1[[2]]) %>% 
                    ggplot(aes(x= reorder(varViz1, varViz1, function(x)-length(x)), fill = varViz2)) + 
                    geom_bar() +
                    theme_minimal() +
                    xlab(input$preVar1[[1]]) +
                    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) 
                ggplotly(g)
            }
        }
        
        # 
        # if (!("Age" %in% input$preVar1)){
        #     preEventData() %>% select(input$preVar1) %>% ggplot(aes(x=))
        # }
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
