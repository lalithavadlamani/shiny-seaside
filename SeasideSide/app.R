library(tidyverse)
library(DT)
library(plotly)


library(shiny)
library(shinydashboard)
library(shinyWidgets)

library(googlesheets4)
library(googledrive)


source("filename_cleaning.R")
source("preEventCleaning.R")


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
                        # selectizeInput("analysisType", "Analysis:", choices = c("Event","Yearly", "Location")),
                        radioGroupButtons(
                            inputId = "analysisType",
                            label = "Analysis:", 
                            choices = c("Event","Yearly", "Location"),
                            status = "danger"
                        ),
                        # Survey choice - Choices populated in server
                        selectizeInput("sheet", "Choose Survey", choices = NULL, multiple = TRUE),
                        
                        # Prevent visualisation
                        conditionalPanel("input.sidebar == 'preEvent'",
                                         
                            selectizeInput("preVar1", "Demographic", choices = preVars,selected  = "pronoun"),
                            
                            conditionalPanel("input.analysisType == 'Event'",
                                selectizeInput("preVarColour", "Colouring Variable", choices = c("None",preVars))
                                ),
                            
                            prettySwitch("advancedPreOptions", "More Plotting Options?", slim = TRUE),
                            conditionalPanel("input.advancedPreOptions == 1",
                                selectizeInput("prePlotOptions (Optional)",
                                               "Plotting Options", 
                                               choices = c("Horizontal", "Proportions", "Numeric Text"), 
                                               selected = "Horizontal",
                                               multiple = TRUE)
                                )
                        )
                            
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
                           
                           plotly::plotlyOutput("preViz", height = 400)
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
        
                        DT::DTOutput("googleSheetData", height = 300)
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
    
    # Updating inputs with choices corresponding to analysis type
    observeEvent(input$analysisType,{
        
                 if (!(is.null(input$emailID) & is.null(input$driveID))){
                     metaData = metaData()
                     if(input$analysisType == "Event"){
                         choices = paste(metaData$location, metaData$date)
                         default = choices[1]
                     }else if (input$analysisType == "Yearly"){
                         choices = metaData$date %>% 
                             lubridate::as_date(format = "%d.%m.%y") %>% 
                             lubridate::year() %>% 
                             unique() %>% 
                             sort()
                         default = choices
                     }else{
                         choices = metaData$location
                         default = choices[1]
                     }
                     updateSelectizeInput(session,
                                          "sheet", 
                                          choices = choices, 
                                          selected = default,
                                          options = list(maxItems = Inf),
                                          server = TRUE)
                 }
    })
    
    #
    observeEvent(input$preVar1,{
        updateSelectizeInput(session,
                             "preVarColour", 
                             choices = c("None", preVars)[c("None", preVars) != input$preVar1],
                             selected = "None",
                             server = TRUE)
        }
    )
    
    
    preEventData = reactive({
        validate(
            need(input$sheet, "Select file please/File is being loaded from the drive.")
        )
        
        if (input$analysisType == "Event"){
            validate(
                need(length(input$sheet) == 1, "Please select only 1 file.")
            )
        }
        
        if (input$analysisType == "Event"){
            splitString = str_split(input$sheet," ")[[1]]
            date = splitString[length(splitString)] 
            location = splitString[splitString != date] %>% paste(collapse = " ")
            date = date %>% lubridate::as_date()

            file_names = filter_data(metaData(), location_filter = location, event_type_filter = "Pre")
            file_names = filter_data(file_names, date_filter = date, event_type_filter = "Pre")$file_name
            
        }else if(input$analysisType == "Yearly"){
            file_names = filter_data(metaData(), year_filter = input$sheet, event_type_filter = "Pre")$file_name            
        }else{
            file_names = filter_data(metaData(), location_filter = input$sheet, event_type_filter = "Pre")$file_name            
        }

        data = file_names %>% lapply(drive_get) %>% lapply(read_sheet)
        processedData = preprocessing_multiple_fn(data, file_names)
        processedData
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
        
        if (input$analysisType %in% c("Location", "Yearly")){
            g = preEventData() %>% 
                select(year,varViz = input$preVar1[[1]]) %>% 
                ggplot(aes(x = year, fill = varViz)) +
                    geom_bar() + 
                    theme_minimal() 
            ggplotly(g)  
            
        }else{
            if (input$preVarColour == "None"){
                PreEventPlot(preEventData(),input$preVar1) %>% ggplotly()
            }else{
                PreEventPlot(preEventData(),c(input$preVar1, input$preVarColour)) %>% ggplotly()
            }
        }
        
        

    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
