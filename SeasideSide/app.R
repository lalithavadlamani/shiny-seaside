library(tidyverse)
library(DT)
library(plotly)


library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(googlesheets4)
library(googledrive)
library(readr)

source("filename_cleaning.R")
source("preEventCleaning.R")



# path = "https://drive.google.com/drive/folders/1PGMilQ7u0zDQ-KJbplDHxG5I-en5IMds"
path = "https://drive.google.com/drive/u/0/folders/1Yl_VatRKZD7HeA-CrZHaCtZXXVt2rwY5"
# driveToken = googledrive::drive_auth(email = c("sourish.iyengar@gmail.com"), path = path)
# sheetsToken = gs4_auth(token = drive_token())
# files = drive_ls(path)
email = "sourish.iyengar@gmail.com"


# Prevent Choices (Variables)
preVars = list(Age = "age_group",Gender = "pronoun", 
               "Previous Attendance" = "previous_attendance", 
               "How did you hear about the event?" = "find_out_event",
               "Perceived Environmental Investment"= "invested_environmental_impact") 


ui <- dashboardPage(
    # Colour
    skin = "blue",
    # Include the custom styling
    
    # Application title
    dashboardHeader(title = "Seashine",

        # Google Drive Dropdown Box
        dropdownMenu(
            type = "notifications",
            headerText = strong("QUICK TIPS"),
            icon = icon("question"),
            badgeStatus = NULL,
            notificationItem(
                text = "Some quick tips about the specific page you are on",
                icon = icon("lightbulb")
            )
        ),
        # Quick Tips Dropdown Box
        dropdownMenu(
            type = "notifications",
            headerText = strong("QUICK TIPS"),
            icon = icon("question"),
            badgeStatus = NULL,
            notificationItem(
                text = "Some quick tips about the specific page you are on",
                icon = icon("lightbulb")
            )
        )             
    ),
    
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
        
        tags$head(
            tags$link(
                rel = "stylesheet", 
                type = "text/css", 
                href = "styles.css")
        ),
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
                    status = "success", 
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
                        
                        # Pre 
                        conditionalPanel("input.sidebar == 'preEvent'",
                                         
                            selectizeInput("preVar1", "Demographic", choices = preVars,selected  = "pronoun"),
                            
                            conditionalPanel("input.analysisType == 'Event'",
                                selectizeInput("preVarColour", "Colouring Variable", choices = c("None",preVars))
                                ),
                            
                            prettySwitch("advancedPreOptions", "More Plotting Options?", slim = TRUE),
                            conditionalPanel("input.advancedPreOptions == 1",
                             #selectizeInput
                                checkboxGroupButtons("prePlotOptions",
                                               "Plotting Options (Optional)", 
                                               choices = c("Horizontal", "Proportions", "Numeric Text","Remove Unknowns"), 
                                               selected = NULL),
                                               # ,
                                               # multiple = TRUE),
                                textInput("preTitle", "Choose plot title", value = NULL ,width = NULL),
                                textInput("preXaxis", "Choose x-axis label", value = NULL ,width = NULL),
                                textInput("preYaxis", "Choose y-axis label", value = NULL,width = NULL)
                                ),
                            downloadButton('downloadPreEvent',"Download the data")
                                
                        ),
                        
                        # Post
                        conditionalPanel("input.sidebar == 'postEvent'",
                                         radioGroupButtons(
                                             inputId = "postEventKPI",
                                             label = "KPI:", 
                                             choices = c("Action","Learning", "Community")
                                         ),
                                         downloadButton('downloadPostEvent',"Download the data")
                                         
                        ),
                        
                        # Stratified
                        conditionalPanel("input.sidebar == 'stratifiedAnalysis'",
                                         radioGroupButtons(
                                             inputId = "stratifiedEventKPI",
                                             label = "KPI:", 
                                             choices = c("Action","Learning", "Community")
                                         ),
                                         downloadButton('downloadStratifiedEventKPIEvent',"Download the data")
                                         
                        )
                            
                    ),
                )
            ),
            
            # Panel 1 Age
            column(8,
                   conditionalPanel("input.sidebar == 'preEvent'",
                       box(title = "Demographic Analysis", 
                           status = "warning", 
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
        
                        DT::DTOutput("preCleanedData", height = 300)
                        )
                )
            ),
            

            
            #Panel 2 Data
            column(12,
                   conditionalPanel("input.sidebar == 'postEvent'",
                                    box(title = "Data",
                                        status = "danger",
                                        solidHeader = TRUE,
                                        collapsible = TRUE,
                                        collapsed = TRUE,
                                        width = 12,

                                        DT::DTOutput("postCleanedData", height = 300)
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
                         choices = paste(metaData$location, metaData$date) %>% unique()
                         default = choices[1]
                     }else if (input$analysisType == "Yearly"){
                         choices = metaData$date %>% 
                             lubridate::as_date(format = "%d.%m.%y") %>% 
                             lubridate::year() %>% 
                             unique() %>% 
                             sort() %>% 
                             unique()
                         default = choices
                     }else{
                         choices = metaData$location %>% unique()
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
    
    # Panel 1
    
        ##  Prevent Colouring
    observeEvent(input$preVar1,{
        updateSelectizeInput(session,
                             "preVarColour", 
                             choices = c("None", preVars)[c("None", preVars) != input$preVar1],
                             selected = "None",
                             server = TRUE)
        }
    )
    
         ## Pre-event Advanced Options - Labelling
    observeEvent(input$advancedPreOptions,{
        updateTextInput(session,
                        "preTitle", 
                         value = paste(input$preVar1,"Barplot") %>% str_to_title()
                        )
        
        updateTextInput(session,
                        "preXaxis", 
                        value = input$preVar1 %>% str_to_title()
                        )
        
        updateTextInput(session,
                        "preYaxis", 
                        value = "Count"
                        )
    }
    )
    
         ## PreEvent read data
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

            file_names = filter_data(metaData(), location_filter = location, form_type = "Pre")
            file_names = filter_data(file_names, date_filter = date, form_type = "Pre")$file_name
            
        }else if(input$analysisType == "Yearly"){
            file_names = filter_data(metaData(), year_filter = input$sheet, form_type = "Pre")$file_name            
        }else{
            file_names = filter_data(metaData(), location_filter = input$sheet, form_type = "Pre")$file_name            
        }

        data = file_names %>% lapply(drive_get) %>% lapply(read_sheet)
        processedData = preprocessing_multiple_fn(data, file_names)
        processedData
        })


        ## PreEvent Datatable

    
    output$preCleanedData = renderDT({
            datatable(
                preEventData(),
                options = list(pageLength=10, scrollX='400px')
            )
        })

    
        ## PreEvent Visualisations 
    
    output$preViz = renderPlotly({

        if (input$analysisType %in% c("Location", "Yearly")){
            g = preEventData() %>% 
                select(year,varViz = input$preVar1) %>% 
                ggplot(aes(x = year, fill = varViz)) +
                    geom_bar() + 
                    theme_minimal() 
              
            
        }else{
            if (input$preVarColour == "None"){
                g = PreEventPlot(preEventData(),input$preVar1)
            }else{
                g = PreEventPlot(preEventData(),c(input$preVar1, input$preVarColour)) 
            }
        }
        
        if (input$advancedPreOptions == TRUE){
            if (!is.null(input$preTitle)){
              g = g + ggtitle(input$preTitle)  
            }
            if (!is.null(input$preXaxis)){
              g = g + xlab(input$preXaxis) 
            }
            if (!is.null(input$preYaxis)){
              g = g + ylab(input$preYaxis) 
            }
            
        }
        ggplotly(g)
        
        

    })
    
        ## PreEvent download data
    output$downloadPreEvent <- downloadHandler(
        filename = function(){"preEvent.csv"}, 
        content = function(fname){
            write.csv(preEventData(), fname)
        }
    )    
    
    # Tab 2
    
    
    postEventData = reactive({
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

            file_names = filter_data(metaData(), location_filter = location, form_type = "Post")
            file_names = filter_data(file_names, date_filter = date, form_type = "Post")$file_name

        }else if(input$analysisType == "Yearly"){
            file_names = filter_data(metaData(), year_filter = input$sheet, form_type = "Post")$file_name
        }else{
            file_names = filter_data(metaData(), location_filter = input$sheet, form_type = "Post")$file_name
        }

        data = read_sheet(drive_get(file_names))
        # data = file_names %>% lapply(drive_get) %>% lapply(read_sheet)
        # processedData = preprocessing_multiple_fn(data, file_names)
        # processedData
    })


    output$postCleanedData = renderDT({
        datatable(
            postEventData(),
            options = list(pageLength=10, scrollX='400px')
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
