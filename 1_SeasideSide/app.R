require(plyr)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(janitor)
library(stringr)
library(stringi)
library(readxl)
library(webshot)
library(mapview)


library(tidyverse)
library(DT)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(googlesheets4)
library(googleAuthR)
library(googledrive)
library(readr)
library(sortable)
library(lubridate)


source("scripts/filename_cleaning.R")
source("scripts/preEventCleaning.R")
source("scripts/Visualisations.R")
source("scripts/postEventCleaning.R")


# email = "sourish.iyengar@gmail.com"
# path = "https://drive.google.com/drive/u/0/folders/1Yl_VatRKZD7HeA-CrZHaCtZXXVt2rwY5"

email = "seashine.data@gmail.com"
path = "https://drive.google.com/drive/u/0/folders/1Z7PwwfwXejk3hDq7elW9OGb3ivnhpYW2"

options(
    gargle_oauth_email = TRUE,
    gargle_oauth_cache = ".secrets"
)


driveToken = drive_auth(cache = ".secrets", email = TRUE)
gs4_auth(token = drive_token())

# Prevent Choices (Variables)
preVars = list(Age = "age_group",Gender = "pronoun", 
               "Previous Attendance" = "previous_attendance", 
               "How did you hear about the event?" = "find_out_event",
               "Perceived Environmental Investment"= "pre_environmental_impact",
               "Aboriginal or Torres Strait Islander Origin?" = "origin"
)


phantomjs_path <- webshot:::find_phantom()
if (is.null(phantomjs_path)){
    webshot::install_phantomjs()
    FlgJS <- F
} else{
    FlgJS <- T
}
phantomjs_path2 <- webshot:::find_phantom()
(FlgJS2 <- !(is.null(phantomjs_path2)))

EtatInstallationJS <- ifelse(isTRUE(FlgJS),
                             "1-PhJS déja installé",
                             ifelse(isTRUE(FlgJS2),
                                    "2-PhJS vient d'être installé",
                                    "3-PhJS n'a pas été installé"))

ui <- dashboardPage(
    # Colour
    skin = "blue",
    # Include the custom styling
    
    # Application title
    dashboardHeader(title = "Seashine",
                    tags$li(class = "dropdown",
                            tags$a(href="https://www.seasidescavenge.org/", target="_blank", 
                                   tags$img(height = "20px", alt="Seaside Logo", src="logo_white.png")
                            )
                    ),
        # Quick Tips Dropdown Box
        dropdownMenu(
            type = "notifications",
            headerText = strong("QUICK TIPS"),
            icon = icon("question"),
            badgeStatus = NULL,
            messageItem(
                from = "HOME",
                message = tags$div(
                    "Welcome to the home page! Start",
                    tags$br(),
                    "by clicking one of the tabs in the",
                    tags$br(),
                    "side menu, or watch the demo ",
                    tags$br(),
                    "videos below for more information."),
                icon = icon("home")
            ),
            messageItem(
                from = "PRE-EVENT",
                message = tags$div(
                    "Use the 'controls' box to choose ",
                    tags$br(),
                    "between event, yearly, and location",
                    tags$br(),
                    " analysis of demographic data for ",
                    tags$br(),
                    "the pre-event survey. You can also ",
                    tags$br(),
                    "view and download the data used",
                    tags$br(),
                    "in the 'data' section."),
                
                icon = icon("chart-bar")
            ),
            messageItem(
                from = "POST-EVENT",
                message = tags$div(
                    "Use the 'controls' box to choose ",
                    tags$br(),
                    "between event, yearly, and location",
                    tags$br(),
                    " analysis of KPI data for ",
                    tags$br(),
                    "the post-event survey. You can also ",
                    tags$br(),
                    "view and download the data used",
                    tags$br(),
                    "in the 'data' section."),
                icon = icon("seedling")
            ),
            messageItem(
                from = "Combined Analysis",
                message = tags$div(
                    "Use the 'controls' box to choose ",
                    tags$br(),
                    "between event, yearly, and location,",
                    tags$br(),
                    "as well as a KPI category to analyse. ",
                    tags$br(),
                    "participant data. You can also ",
                    tags$br(),
                    "view and download the combined data",
                    tags$br(),
                    "used in the 'data' section."),
                icon = icon("project-diagram")
            ),
            messageItem(
                from = "Export Report",
                message = tags$div(
                    "Download a report with pre-made",
                    tags$br(),
                    "visualisastions of the surveys ",
                    tags$br(),
                    "you are looking for. "),
                icon = icon("file-invoice")
            )

            
            
            
        )   
        
        
    ),
    
    # Sidebar Menu
    dashboardSidebar( 
        sidebarMenu(id = "sidebar",
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Pre-event", tabName = "preEvent", icon = icon("chart-bar")),
        menuItem("Post-event", tabName = "postEvent", icon = icon("seedling")),
        menuItem("Combined Analysis", tabName = "stratifiedAnalysis", icon = icon("project-diagram")),
        menuItem("Report", tabName = "report", icon = icon("file-invoice"))
        
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
        fluidRow(
            column(12,
                   box(
                       status = "danger", 
                       solidHeader = TRUE,
                       collapsible = TRUE,
                       # collapsed = TRUE,
                       # width = 12,
                       
                       title = "File Information",
                       # textInput("emailID", "Paste drive email address:", value = email ,width = NULL, placeholder = email),
                       searchInput("driveID", "Paste folder drive link:", value = path,width = NULL, placeholder = path,
                                   btnSearch = icon("check"), 
                                   btnReset = icon("remove")),
                       width = 12
                   )
            )
        ),

        

        # Pre Event Panel
        ## Row 1
        fluidRow(
            
            # Panel 1 sidebar
            column(4,
                                    
               conditionalPanel("input.sidebar != 'home'",
                                    
                box(
                    status = "warning", 
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    title = "Controls",
                    width = 12,

                    # Inputs
                    sidebarPanel(
                        width = 12,
                        # Year by Year Analysis     
                        radioGroupButtons(
                            inputId = "analysisType",
                            label = "Analysis:", 
                            choices = c("Event","Yearly", "Location"),
                            status = "warning"
                        ),  
                        tags$script("$(\"input:radio[name='analysisType'][value='Event']\").parent().css('background-color', '#F27B34');"),
                        tags$script("$(\"input:radio[name='analysisType'][value='Yearly']\").parent().css('background-color', '#F27B34');"),
                        tags$script("$(\"input:radio[name='analysisType'][value='Location']\").parent().css('background-color', '#F27B34');"),
                        
                        # Survey choice - Choices populated in server
                        selectizeInput("sheet", "Choose Survey", choices = NULL, multiple = TRUE),
                        
                        # Pre 
                        conditionalPanel("input.sidebar == 'preEvent'",
                                         
                            selectizeInput("preVar1", "Demographic", choices = preVars,selected  = "pronoun"),
                            
                            # Only show for barplot
                            conditionalPanel("input.analysisType == 'Event' & input.preTabViz == 1",
                                selectizeInput("preVarColour", "Colouring Variable", choices = c("None",preVars))
                                ),
                            
                            # Only show for barplot
                            conditionalPanel("input.preTabViz == 1",
                                prettySwitch("advancedPreOptions", "More Plotting Options?", slim = TRUE)
                            ),
                            
                            conditionalPanel("input.advancedPreOptions == 1 & input.preTabViz == 1",
                             #selectizeInput
                                checkboxGroupButtons("prePlotOptions",
                                               "Plotting Options (Optional)", 
                                               choices = list("Horizontal" = "horizontal", "Proportions" = "proportions", "Numeric Text" = "numeric_text",
                                                              "Remove Unknowns" = "missing"), 
                                               selected = "missing"),

                                textInput("preTitle", "Choose plot title", value = NULL ,width = NULL),
                                textInput("preXaxis", "Choose x-axis label", value = NULL ,width = NULL),
                                textInput("preYaxis", "Choose y-axis label", value = NULL,width = NULL)
                                ),
                            
                            
                            # For map
                            conditionalPanel("input.preTabViz == 2",
                                             radioGroupButtons(
                                                 inputId = "preMapAnalysisType",
                                                 label = "Spatial Analysis:", 
                                                 choices = c("Participants","Event")
                                             )
                            ),
                            
                            downloadButton('downloadPreEvent',"Download the data"),
                            
                            

                                
                        ),
                    
                        
                        # Post
                        conditionalPanel("input.sidebar == 'postEvent'",
                                         
                             conditionalPanel("input.postTabViz == 2",
                                 radioGroupButtons(
                                     inputId = "postEventKPI",
                                     label = "KPI:", 
                                     choices = c("Action","Learning", "Community")
                                 )
                             ),
                             conditionalPanel("input.postTabViz == 1",
                                              prettySwitch("advancedPostOptions", "More Plotting Options?", slim = TRUE)
                             ),
                             conditionalPanel("input.advancedPostOptions == 1",
                                              conditionalPanel("input.analysisType == 'Event'",
                                                  checkboxGroupButtons("postPlotOptions",
                                                                       "Plotting Options (Optional)", 
                                                                       choices = list("Horizontal" = "horizontal", "Numeric Text" = "numeric_text"), 
                                                                       selected = NULL)
                                              ),
                                              
                                              
                                              conditionalPanel("input.postTabViz == 1",
                                                  textInput("postTitle", "Choose plot title", value = NULL ,width = NULL),
                                                  textInput("postXaxis", "Choose x-axis label", value = NULL ,width = NULL),
                                                  textInput("postYaxis", "Choose y-axis label", value = NULL,width = NULL)
                                              )
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
                                         conditionalPanel("input.stratifiedAnalysisTabViz == 1",
                                             selectizeInput("stratifiedVarColour", "Colouring Variable", choices = c("None",preVars))
                                         ),
                                         
                                         # For map
                                         conditionalPanel("input.stratifiedAnalysisTabViz == 2",
                                                          radioGroupButtons(
                                                              inputId = "stratifiedMapAnalysisType",
                                                              label = "Spatial Analysis:", 
                                                              choices = c("Participants","Event")
                                                          )
                                         ),
                                         
                                         selectizeInput(
                                             "filteringVariable", "Variable to filter by:", choices = c("None",preVars)
                                         ),
                                         tabItem(tabName = "drag",
                                                 uiOutput("bucket")
                                         ),
                                         conditionalPanel("input.stratifiedAnalysisTabViz == 1",
                                                          prettySwitch("advancedStratifiedOptions", "More Plotting Options?", slim = TRUE)
                                         ),
                                         
                                         conditionalPanel("input.advancedStratifiedOptions == 1",
                                                          conditionalPanel("input.analysisType == 'Event' &  input.stratifiedAnalysisTabViz == 1",
                                                                           checkboxGroupButtons("stratifiedPlotOptions",
                                                                                                "Plotting Options (Optional)", 
                                                                                                choices = list("Horizontal" = "horizontal", "Proportions" = "proportions", "Numeric Text" = "numeric_text",
                                                                                                                "Remove Unknowns" = "missing"), 
                                                                                                selected = "missing")
                                                          ),
                                                          
                                                          
                                                          conditionalPanel("input.stratifiedAnalysisTabViz == 1",
                                                                           textInput("stratifiedTitle", "Choose plot title", value = NULL ,width = NULL),
                                                                           textInput("stratifiedXaxis", "Choose x-axis label", value = NULL ,width = NULL),
                                                                           textInput("stratifiedYaxis", "Choose y-axis label", value = NULL,width = NULL)
                                                          )
                                         ),
                                         


                                         downloadButton('downloadStratifiedEventKPIEvent',"Download the data")
                                         
                        )
                            
                    ),
                    ),
                )
            ),
            
            conditionalPanel("input.sidebar == 'home'",
                 column(12,
                        box(
                            status = "warning", 
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            title = "Helpful Demo Videos",
                            width = 12,
                            # HTML('<iframe src="jenn_demo.mp4" title="demo_video"</iframe>'),
                            # HTML('<iframe width="280" height="157.5" src="https://www.youtube.com/embed/W86cTIoMv2U" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                            fluidPage(
                            tags$video(id="demo_video", type = "video/mp4", width = "400px", width = "200px", src = "FINAL_1.mkv", controls = "controls"),
                            tags$video(id="demo_video", type = "video/mp4", width = "400px", width = "200px", src = "FINAL_2.mkv", controls = "controls"),
                            tags$video(id="demo_video", type = "video/mp4", width = "400px", width = "200px", src = "FINAL_3.mkv", controls = "controls")
                            
                            
                            )
                        )
                 )
                 
            ),  
            column(8,

                   # Panel 1 Viz
                   conditionalPanel("input.sidebar == 'preEvent'",
                        box(
                            title = "Demographic Analysis",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            width = 12,
                            status = "success",
                            
                       tabBox(
                           # title = "Demographic Analysis",
                           id = "preTabViz",    
                           width = 12,
                           tabPanel("Main Analysis", "",value = 1,
                                    plotly::plotlyOutput("preViz", height = 400)
                           ),
                           tabPanel("Spatial Visualisation", "", value = 2,
                                    leaflet::leafletOutput("preVizMap")
                           )
                       )
                     )
                   )
                   # )
                   ,
                   
                   # Panel 2
                  conditionalPanel("input.sidebar == 'postEvent'",
                       box(
                           title = "KPI Analysis",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           status = "success",
                                   tabBox(
                                       id = "postTabViz",    
                                       width = 12,
                                       tabPanel("Main Analysis", "",value = 1,
                                                plotly::plotlyOutput("postViz", height = 400)
                                       ),
                                       tabPanel("Spatial Visualisation", "", value = 2,
                                                leaflet::leafletOutput("postVizMap")
                                       )
                                   )
                       )
                  ),
                  
                  # Panel 3
                  conditionalPanel("input.sidebar == 'stratifiedAnalysis'",
                       box(
                           title = "Combined Analysis",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           status = "success",
                                   tabBox(
                                       id = "stratifiedAnalysisTabViz",    
                                       width = 12,
                                       tabPanel("Main Analysis", "",value = 1,
                                                plotly::plotlyOutput("stratifiedViz", height = 400)
                                       ),
                                       tabPanel("Spatial Visualisation", "", value = 2,
                                                leaflet::leafletOutput("stratifiedVizMap")
                                       )
                                   )
                       )
                  ),
                  # Panel 4
                  conditionalPanel("input.sidebar == 'report'",
                                       box(title = "Download Report",
                                           solidHeader = TRUE,
                                           collapsible = TRUE,
                                           collapsed = FALSE,
                                           width = 12,
                                           status = "success",
                                           ## DOESN'T DOWNLOAD ANYTHING
                                           downloadButton("downloadHTML",
                                                          "Download HTML Report (Recommended)"
                                                          ),
                                           downloadButton("downloadPDF",
                                                          "Download PDF Report"
                                           )
                                       )
                                   )
                  
                  
            ),
            
            # Panel 1 Data
            column(12,
                conditionalPanel("input.sidebar == 'preEvent'",   
                    box(title = "Data",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed = TRUE,
                        width = 12,
                        status = "primary",
        
                        DT::DTOutput("preCleanedData", height = 300)
                        )
                )
            ),
            
            
            ## Panel 2 Data
            
            column(12,
                   conditionalPanel("input.sidebar == 'postEvent'",
                                    box(title = "Data",
                                        status = "primary",
                                        solidHeader = TRUE,
                                        collapsible = TRUE,
                                        collapsed = TRUE,
                                        width = 12,

                                        DT::DTOutput("postCleanedData", height = 300)
                                    )
                   )
            ),
            
            ## Panel 3 Data
            
            column(12,
                   conditionalPanel("input.sidebar == 'stratifiedAnalysis'",
                                    box(title = "Data",
                                        status = "primary",
                                        solidHeader = TRUE,
                                        collapsible = TRUE,
                                        collapsed = TRUE,
                                        width = 12,
                                        
                                        DT::DTOutput("stratifiedAnalysisCleanedData", height = 300)
                                    )
                   )
            )
            
            
            
        )
        
    )
    
    
    
)



server <- function(input, output, session) {
    
    sheetInitialisation = reactive({
        files = drive_ls(input$driveID, recursive = TRUE, type = "spreadsheet")
    })


    metaData = reactive({name_processing(sheetInitialisation()$name)})
    
    # Updating Survey Choices
    # observeEvent(input$driveID,{
    #              if (!(is.null(input$emailID) & is.null(input$driveID))){
    # 
    #                  files = sheetInitialisation()
    #                  choices = files$name
    #                  updateSelectizeInput(session,
    #                                       "sheet",
    #                                       choices = choices,
    #                                       selected = choices[1],
    #                                       server = TRUE)
    #              }
    #          }
    #     )

    # Updating inputs with choices corresponding to analysis type
    observeEvent({input$analysisType
                  input$driveID},{
        
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
    observeEvent(c(input$preVar1,input$preVarColour, input$analysisType),{
        
        if (input$analysisType == "Event"){
            title = paste(input$preVar1,"Barplot") %>% str_replace_all("_", " ") %>% str_to_title()
            xaxis = input$preVar1 %>% str_replace_all("_", " ") %>% str_to_title()
        }else{
            title = paste(input$preVar1,"Barplot by Year") %>% str_replace_all("_", " ") %>% str_to_title()
            xaxis = "year" %>% str_to_title()
        }
        
        updateTextInput(session,
                        "preTitle", 
                         value = title
                        )
        
        updateTextInput(session,
                        "preXaxis", 
                        value = xaxis
                        )
        
        updateTextInput(session,
                        "preYaxis", 
                        value =  "Number of Responses"
                        )
    }
    )
    
         ## PreEvent read data
    preEventData = reactive({
        
        validate(
            need(input$sheet, "Please select file/File is being loaded from the drive.")
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
        processedData = preprocessing_multiple_fn(dataframe_ls = data, excel_ls_name = file_names)
        processedData
        })
    
    
    
    
    
    
        ## PreEvent Datatable
    
    output$preCleanedData =  renderDT({
            datatable(
                preEventData(),
                options = list(pageLength=10, scrollX='400px')
            )
        })

    
        ## PreEvent Visualisations 
    
    output$preViz = renderPlotly({
        
        data = preEventData()
        data = data %>% 
            mutate(year = as.Date(as.character(year), format = "%Y") %>% lubridate::year())
        
        data$age_group = data$age_group %>% factor(age_range_options)
        additional = input$prePlotOptions

        if (input$analysisType %in% c("Location", "Yearly")){
            g = PreEventPlotYear(data,varNames = c("year", input$preVar1), additional = additional)
        }else{
            if (input$preVarColour == "None"){
                g = PreEventPlot(data,varNames = c(input$preVar1), additional = additional)
            }else{
                g = PreEventPlot(data,varNames = c(input$preVar1, input$preVarColour), additional = additional)
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
    
    
    output$preVizMap = leaflet::renderLeaflet({
        
        data = preEventData()
        if (input$preMapAnalysisType == "Participants"){
            map_one_variable(data, input$preVar1)            
        }else{
            data %>% dplyr::select(-postcode) %>% dplyr::rename(postcode = "postcode_event") %>% 
                map_one_variable(input$preVar1) 
        }

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
            need(input$sheet, "Please select file/File is being loaded from the drive.")
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
        
        validate(need(file_names, "The file selected doesn't have post-event data available"))
        # data = read_sheet(drive_get(file_names))
        data = file_names %>% lapply(drive_get) %>% lapply(read_sheet)
        processedData = postprocessing_multiple_fn(data, file_names)
        processedData
    })


    output$postCleanedData = renderDT({
        datatable(
            postEventData(),
            options = list(pageLength=10, scrollX='400px')
        )
    })
    
    
    output$downloadPostEvent <- downloadHandler(
        filename = function(){"postEvent.csv"}, 
        content = function(fname){
            write.csv(postEventData(), fname)
        }
    )
    
    
    
    ## Pre-event Advanced Options - Labelling
    observeEvent(c(input$postPlotOptions, input$analysisType),{
        
        if (input$analysisType == "Event"){
            title = paste("KPI","Barplot") 
            xaxis = "KPI" 
            yaxis = "KPI"
        }else{
            title = paste("Yearly KPI Scores")
            xaxis = "Year"
            yaxis = "KPI"
        }
        
        updateTextInput(session,
                        "postTitle", 
                        value = title
        )
        
        updateTextInput(session,
                        "postXaxis", 
                        value = xaxis
        )
        
        updateTextInput(session,
                        "postYaxis", 
                        value =  yaxis
        )
    }
    )
    
    
    output$postViz = renderPlotly({
        
        data = postEventData()
        data = data %>% 
            dplyr::select(Action = action_kpi, Learning = learning_kpi, Community = community_kpi,year = year) %>% 
            mutate(year = as.Date(as.character(year), format = "%Y") %>% lubridate::year())

        if (input$analysisType %in% c("Location", "Yearly")){
            g = postEventYearPlot(data)
            
        }else{
            additional = input$postPlotOptions
            data = data %>% mutate_all(mean, na.rm = TRUE) %>% dplyr::slice(1) %>% gather(-year, key = "KPI", value = "score")
            g = PostEventPlot(df = data, additional = additional)
        }
        
        if (input$advancedPostOptions == TRUE){
            if (!is.null(input$postTitle)){
                g = g + ggtitle(input$postTitle)
            }
            if (!is.null(input$postXaxis)){
                g = g + xlab(input$postXaxis)
            }
            if (!is.null(input$postYaxis)){
                g = g + ylab(input$postYaxis)
            }

        }
        

        ggplotly(g)
        
        
        
    })
    
    output$postVizMap = leaflet::renderLeaflet({
        data = postEventData()
        kpi = str_to_lower(input$postEventKPI)
        map_kpi(data, kpi)
        
        
        
    })
    
    
    
    
    
    # Stratified tab
    
    stratifiedData = reactive({
        stratifiedData = preEventData() %>%
            inner_join(postEventData() %>% 
                           select(-year, -postcode_event) %>% 
                           dplyr::rename(email = "email address"), by = c("email", "location"))
        stratifiedData
        })
    

    output$stratifiedAnalysisCleanedData = renderDT({
        datatable(
            stratifiedData(),
            options = list(pageLength=10, scrollX='400px')
        )
    })

    
    output$bucket <- renderUI({

        if (input$filteringVariable == "None"){
            NULL
        } else{
            fv = input$filteringVariable
            filterVars = stratifiedData() %>% dplyr::rename(var1 = fv) %>% pull(var1) %>% unique()
            bucket_list(
                header = "Please drag away the columns you don't want in the KPI calculation:",
                group_name = "bucket_list_group",
                orientation = "horizontal",
                add_rank_list(text = "Visualised Categories",
                              labels = filterVars, input_id = "nonFiltered"),
                add_rank_list(text = "Non-Visualised Categories",
                              labels = NULL,
                              input_id = "bucket2")
            )  
        }
        


    })
    
    
    
    
    observeEvent(c(input$postStratifiedOptions, input$analysisType),{
        
            if (input$analysisType == "Event"){
                title = paste("KPI","Barplot") 
                xaxis = "KPI" 
                yaxis = "KPI"
            }else{
                title = paste("Yearly KPI Scores")
                xaxis = "Year"
                yaxis = "KPI"
            }
            
            updateTextInput(session,
                            "stratifiedTitle", 
                            value = title
            )
            
            updateTextInput(session,
                            "stratifiedXaxis", 
                            value = xaxis
            )
            
            updateTextInput(session,
                            "stratifiedYaxis", 
                            value =  yaxis
            )
        }
    )
    
    
    output$stratifiedViz = renderPlotly({
        
 
        if (input$filteringVariable == "None"){
           data = stratifiedData()
        }else{
           data =  stratifiedData() %>% filter(get(input$filteringVariable) %in% input$nonFiltered)
        }
        data$age_group = data$age_group %>% factor(age_range_options)
        
        if (input$stratifiedVarColour == "None"){
            demographic_name = ""
            data = data %>% mutate(demographic = "All Participants")
        }else{
            demographic_name = input$stratifiedVarColour
            data =  data %>%
                dplyr::rename(demographic = input$stratifiedVarColour)
        }

    
        data = data %>%
            dplyr::select(Action = action_kpi, Learning = learning_kpi, Community = community_kpi,year = year, demographic) %>%
            mutate(year = as.Date(as.character(year), format = "%Y") %>% lubridate::year()) 

        if (input$analysisType != "Event"){
            data = data %>% 
                select(value = input$stratifiedEventKPI, demographic, year) %>% 
                mutate(KPI = input$stratifiedEventKPI) %>% 
                group_by(demographic, year, KPI)  %>% 
                dplyr::summarise(value = mean(value)) %>% 
                ungroup() %>% 
                mutate(year = as.Date(as.character(year), format = "%Y") %>% lubridate::year())
            g = yearlyStratifiedVizPlot(data,  kpi_name = input$stratifiedEventKPI, demographic_name = demographic_name)
            
        }else{
            additional = input$stratifiedPlotOptions
            data = data %>% select(KPI = input$stratifiedEventKPI, demographic)  
    
            
            g = stratifiedEventPlot(data, varNames = "demographic", demographicName = demographic_name, kpi = input$stratifiedEventKPI, additional = additional)

            
        }




        if (input$advancedStratifiedOptions == TRUE){
            if (!is.null(input$stratifiedTitle)){
                g = g + ggtitle(input$stratifiedTitle)
            }
            if (!is.null(input$postXaxis)){
                g = g + xlab(input$stratifiedXaxis)
            }
            if (!is.null(input$postYaxis)){
                g = g + ylab(input$stratifiedYaxis)
            }

        }
        
        

        
        ggplotly(g)

    })


    output$stratifiedVizMap = leaflet::renderLeaflet({
        
        if (input$filteringVariable == "None"){
            data = stratifiedData()
        }else{
            data = stratifiedData() %>% filter(get(input$filteringVariable) %in% input$nonFiltered)
        }
        
        kpi = str_to_lower(input$stratifiedEventKPI)
        if (input$stratifiedMapAnalysisType == "Participants"){
            data %>% dplyr::select(-postcode_event) %>% dplyr::rename(postcode_event = "postcode") %>%
                map_kpi(kpi)
        }else{
            map_kpi(data, kpi)
        }

    
        
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # Report
    
    
    ## Participant Count - Pre Event
    p1 = reactive({
        data = preEventData()
        data$age_group = data$age_group %>% factor(age_range_options)
        
        if (input$analysisType == "Event"){
            PreEventPlot(data, varNames = c("age_group"), additional = c("horizontal", "numeric_text", "missing"))
        }else{
            PreEventPlot(data, varNames = c("year", "age_group"), additional = c("horizontal", "numeric_text", "proportions", "missing")) +
                ggtitle("Annual Participant Attendance")
        }

    })
    
    
    p7 = reactive({
        data = preEventData()
        data$age_group = data$age_group %>% factor(age_range_options)
        
        if (input$analysisType == "Event"){
            PreEventPlot(data, varNames = c("origin"), additional = c("horizontal", "numeric_text", "missing"))
        }else{
            PreEventPlot(data, varNames = c("year", "origin"), additional = c("horizontal", "numeric_text", "proportions", "missing")) +
                ggtitle("Annual Indigenous Participant Attendance")
        }
    })
    
    ## Map - Pre event
    p2 = reactive({
        data = preEventData()
        map_one_variable(data, "previous_attendance") 
    })
    
    
    

    p3 = reactive({
        data = postEventData()
        data = data %>% 
            dplyr::select(Action = action_kpi, Learning = learning_kpi, Community = community_kpi,year = year) %>% 
            mutate(year = as.Date(as.character(year), format = "%Y") %>% lubridate::year())
        
        if (input$analysisType %in% c("Location", "Yearly")){
            g = postEventYearPlot(data) + ggtitle("Annual KPI Analsyis")
            
        }else{
            additional = input$postPlotOptions
            data = data %>% mutate_all(mean, na.rm = TRUE) %>% dplyr::slice(1) %>% gather(-year, key = "KPI", value = "score")
            g = PostEventPlot(df = data, additional = c("horizontal", "numeric_text"))
        }

    })

    
    p4 = reactive({
        data = postEventData() %>% 
            rowwise() %>% 
            mutate(`Average KPI` = (learning_kpi+community_kpi+action_kpi)/3 ) %>% 
            ungroup()
        
        map_kpi(data, "average KPI")
        
    })
    
    p5 = reactive({
        data =  stratifiedData() %>% filter(previous_attendance %in% c("Yes", "No"))
        data$age_group = data$age_group %>% factor(age_range_options)

        demographic_name = "previous_attendance"
        data =  data %>%
            dplyr::rename(demographic = demographic_name) %>% 
            dplyr::select(Action = action_kpi, Learning = learning_kpi, Community = community_kpi,year = year, demographic) %>%
            mutate(year = as.Date(as.character(year), format = "%Y") %>% lubridate::year()) %>% 
            rowwise() %>% 
            mutate(`Average KPI` = (Action+Learning+Community)/3) %>% 
            ungroup()
            

        if (input$analysisType != "Event"){
            data = data %>% 
                select(value = "Average KPI", demographic, year) %>% 
                mutate(KPI = "Average KPI") %>% 
                group_by(demographic, year, KPI)  %>% 
                dplyr::summarise(value = mean(value)) %>% 
                ungroup() %>% 
                mutate(year = as.Date(as.character(year), format = "%Y") %>% lubridate::year())
            
            g = yearlyStratifiedVizPlot(data,  kpi_name = "Average", demographic_name = demographic_name)
            
        }else{
            data = data %>% select(KPI = "Average KPI", demographic)
            g = stratifiedEventPlot(data, varNames = "demographic",
                                    demographicName = demographic_name, 
                                    kpi = "Average KPI", 
                                    additional = c("horizontal", "missing", "numeric_text")) +
                                        ylab("Average KPI Scores")
            
        }

    })
    
    
    p6 = reactive({
        data = stratifiedData()
        data = data %>% dplyr::select(-postcode_event) %>%
            dplyr::rename(postcode_event = "postcode") %>%
            rowwise() %>% 
            mutate(`Average KPI` = (learning_kpi+community_kpi+action_kpi)/3 ) %>% 
            ungroup() %>% 
        map_kpi("average KPI")
        
    })

    
    output$downloadHTML <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "report.html",
        
        content = function(file) {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            tempReport <- file.path(tempdir(), "reportHTML.Rmd")
            file.copy("reportHTML.Rmd", tempReport, overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            
            id <- showNotification(
                "Rendering report...", 
                duration = NULL, 
                closeButton = FALSE
            )
            on.exit(removeNotification(id), add = TRUE)
            
            params = list(v1 = p1(), v2 = p2(), v3 = p3(), v4 = p4(), v5 = p5(), v6 = p6(), v7 = p7())

            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
        }
    )
    
    
    output$downloadPDF <- downloadHandler(
        filename = "report.pdf",
        
        content = function(file) {
            
            mapshot(p2(), file = "map1.png")
            mapshot(p4(), file = "map2.png")
            mapshot(p6(), file = "map3.png")
            
            tempReport <- normalizePath('reportPDF.Rmd')
            map1 <- normalizePath('map1.png')
            map2 <- normalizePath('map2.png')
            map3 <- normalizePath('map3.png')

            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            
            file.copy(tempReport, 'reportPDF.Rmd')
            file.copy(map1, 'map1.png')
            file.copy(map2, 'map2.png')
            file.copy(map3, 'map3.png')
            # x1 = getwd()
            # x2 = dir()
            # 
            # print(x1)
            # print(x2)
                
            setwd("/srv/connect/apps/1_SeasideSide")

            
            id <- showNotification(
                "Rendering report...", 
                duration = NULL, 
                closeButton = FALSE
            )
            on.exit(removeNotification(id), add = TRUE)
            
            params = list(v1 = p1(), v2 = p2(), v3 = p3(), v4 = p4(), v5 = p5(), v6 = p6(), v7 = p7())

            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
        }
    )
    

    
    
    
    
    output$messageMenu <- renderMenu({
        dropdownMenu(type = "messages", 
                     messageItem(from = "HELP", message = "PLEASEs"))
    })
    
    
    

    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

