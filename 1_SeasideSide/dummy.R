# 
# 
# 
# library(shiny)
# library(shinydashboard)
# library(shinyWidgets)
# library(dashboardthemes)
# library(googlesheets4)
# library(googledrive)
# library(readr)
# library(sortable)
# library(lubridate)
# library(shiny)
# 
# 
# options(
#     gargle_oauth_email = TRUE,
#     gargle_oauth_cache = "1_SeasideSide/.secrets"
# )
# 
# drive_auth(cache = "1_SeasideSide/.secrets", email = TRUE)
# gs4_auth(cache = "1_SeasideSide/.secrets", email = TRUE)
# 
# sheet_id <- drive_get("Bondi Junction_2026_28.09.2021_Post_Participants")$id
# 
# 
# # Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white')
#     })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)








library("googledrive")
library("googlesheets4") # I am using the developing version 0.1.0.9000
library("shiny")
library("DT")
# You want to deploy an app in Shinyapps.io or other server
# FIRST STEP----
# Get the token an store it in a cache folder embedded in your app directory
# designate project-specific cache
# options(
#     gargle_oauth_email = TRUE,
#     gargle_oauth_cache = "1_SeasideSide/.secrets"
# )
# options(gargle_quiet = FALSE) # So you can know what is happening
# Authenticate in interactive mode (run the app) ONCE and check if the token
# has been stored inside the .secrets folder, after that just comment this line
#drive_auth() # Authenticate to produce the token in the cache folder
# Grant permission to googlesheets to access to the token produced
#sheets_auth(token = drive_token())

# SECOND STEP----
# Comment lines 10, 13 and 15 and uncomment lines 21 and 22
# You tell gargle to search the token in the secrets folder and to look
# for an auth given to a certain email (enter your email linked to googledrive!)
drive_auth(cache = ".secrets", email = TRUE)
gs4_auth(token = drive_token())


# THIRD STEP---
# Now you can deploy your app in shinyapps.io!!
# Test if your app runs properly in the local version
# Authenticate in ShinyApps.io
# rsconnect::setAccountInfo(name="<ACCOUNT>", token="<TOKEN>", secret="<SECRET>")
# setwd() in your App directory
# library(rsconnect)
# deployApp()
# Enjoy your new App!!

ui <- # Define UI for application that plots random distributions
    fluidPage(

        # Application title
        titlePanel("Hello Shiny!"),

        # Sidebar with a slider input for number of observations
        sidebarLayout(
            sidebarPanel(
                sliderInput("obs",
                            "Number of observations:",
                            min = 1,
                            max = 1000,
                            value = 500),
                actionButton(
                    "add",
                    "Add new entry")
            ),

            # Show a plot of the generated distribution
            mainPanel(
                "Check your googlesheet!!",
                DTOutput("plz"),


            )
        )
    )

server <- function(input, output, session) {
    # Expression that generates a plot of the distribution. The expression
    # is wrapped in a call to renderPlot to indicate that:
    #
    #  1) It is "reactive" and therefore should be automatically
    #     re-executed when inputs change
    #  2) Its output type is a plot
    #
    sheetInit = reactive({
        data = drive_ls("https://drive.google.com/drive/folders/1Yl_VatRKZD7HeA-CrZHaCtZXXVt2rwY5")$name[1]
        wb <- drive_get(data)
        dt <- read_sheet(wb)
    })

    output$plz = renderDT({
        sheetInit()
    })

}

shinyApp(ui, server)





