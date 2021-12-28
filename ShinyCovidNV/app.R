#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#' TODO this will not work without a bit of organisation of the underlying 
#' functions used. The most repeatable way would be to make the generation of 
#' data and  plotting be part of a package which then could be loaded.

#generate master data once
getwd()
#TODO current attempt, to be attempted again 
setwd("..")
#JH plotting and data extracting functions
source("./Functions/JHFunctions.R")
source("./Scripts/JHGetData.R", encoding='utf-8')


library(tidyverse)
library(lubridate)
library(plotly)
options(digits = 3)   # report 3 significant digits
options(scipen = -2)

#list of unique countries to pick from
countries_unique <- unique(master_data$Country_Region)

#list of variables to pick from
var_unique <- names(master_data)

#min and max dates availablw
minDate <- min(master_data$Date)
maxDate <- max(master_data$Date)


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Covid Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput(inputId = "SelectedCountries",
                           choices = countries_unique,
                           label = "Select Countries",
                           multiple = TRUE) ,
            selectizeInput(inputId = "SelectedVariable",
                           choices = var_unique,
                           label = "Select Variable",
                           multiple = FALSE),
            
            dateRangeInput(inputId = "SelectedDates",
                           label = "Select Date range:",
                           min = minDate,
                           max = maxDate)
            #TODO add date time selection
        ),

        
        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("covidPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$covidPlot <- renderPlotly({
      theplot <- JHGetplot_CountryLevel(
        JH_Data = (master_data %>%
                     filter(Date >= input$SelectedDates[1] & Date <= input$SelectedDates[2])),
        CountryList = input$SelectedCountries,
        VarName = input$SelectedVariable)
      ggplotly(theplot,dynamicTicks = TRUE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
