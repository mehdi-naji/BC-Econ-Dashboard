# =================================================================== #
# ------------------------------IMPORTS------------------------------ #
# =================================================================== #
library(shiny)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(plotly)
library(leaflet)

# =================================================================== #
# --------------------------DATA ------------------------------------ #
# =================================================================== #

url = "https://raw.githubusercontent.com/mehdi-naji/BC-Econ-Dashboard/main/data/processed/processed_df.csv?token=GHSAT0AAAAAACH6HVANUOV5JLFUJRKZSGJAZIN2R5A"
df = read.csv(url, header=TRUE)

# =================================================================== #
# ------------------------------SHINY UI----------------------------- #
# =================================================================== #

options(shiny.autoreload = TRUE)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  titlePanel('Climate Metrics in Canada'),
  
  fluidRow(
    column(3,
           sliderInput("range", "Year Range:", 
                       min = 1940, max = 2019,
                       value = c(1940, 2019), step = 5,
                       sep = ""),
           uiOutput("cities_dropdown"),
           radioButtons('option', 'Select Metric', options),
           selectInput("dataset", "Choose a dataset:", choices = c("tempreture", "precipitation")),
           downloadButton("downloadData", "Download"),
           
           leafletOutput("map")          
    ),
    
    
    column(4,
           plotlyOutput("line_plot"),
           plotlyOutput("line_plot2")),
    column(5,
           plotlyOutput("month_averages"),
           plotlyOutput("diff_plot"))
  )
)


server <- function(input, output, session) {
}
shinyApp(ui, server)

