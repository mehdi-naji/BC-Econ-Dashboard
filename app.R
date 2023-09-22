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

url <- "https://github.com/mehdi-naji/BC-Econ-Dashboard/raw/main/data/processed/processed_df.csv"
df <- read.csv(url, header = TRUE)


regions <- unique(df$GEO)
Provinces <- regions[regions != "Canada"]
year_range <- unique(df$Year)
year_initial <- min(year_range)
category_range <- unique(df$`Household expenditures, summary-level categories`)


# =================================================================== #
# ------------------------------SHINY UI----------------------------- #
# =================================================================== #

options(shiny.autoreload = TRUE)

# Define the UI
ui <- fluidPage(
  titlePanel("Province and Year Bar Charts"),
  sidebarLayout(
    sidebarPanel(
      selectInput("province", "Select Province:", choices = Provinces),
      selectInput("year", "Select Year:", choices = year_range),
      selectInput("category", "Expenditure Category", choices =  category_range)
    ),
    mainPanel(
      plotOutput("barChart1")
    )
  )
)

# Define the server
server <- function(input, output) {
  # Filter data based on user input
  filteredData <- reactive({
    df %>% filter(GEO == input$province, 
                    Year == input$year,
                    `Household expenditures, summary-level categories` == input$category,
                    `Before-tax household income quintile` != "All quintiles"
                    )
  })
  
  
  # Create Bar Chart 1
  
output$barChart1 <- renderPlot({
  ggplot(filteredData()) +
    aes(x=reorder(`Before-tax household income quintile`, VALUE), 
        y=VALUE, 
        fill=GEO) +
    geom_col(stat="identity", color="white", position=position_dodge()) +
    # scale_y_continuous(breaks = seq(-30, 40, by = 5))+
    theme(panel.grid.major.y = element_line(color = "grey",
                                            size = 0.5,
                                            linetype = 2))+ 
    ggtitle(paste0("Monthly Average of ", 
                   input$year)) +
    xlab("Month") + 
    ylab(paste("Average Monthly",
               input$year))+
    theme(text = element_text(size=12),
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          axis.text.x = element_text(angle = 90))
})
}
# Run the Shiny app
shinyApp(ui = ui, server = server)