# =================================================================== #
# ------------------------------IMPORTS------------------------------ #
# =================================================================== #
library(shiny)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(lubridate)
library(plotly)
library(leaflet)
library(cowplot)
options(shiny.autoreload = TRUE)

# =================================================================== #
# --------------------------DATA ------------------------------------ #
# =================================================================== #

url_GDP <- "https://github.com/mehdi-naji/BC-Econ-Dashboard/raw/main/data/processed/GDP_Industry_dash.csv"
url_EMPL <- "https://github.com/mehdi-naji/BC-Econ-Dashboard/raw/main/data/processed/EMPL_Industry_dash.csv"

GDP_df <- read.csv(url_GDP, header = TRUE)
EMPL_df <- read.csv(url_EMPL, header = TRUE)




Provinces <- intersect(unique(EMPL_df$GEO), unique(GDP_df$GEO))
Industries <- intersect(unique(EMPL_df$NAICS), unique(GDP_df$NAICS))
year_range <- union(unique(EMPL_df$Year), unique(GDP_df$Year))
year_min <- min(year_range)
year_max <- max(year_range)
options <- c('Employment', 'Unemployment Rate')


# =================================================================== #
# ------------------------------SHINY UI----------------------------- #
# =================================================================== #


# Define the UI
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  titlePanel("Households' Spending Distribution and Growth Incidence Chart"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("range", "Year Range:", 
                  min = year_min, max = year_max,
                  value = c(year_min, year_max), step = 5,
                  sep = ""),
      uiOutput("province_dropdown"),
      uiOutput("industry_dropdown"),
      radioButtons('option', 'Select Metric', options),
      verbatimTextOutput("textbox")
    ),
    mainPanel(
      plotlyOutput("barChart1"),
      plotlyOutput("barChart2"),
    )
  )
)

# =================================================================== #
# ------------------------------SHINY SERVER------------------------- #
# =================================================================== #

server <- function(input, output, session) {
  thematic::thematic_shiny()
  
  # ======Get Data for Reactivity====== #
  filtered_df <- reactive({
      df |> filter(GEO %in% input$province, 
                  Year >= input$range[1] & Year <= input$range[2],
                  NAICS %in% input$industry) 
  })
  
    # filtered_EMPL <- reactive({
  #   if (input$option == "Employment"){
  #     EMPL_df |> filter(Labour.force.characteristics == "Emplyment",
  #                             GEO %in% input$province, 
  #                             Year >= input$range[1] & Year <= input$range[2],
  #                             NAICS %in% input$industry)
  #   } else {EMPL_df |> filter(`Labour force characteristics` == "Unemployment rate",
  #                                   GEO %in% input$province, 
  #                                   Year >= input$range[1] & Year <= input$range[2],
  #                                   NAICS %in% input$industry)}
  # })
  
  
  # ======Server Side of Province Input====== #
  output$province_dropdown <- renderUI({
    selectInput("province", 
                "Select Provinces", 
                Provinces, 
                multiple = TRUE, 
                selected = c('British Columbia')
    )
  })
  
  # ======Server Side of Industry Input====== #
  output$industry_dropdown <- renderUI({
    selectInput("industry", 
                "Select Industries", 
                Industries, 
                multiple = TRUE, 
                selected = c('Construction [23]')
    )
  })
  
  # ======Plot 1 - Annual Average Line Plot======
  output$barChart1 <- renderPlotly({
    p1 <- ggplot(filtered_df()) +
      aes(x = Year, 
          y = GDP, 
          color = interaction(GEO, NAICS)) + 
      geom_line(position = position_dodge()) +
      theme(panel.grid.major.y = element_line(color = "grey",
                                              size = 0.5,
                                              linetype = 2)) + 
      ggtitle(paste0("Distribution of Expenditures in Current Dollars")) +
      ylab(paste("Dollars", "input$year")) +
      xlab("") +
      theme(text = element_text(size = 12),
            plot.title = element_text(face = "bold")) +
      labs(color = "Combined Legend")
    
    # Convert ggplot to plotly
    p1 <- ggplotly(p1)
    
    # Modify the legend position
    p1 <- p1 %>% layout(legend = list(orientation = "h", x = 0, y = -0.5))
    
    # Return the modified plotly object
    p1
  })
  
  
  # ======Plot 2 - Annual Average Line Plot====== #
  p2 <- ggplot()  # Initialize empty plot
  output$barChart2 <- renderPlotly({
    p2 <- ggplot(filtered_df()) +
      aes(x = Year, 
          y = input$option, 
          fill = interaction(GEO, NAICS, name = "Legend")) 
    # +  
    #   geom_col(position = position_dodge()) +
    #   theme(panel.grid.major.y = element_line(color = "grey",
    #                                           size = 0.5,
    #                                           linetype = 2)) +
    #   ggtitle("Distribution of Expenditure Growth in Percent Change") +
    #   ylab("Growth rate") +
    #   xlab("")+
    #   scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    #   theme(text = element_text(size = 12),
    #         plot.title = element_text(face = "bold"),
    #         axis.title = element_text(face = "bold"))+
    #   labs(fill = "Combined Legend")+
    #   theme(legend.position = "none")
    
    # Return the ggplot object
    p2 %>% plotly::ggplotly()
    
  })
  # Create a reactive expression for the text
  text_content <- reactive({
    # You can modify this section to generate the text you want to display
    text <- paste("This is a sample text box.", 
                  "You can add more sentences or content here.")
    return(text)
  })
  
  # Render the text content in the text box
  output$textbox <- renderText({
    text_content()
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)


