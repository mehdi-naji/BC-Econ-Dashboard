# =================================================================== #
# ------------------------------IMPORTS------------------------------ #
# =================================================================== #
library(shiny)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(plotly)
library(leaflet)
library(cowplot)
options(shiny.autoreload = TRUE)

# =================================================================== #
# --------------------------DATA ------------------------------------ #
# =================================================================== #

url <- "https://github.com/mehdi-naji/BC-Econ-Dashboard/raw/main/data/processed/processed_df.csv"
df <- read.csv(url, header = TRUE)


regions <- unique(df$GEO)
Provinces <- regions[regions != "Canada"]
year_range <- unique(df$Year)
year_initial <- min(year_range)
category_range <- unique(df$Category)
Categories <- unique(df$Category)

# =================================================================== #
# ------------------------------SHINY UI----------------------------- #
# =================================================================== #


# Define the UI
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  titlePanel("Households' Spending Distribution and Growth Incidence Chart"),
  sidebarLayout(
    sidebarPanel(
      # selectInput("province", "Select Province:", choices = Provinces),
      selectInput("year", "Select Year:", choices = year_range),
      uiOutput("province_dropdown"),
      uiOutput("category_dropdown"),
      #selectInput("category", "Expenditure Category", choices =  category_range),
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
  filteredData <- reactive({
    df %>% filter(GEO %in% input$province, 
                    Year == input$year,
                    Category %in% input$category,
                    Quintile != "All quintiles"
                  
                    )
  })
  
  
  # ======Server Side of Province Input====== #
  output$province_dropdown <- renderUI({
    selectInput("province", 
                "Select Provinces", 
                Provinces, 
                multiple = TRUE, 
                selected = c('British Columbia')
                )
  })
  
  # ======Server Side of Category Input====== #
  output$category_dropdown <- renderUI({
    selectInput("category", 
                "Select Categories", 
                Categories, 
                multiple = TRUE, 
                selected = c('Total expenditure')
    )
  })
  
  # ======Plot 1 - Annual Average Line Plot======
  output$barChart1 <- renderPlotly({
    p1 <- ggplot(filteredData()) +
      aes(x = reorder(Quintile, VALUE), 
          y = VALUE, 
          fill = interaction(GEO, Category)) + 
      geom_col(position = position_dodge()) +
      theme(panel.grid.major.y = element_line(color = "grey",
                                              size = 0.5,
                                              linetype = 2)) + 
      ggtitle(paste0("Distribution of Expenditures in Current Dollars")) +
      ylab(paste("Dollars", input$year)) +
      xlab("") +
      theme(text = element_text(size = 12),
            plot.title = element_text(face = "bold")) +
      labs(fill = "Combined Legend")
    
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
    p2 <- ggplot(filteredData()) +
      aes(x = reorder(Quintile, VALUE), 
          y = GrowthRate * 100, 
          fill = interaction(GEO, Category, name = "Legend")) +  # Specify the same name for the legend
      geom_col(position = position_dodge()) +
      theme(panel.grid.major.y = element_line(color = "grey",
                                              size = 0.5,
                                              linetype = 2)) + 
      ggtitle("Distribution of Expenditure Growth in Percent Change") +
      ylab("Growth rate") +
      xlab("")+
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      theme(text = element_text(size = 12),
            plot.title = element_text(face = "bold"),
            axis.title = element_text(face = "bold"))+
      labs(fill = "Combined Legend")+
      theme(legend.position = "none")
    
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


