# =================================================================== #
# ------------------------------IMPORTS------------------------------ #
# =================================================================== #
library(shiny)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(tidyquant)
library(lubridate)
library(plotly)
library(leaflet)
library(cowplot)
options(shiny.autoreload = TRUE)

# =================================================================== #
# --------------------------DATA ------------------------------------ #
# =================================================================== #

url_GDPEMPL <- "https://github.com/mehdi-naji/BC-Econ-Dashboard/raw/main/data/processed/GDPEMPL_Industry_dash.csv"

df <- read.csv(url_GDPEMPL, header = TRUE)

Provinces <- unique(df$GEO)
Industries <- unique(df$NAICS)
year_range <- unique(df$Year)
year_min <- min(year_range)
year_max <- max(year_range)


# =================================================================== #
# ------------------------------SHINY UI----------------------------- #
# =================================================================== #
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  titlePanel("Canadian Provinces Production and Employment"),
  fluidRow(
    column(3,
      sliderInput("range", "Year Range:", 
                  min = year_min, max = year_max,
                  value = c(year_min, year_max), step = 1,
                  sep = ""),
      uiOutput("province_dropdown"),
      uiOutput("industry_dropdown"),
      radioButtons("radio", label = "Select an option",
                   choices = list("Option 1" = 1, "Option 2" = 2), 
                   selected = 1)
    ), column(9, plotlyOutput("cell2"))
  ),
  fluidRow(
    column(6, plotlyOutput("cell3")),
    column(6, plotlyOutput("cell4"))
    ),
  fluidRow(column(12, plotOutput("legend"))),
  fluidRow(
    column(6, plotlyOutput("cell5")),
    column(6, plotlyOutput("cell6"))
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
  
  filtered_df2 <- reactive({
    df |> filter(GEO %in% input$province[1], 
                 Year == input$range[2])  |>
          mutate(color = ifelse(NAICS %in% input$industry, "Bold", "Passive"))
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
  
  # ======Server Side of Industry Input====== #
  output$industry_dropdown <- renderUI({
    selectInput("industry", 
                "Select Industries", 
                Industries, 
                multiple = TRUE, 
                selected = c('Construction [23]')
    )
  })
  

  plt1 <- renderPlotly({
    p1_data <- filtered_df2() 
    
    max_gdp <- max(p1_data$GDP, na.rm = TRUE)
    max_emp <- max(p1_data$Employment, na.rm = TRUE)
    
    ratio <- max_gdp / max_emp
    
    p1 <- p1_data |>
      ggplot() +
      geom_bar(data = p1_data,
               aes(x = reorder(NAICS,GDP), y=GDP, color= color, fill = GEO), stat = "identity", position = "dodge") +
      geom_point(data = p1_data,
                 aes(x = reorder(NAICS,GDP), y=Employment * ratio,
                     text = paste0("Province" = GEO, "\n",
                                   "Industry" = NAICS, "\n",
                                   "Year = ", Year,"\n" ,
                                   "Industry GDP = $", GDP," million", "\n",
                                   "Employmnet = ", Employment, " thousands")), color = "yellow", size = 3)+
      coord_flip() +
      scale_fill_manual(values = c("Bold" = "blue"))+
      labs(y = "Industry GDP and Employment", title = "Horizontal Bar Plot")+
      theme(legend.position = "none")
    ggplotly(p1, tooltip="text")
    
  })
  
  
  
  plt2 <- renderPlotly({
    p2_data <- filtered_df()
    
    p2 <- ggplot(p2_data)+
      aes(x = Year , fill = interaction(GEO, NAICS), color = interaction(GEO, NAICS)) +
      geom_line(aes(y = GDP))+
      theme(legend.position = "none")
    
    
    # Convert ggplot to plotly
    p2 <- ggplotly(p2)
  })
  
  plt3 <- renderPlotly({
    p3_data <- filtered_df()
    
    p3 <- ggplot(p3_data)+
      aes(x = Year , fill = interaction(GEO, NAICS), color = interaction(GEO, NAICS)) +
      geom_bar(aes(y = GDPG, 
                   text = paste0("Province" = GEO, "\n",
                                 "Industry" = NAICS, "\n",
                                 "Year = ", Year,"\n" ,
                                 "Industry GDP = $", GDP," million", "\n",
                                 "Growth = ", round(GDPG,digits=2), "%")), 
               stat = "identity", position = "dodge")+
      theme(legend.position = "none")
    
    
    p3 <- ggplotly(p3, tooltip = "text")
  })
  
  plt4 <- renderPlotly({
    p4_data <- filtered_df()
    
    p4 <- ggplot(p4_data)+
      aes(x = Year , fill = interaction(GEO, NAICS), color = interaction(GEO, NAICS)) +
      geom_line(aes(y = Employment))+
      theme(legend.position = "none")
    
    
    # Convert ggplot to plotly
    p4 <- ggplotly(p4)
  })
  
  plt5 <- renderPlotly({
    p5_data <- filtered_df()
    
    p5 <- ggplot(p5_data)+
      aes(x = Year , fill = interaction(GEO, NAICS), color = interaction(GEO, NAICS)) +
      geom_bar(aes(y = Unemployment.rate, 
                   text = paste0("Province" = GEO, "\n",
                                 "Industry" = NAICS, "\n",
                                 "Year = ", Year,"\n" ,
                                 "Industry GDP = $", GDP," million", "\n",
                                 "Growth = ", round(GDPG,digits=2), "%")), 
               stat = "identity", position = "dodge")+
      theme(legend.position = "none")
    
    p5 <- ggplotly(p5, tooltip = "text")
  })

  plt_legend <- renderPlot({
    pl_data <- filtered_df()
    plt <- ggplot(pl_data)+
      aes(x=Year, y = Employment, fill = interaction(GEO, NAICS)) +
      geom_col()
    # Extract the legend from the ggplot object
    legend <- get_legend(plt)
    
    # Draw the legend without the plot
    # grid.newpage()
    print(legend)
  })
  
  output$cell2 <- plt1
  output$cell3 <- plt2
  output$cell4 <- plt3
  output$cell5 <- plt4
  output$cell6 <- plt5
  output$legend <- plt_legend
  
}

  
  
# Run the Shiny app
shinyApp(ui = ui, server = server)


