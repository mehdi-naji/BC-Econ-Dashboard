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
library(grid)
library(gtable)

options(shiny.autoreload = TRUE)

# =================================================================== #
# --------------------------DATA ------------------------------------ #
# =================================================================== #

url_GDPEMPL <- "https://github.com/mehdi-naji/BC-Econ-Dashboard/raw/main/data/processed/GDPEMPL_Industry_dash.csv"

df <- read.csv(url_GDPEMPL, header = TRUE)

dff <- na.omit(df)

Provinces <- unique(df$GEO)
Industries <- unique(df$NAICS)
year_range_df <- unique(df$Year)
year_range_dff <- unique(dff$Year)

year_min_range <- min(year_range_df)
year_max_range <- max(year_range_df)

year_min <- min(year_range_dff)
year_max <- max(year_range_dff)

# =================================================================== #
# ------------------------------SHINY UI----------------------------- #
# =================================================================== #
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "cyborg"),
  titlePanel("Province-Industry GDP, Employment, and Investment"),
  fluidRow(
    column(2,
      sliderInput("range", "Year Range:", 
                  min = year_min_range, max = year_max_range,
                  value = c(year_min, year_max), step = 1,
                  sep = ""),
      radioButtons("GDPtype", label = "Select the GDP type:",
                   choices = list("Real GDP (Chained-2017 dollars)" = 1, "Nominal GDP (Current Price)" = 2), 
                   selected = 1),
      uiOutput("province_dropdown"),
      uiOutput("industry_dropdown"),
      downloadButton("download1", "Download as CSV")
    ), 
    column(10, 
           fluidRow(
             column(5),
             column(4, offset = 3,
                    radioButtons("type1", label = "",
                                 choices = list("Sort by GDP" = 1, "Sort by Employment" = 2), 
                                 selected = 1, inline= TRUE)),
             column(1)
                    ),
          plotlyOutput("gdpemp")
  )),
  fluidRow(
    column(6, plotlyOutput("gdplevel")),
    column(6, plotlyOutput("gdpgrowth"))
    ),
  fluidRow(
    column(6, plotlyOutput("employment")),
    column(6, plotlyOutput("unemprate"))
    ),
  
  fluidRow(
    column(6, plotOutput("legend")),
    column(6, plotlyOutput("investment"))),
)


# =================================================================== #
# ------------------------------SHINY SERVER------------------------- #
# =================================================================== #

server <- function(input, output, session) {
  thematic::thematic_shiny()
  
  # ======Get Data for Reactivity====== #
  
   filtered_df <- reactive({
     if (input$GDPtype == 1) {
       df |> filter(GEO %in% input$province, 
                    Year >= input$range[1] & Year <= input$range[2],
                    NAICS %in% input$industry) |>
             rename(GDP = Chained_2017,
                    GDPG = RGDPG)
     } else {
       df |> filter(GEO %in% input$province, 
                    Year >= input$range[1] & Year <= input$range[2],
                    NAICS %in% input$industry) |>
         rename(GDP = CurrentValue,
                GDPG = NGDPG)
     }
    })
    
    filtered_df2 <- reactive({
      if (input$GDPtype == 1) {
        df |> filter_at(vars(Employment, Chained_2017, CurrentValue), all_vars(!is.na(.))) |>
              filter(GEO %in% input$province[1],
                     Year == max(Year))  |>
              mutate(color = ifelse(NAICS %in% input$industry, "Bold", "Passive"))|>
              rename(GDP = Chained_2017,
                     GDPG = RGDPG)
      } else {
        df |> filter_at(vars(Employment, Chained_2017, CurrentValue), all_vars(!is.na(.))) |>
          filter(GEO %in% input$province[1],
                 Year == max(Year))  |>
          mutate(color = ifelse(NAICS %in% input$industry, "Bold", "Passive"))|>
          rename(GDP = CurrentValue,
                 GDPG = NGDPG)
      }
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
    p1_data <- na.omit(p1_data)
    
    max_gdp <- max(p1_data$GDP, na.rm = TRUE)
    max_emp <- max(p1_data$Employment, na.rm = TRUE)
    
    ratio <- max_gdp / max_emp
    
    if (input$type1 == 1) {
      p1 <- p1_data |>
        ggplot() +
        geom_bar(data = p1_data,
                 aes(x = reorder(NAICS,GDP), y=GDP, color= color, fill = GEO), stat = "identity", position = "dodge")
    } else {
      p1 <- p1_data |>
        ggplot() +
        geom_bar(data = p1_data,
                 aes(x = reorder(NAICS,Employment), y=GDP, color= color, fill = GEO), stat = "identity", position = "dodge")
   }
    
    p1 <- p1 +
        geom_point(data = p1_data,
                 aes(x = reorder(NAICS,GDP), y=Employment * ratio,
                     text = paste0("Province" = GEO, "\n",
                                   "Industry" = NAICS, "\n",
                                   "Year = ", Year,"\n" ,
                                   "Industry GDP = $", GDP," million", "\n",
                                   "Employmnet = ", Employment, " thousands")), color = "yellow", size = 3)+
      coord_flip() +
      scale_fill_manual(values = c("Bold" = "blue"))+
      labs(y = "Industry GDP", 
           title = paste0("Industry GDP and Employment of ",
                          input$province[1],
                          " in ",
                          max(p1_data$Year)),
           x="")+
      theme(legend.position = "none",
            axis.text = element_text(size = 14),
            plot.title = element_text(size = 25))
    
    ggplotly(p1, tooltip="text")
    
  })
  
  
  
  plt2 <- renderPlotly({
    p2_data <- filtered_df()
    
    if (input$GDPtype ==1) {xxx = "Real GDP (Chained-2017 dollars)"
                    } else {xxx = "Nominal GDP (Current Value)"}
    
    p2 <- ggplot(p2_data)+
      aes(x = Year , fill = interaction(GEO, NAICS), color = interaction(GEO, NAICS)) +
      geom_line(aes(y = GDP))+
      labs(title = paste0("Province-Industry ",xxx," time series"),
           y = "Million Dollars")+
      theme(legend.position = "none",
            plot.title = element_text(size = 20))
    
    
    # Convert ggplot to plotly
    p2 <- ggplotly(p2)
  })
  
  plt3 <- renderPlotly({
    p3_data <- filtered_df()
    
    if (input$GDPtype ==1) {xxx = "Real GDP (Chained-2017 dollars)"
    } else {xxx = "Nominal GDP (Current Value)"}
    
    p3 <- ggplot(p3_data)+
      aes(x = Year , fill = interaction(GEO, NAICS), color = interaction(GEO, NAICS)) +
      geom_bar(aes(y = GDPG, 
                   text = paste0("Province" = GEO, "\n",
                                 "Industry" = NAICS, "\n",
                                 "Year = ", Year,"\n" ,
                                 "Industry GDP = $", GDP," million", "\n",
                                 "Growth = ", round(GDPG,digits=2), "%")), 
               stat = "identity", position = "dodge")+
      labs(title = paste0("Province-Industry " ,xxx, " Growth"),
           y = "Million Dollars")+
      theme(legend.position = "none",
            plot.title = element_text(size = 20))
    
    
    p3 <- ggplotly(p3, tooltip = "text")
  })
  
  plt4 <- renderPlotly({
    p4_data <- filtered_df()
    
    p4 <- ggplot(p4_data)+
      aes(x = Year , fill = interaction(GEO, NAICS), color = interaction(GEO, NAICS)) +
      geom_line(aes(y = Employment))+
      labs(title = "Province-Industry Empoyment time series",
           y = "Thouhsand persons")+
      theme(legend.position = "none",
            plot.title = element_text(size = 20))
    
    
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
      labs(title = "Province-Industry Unemployment Rate",
           y = "Percent")+
      theme(legend.position = "none",
            plot.title = element_text(size = 20))
    
    p5 <- ggplotly(p5, tooltip = "text")
  })

  plt_investment <- renderPlotly({
    pi_data <- filtered_df()
    pi <- ggplot(pi_data)+
      aes(x = Year , fill = interaction(GEO, NAICS), color = interaction(GEO, NAICS)) +
      geom_bar(aes(y = Investment, 
                   text = paste0("Province" = GEO, "\n",
                                 "Industry" = NAICS, "\n",
                                 "Year = ", Year,"\n" ,
                                 "Industry GDP = $", GDP," million", "\n",
                                 "Growth = ", round(GDPG,digits=2), "%")), 
               stat = "identity", position = "dodge")+
      labs(title = "Province-Industry Investment (Capital Expenditure) - Current Value",
           y = "Million Dollars")+
      theme(legend.position = "none",
            plot.title = element_text(size = 16))
    
    pi <- ggplotly(pi, tooltip = "text")
   
  })
  
  
  plt_legend <- renderPlot({
    pi_data <- filtered_df()
    
    pi <- ggplot(pi_data)+
      aes(x = Year , fill = interaction(GEO, NAICS)) +
      geom_bar(aes(y = Investment), 
               stat = "identity", position = "dodge")+
      labs(fill = "The Combined Legend") + 
      theme(legend.text = element_text(size = 25), 
            legend.title = element_text(size = 25)) 
    g <- ggplotGrob(pi)
    
    legend <- gtable::gtable_filter(g, "guide-box")
    
    grid::grid.draw(legend)
    
  })
  
  output$gdpemp <- plt1
  output$gdplevel <- plt2
  output$gdpgrowth <- plt3
  output$employment <- plt4
  output$unemprate <- plt5
  output$investment <- plt_investment
  output$legend <- plt_legend
  
  # Download handler
  output$download1 <- downloadHandler(
    filename = "data.csv", 
    content = function(file) {
      write.csv(filtered_df(), file)})}

    

  
  
# Run the Shiny app
shinyApp(ui = ui, server = server)


