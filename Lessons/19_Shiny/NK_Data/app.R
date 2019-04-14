#### Load packages ----
library(shiny)
library(shinythemes)
library(tidyverse)

#### Load data ----
refdata <- read_csv("Data/Refrence data Q2.csv")
refdata$repmt_date <- as.Date(refdata$repmt_date, format = "%Y-%m-%d")
refdata <- refdata %>%
  select(-repmt_amt, -repmt_curr)

#### Define UI ----
ui <- fluidPage(theme = shinytheme("yeti"),
  titlePanel("Products by repayment type"),
  sidebarLayout(
    sidebarPanel(
      
      # Select product to plot
      selectInput(inputId = "p", 
                  label = "Product",
                  choices = unique(refdata$product_code), 
                  selected = "tp_ug"),
      
      # Select frequency
      checkboxGroupInput(inputId = "fill",
                         label = "frequency ID",
                         choices = unique(refdata$repmt_freq),
                         selected = c(1, 7)),
      
      # Select repayment type
      checkboxGroupInput(inputId = "shape",
                         label = "Repayment type",
                         choices = c("Activation and Top Up", "Top Up","Unlock"),
                         selected = "Top Up"),

      # Select date range to be plotted
      sliderInput(inputId = "x",
                  label = "Date",
                  min = as.Date("2017-03-01"),
                  max = as.Date("2018-12-31"),
                  value = c(as.Date("2017-12-01"), as.Date("2018-05-31")))),

    # Output: Description, lineplot, and reference
    mainPanel(
      plotOutput("scatterplot", brush = brushOpts(id = "scatterplot_brush")), 
      tableOutput("mytable")
    )))

#### Define server  ----
server <- function(input, output) {
  
    # Define reactive formatting for filtering within columns
     filtered_nutrient_data <- reactive({
       nutrient_data %>%
         filter(sampledate >= input$x[1] & sampledate <= input$x[2]) %>%
         filter(depth_id %in% input$fill) %>%
         filter(lakename %in% input$shape) 
     })
    
    # Create a ggplot object for the type of plot you have defined in the UI  
       output$scatterplot <- renderPlot({
        ggplot(filtered_nutrient_data(), 
               aes_string(x = "sampledate", y = input$y, 
                          fill = "depth_id", shape = "lakename")) +
          geom_point(alpha = 0.8, size = 2) +
          theme_classic(base_size = 14) +
          scale_shape_manual(values = c(21, 24)) +
          labs(x = "Date", y = expression(Concentration ~ (mu*g / L)), shape = "Lake", fill = "Depth ID") +
          scale_fill_distiller(palette = "YlOrBr", guide = "colorbar", direction = 1)
          #scale_fill_viridis_c(option = "viridis", begin = 0, end = 0.8, direction = -1)
      })
       
    # Create a table that generates data for each point selected on the graph  
       output$mytable <- renderTable({
         brush_out <- brushedPoints(filtered_nutrient_data(), input$scatterplot_brush)
       })
       
  }


#### Create the Shiny app object ----
shinyApp(ui = ui, server = server)
