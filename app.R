library(shiny)
library(tidyverse)
library(DT)

melb <- read_csv("melb_data.csv")

ui <- fluidPage(
  titlePanel("Melbourne Housing Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Filters"),
      
      checkboxGroupInput(
        "type",
        "Property Type:",
        choices = c(
          "House" = "h",
          "Townhouse" = "t",
          "Unit" = "u"
        ),
        selected = c("h", "t", "u")
      ),
      
      checkboxGroupInput(
        "region",
        "Region:",
        choices = sort(unique(na.omit(melb$Regionname))),
        selected = sort(unique(na.omit(melb$Regionname))),
      ),
      
      selectInput(
        "num_var1",
        "First Numerical Variable:",
        choices = c(
          "Price" = "Price",
          "Rooms" = "Rooms",
          "Distance" = "Distance",
          "Bedrooms" = "Bedroom2",
          "Bathrooms" = "Bathroom",
          "Car Spaces" = "Car",
          "Land Size" = "Landsize"
        ),
        selected = "Price"
      ),
    uiOutput("num_slider1"),
    selectInput(
      "num_var2",
      "Second Numerical Variable:",
      choices = c(
        "Price" = "Price",
        "Rooms" = "Rooms",
        "Distance" = "Distance",
        "Bedrooms" = "Bedroom2",
        "Bathrooms" = "Bathroom",
        "Car Spaces" = "Car",
        "Land Size" = "Landsize"
      ),
      selected = "Distance"
    ),
    
    uiOutput("num_slider2"),
    
      actionButton(
        "apply_filters",
        "Apply Filters"
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "About",
          h2("About the Melbourne Housing Explorer"),
          
          p(
            "This app allows users to explore housing data from Melbourne, Australia.
    Users can filter the data by property type, region, and two numerical
    variables to explore different characteristics of the housing market."
          ),
          
          h3("About the Data"),
          
          p(
            "The dataset contains information about residential properties in Melbourne,
  Australia, including property prices, property types, number of rooms,
  distance from the city center, land size, and geographic region."
          ),
          
          p(
            "Source: ",
            a(
              "Melbourne Housing Market dataset on Kaggle",
              href = "https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market",
              target = "_blank"
            )
          ),
          
          h3("Using the App"),
          
          p(
            "Use the sidebar to select property types and regions, choose two numerical
    variables and their ranges, and then click Apply Filters. The Data Download
    tab displays the filtered data and allows it to be downloaded as a CSV file.
    The Data Exploration tab provides additional summaries and visualizations
    of the selected data."
          )
        ),
        h3("Melbourne, Australia"),
        
        img(
          src = "melbourne.jpg",
          width = "100%"
        ),
        
        tabPanel(
          "Data Download",
          br(),
          downloadButton(
            "download_data",
            "Download Filtered Data"
          ),
          br(),
          br(),
          DTOutput("data_table")
        ),
        tabPanel("Data Exploration")
      )
    )
  )
)

server <- function(input, output, session) {
  output$num_slider1 <- renderUI({
    
    values <- melb[[input$num_var1]]
    
    sliderInput(
      "num_range1",
      "Select Range:",
      min = min(values, na.rm = TRUE),
      max = max(values, na.rm = TRUE),
      value = c(
        min(values, na.rm = TRUE),
        max(values, na.rm = TRUE)
      )
    )
  })
  output$num_slider2 <- renderUI({
    
    values <- melb[[input$num_var2]]
    
    sliderInput(
      "num_range2",
      "Select Range:",
      min = min(values, na.rm = TRUE),
      max = max(values, na.rm = TRUE),
      value = c(
        min(values, na.rm = TRUE),
        max(values, na.rm = TRUE)
      )
    )
  })
  filtered_data <- eventReactive(input$apply_filters, {
    
    req(input$num_range1, input$num_range2)
    
    melb |>
      filter(
        Type %in% input$type,
        Regionname %in% input$region,
        .data[[input$num_var1]] >= input$num_range1[1],
        .data[[input$num_var1]] <= input$num_range1[2],
        .data[[input$num_var2]] >= input$num_range2[1],
        .data[[input$num_var2]] <= input$num_range2[2]
      )
    
  })
  output$data_table <- renderDT({
    
    datatable(
      filtered_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
    
  })
  output$download_data <- downloadHandler(
    
    filename = function() {
      "melbourne_housing_filtered.csv"
    },
    
    content = function(file) {
      write.csv(
        filtered_data(),
        file,
        row.names = FALSE
      )
    }
    
  )
}

shinyApp(ui = ui, server = server)