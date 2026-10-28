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
        selected = sort(unique(na.omit(melb$Regionname)))
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
          ),
          
          h3("Melbourne, Australia"),
          
          img(src = "melbourne.jpg", width = "100%")
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
        tabPanel(
          "Data Exploration",
          
          h2("Explore the Housing Data"),
          
          p(
            "Use the options below to explore summaries and graphs of the filtered housing data."
          ),
          
          selectInput(
            "summary_type",
            "Choose a Summary:",
            choices = c(
              "Categorical Summary",
              "Numerical Summary",
              "Graphical Summary"
            )
          ),
          
          conditionalPanel(
            condition = "input.summary_type == 'Categorical Summary'",
            
            selectInput(
              "cat_var1",
              "First Categorical Variable:",
              choices = c(
                "Property Type" = "Type",
                "Region" = "Regionname",
                "Method" = "Method"
              )
            ),
            
            selectInput(
              "cat_var2",
              "Second Categorical Variable:",
              choices = c(
                "None" = "None",
                "Property Type" = "Type",
                "Region" = "Regionname",
                "Method" = "Method"
              ),
              selected = "None"
            ),
            
            h3("Frequency Table"),
            
            tableOutput("categorical_table")
          ),
          
          conditionalPanel(
            condition = "input.summary_type == 'Numerical Summary'",
            
            selectInput(
              "summary_num",
              "Numerical Variable:",
              choices = c(
                "Price" = "Price",
                "Rooms" = "Rooms",
                "Distance" = "Distance",
                "Bedrooms" = "Bedroom2",
                "Bathrooms" = "Bathroom",
                "Car Spaces" = "Car",
                "Land Size" = "Landsize",
                "Building Area" = "BuildingArea",
                "Year Built" = "YearBuilt"
              )
            ),
            
            selectInput(
              "summary_group",
              "Group By:",
              choices = c(
                "Property Type" = "Type",
                "Region" = "Regionname",
                "Method" = "Method"
              )
            ),
            
            h3("Numerical Summary"),
            
            tableOutput("numerical_table")
          ),
          
          conditionalPanel(
            condition = "input.summary_type == 'Graphical Summary'",
            
            selectInput(
              "plot_type",
              "Choose a Graph:",
              choices = c(
                "Histogram",
                "Boxplot",
                "Scatterplot",
                "Bar Chart",
                "Faceted Histogram",
                "Heatmap"
              )
            ),
            
            conditionalPanel(
              condition = "input.plot_type != 'Bar Chart'",
              selectInput(
                "plot_num1",
                "First Numerical Variable:",
                choices = c(
                  "Price" = "Price",
                  "Rooms" = "Rooms",
                  "Distance" = "Distance",
                  "Bedrooms" = "Bedroom2",
                  "Bathrooms" = "Bathroom",
                  "Car Spaces" = "Car",
                  "Land Size" = "Landsize",
                  "Building Area" = "BuildingArea"
                )
              )
            ),
            
            conditionalPanel(
              condition = "input.plot_type == 'Scatterplot'",
              selectInput(
                "plot_num2",
                "Second Numerical Variable:",
                choices = c(
                  "Price" = "Price",
                  "Rooms" = "Rooms",
                  "Distance" = "Distance",
                  "Bedrooms" = "Bedroom2",
                  "Bathrooms" = "Bathroom",
                  "Car Spaces" = "Car",
                  "Land Size" = "Landsize",
                  "Building Area" = "BuildingArea"
                ),
                selected = "Distance"
              )
            ),
            
            conditionalPanel(
              condition = "input.plot_type != 'Histogram'",
              selectInput(
                "plot_cat",
                "Categorical Variable:",
                choices = c(
                  "Property Type" = "Type",
                  "Region" = "Regionname",
                  "Method" = "Method"
                )
              )
            ),
            
            br(),
            
            shinycssloaders::withSpinner(
              plotOutput("exploration_plot", height = "500px")
            )
          )
        )
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
  # Categorical summaries
  
  output$categorical_table <- renderTable({
    
    data <- filtered_data()
    
    req(input$cat_var1, input$cat_var2)
    
    if (input$cat_var2 == "None") {
      
      result <- as.data.frame(
        table(data[[input$cat_var1]])
      )
      
      names(result) <- c("Category", "Count")
      
    } else {
      
      result <- as.data.frame(
        table(
          data[[input$cat_var1]],
          data[[input$cat_var2]]
        )
      )
      
      names(result) <- c(
        input$cat_var1,
        input$cat_var2,
        "Count"
      )
    }
    
    result
    
  })
  
  
  # Numerical summaries
  
  output$numerical_table <- renderTable({
    
    data <- filtered_data()
    
    req(input$summary_num, input$summary_group)
    
    data |>
      group_by(.data[[input$summary_group]]) |>
      summarize(
        Count = sum(!is.na(.data[[input$summary_num]])),
        Mean = mean(.data[[input$summary_num]], na.rm = TRUE),
        Median = median(.data[[input$summary_num]], na.rm = TRUE),
        SD = sd(.data[[input$summary_num]], na.rm = TRUE),
        Minimum = min(.data[[input$summary_num]], na.rm = TRUE),
        Maximum = max(.data[[input$summary_num]], na.rm = TRUE),
        .groups = "drop"
      )
    
  }, digits = 2)
  
  
  # Graphical summaries
  
  output$exploration_plot <- renderPlot({
    
    data <- filtered_data()
    
    req(
      input$plot_type,
      input$plot_num1,
      input$plot_num2,
      input$plot_cat
    )
    
    validate(
      need(nrow(data) > 0, "No data matches the selected filters.")
    )
    
    if (input$plot_type == "Histogram") {
      
      ggplot(
        data,
        aes(x = .data[[input$plot_num1]])
      ) +
        geom_histogram(bins = 30, fill = "steelblue") +
        labs(
          title = paste("Distribution of", input$plot_num1),
          x = input$plot_num1,
          y = "Number of Properties"
        )
      
    } else if (input$plot_type == "Boxplot") {
      
      ggplot(
        data,
        aes(
          x = .data[[input$plot_cat]],
          y = .data[[input$plot_num1]],
          fill = .data[[input$plot_cat]]
        )
      ) +
        geom_boxplot() +
        labs(
          title = paste(input$plot_num1, "by", input$plot_cat),
          x = input$plot_cat,
          y = input$plot_num1,
          fill = input$plot_cat
        ) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
    } else if (input$plot_type == "Scatterplot") {
      
      ggplot(
        data,
        aes(
          x = .data[[input$plot_num2]],
          y = .data[[input$plot_num1]],
          color = .data[[input$plot_cat]]
        )
      ) +
        geom_point(alpha = 0.4) +
        labs(
          title = paste(input$plot_num1, "vs.", input$plot_num2),
          x = input$plot_num2,
          y = input$plot_num1,
          color = input$plot_cat
        )
      
    } else if (input$plot_type == "Bar Chart") {
      
      ggplot(
        data,
        aes(
          x = .data[[input$plot_cat]],
          fill = Type
        )
      ) +
        geom_bar(position = "dodge") +
        labs(
          title = paste("Property Types by", input$plot_cat),
          x = input$plot_cat,
          y = "Number of Properties",
          fill = "Property Type"
        ) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
    } else if (input$plot_type == "Faceted Histogram") {
      
      ggplot(
        data,
        aes(
          x = .data[[input$plot_num1]],
          fill = Type
        )
      ) +
        geom_histogram(bins = 30) +
        facet_wrap(
          vars(.data[[input$plot_cat]]),
          scales = "free_y"
        ) +
        labs(
          title = paste(
            "Distribution of",
            input$plot_num1,
            "by",
            input$plot_cat
          ),
          x = input$plot_num1,
          y = "Number of Properties",
          fill = "Property Type"
        )
      
    } else if (input$plot_type == "Heatmap") {
      
      heatmap_data <- data |>
        filter(
          !is.na(.data[[input$plot_num1]]),
          !is.na(.data[[input$plot_cat]]),
          !is.na(Type)
        ) |>
        group_by(
          .data[[input$plot_cat]],
          Type
        ) |>
        summarize(
          Average = mean(.data[[input$plot_num1]]),
          .groups = "drop"
        )
      
      ggplot(
        heatmap_data,
        aes(
          x = Type,
          y = .data[[input$plot_cat]],
          fill = Average
        )
      ) +
        geom_tile() +
        labs(
          title = paste(
            "Average",
            input$plot_num1,
            "by Property Type and",
            input$plot_cat
          ),
          x = "Property Type",
          y = input$plot_cat,
          fill = paste("Average", input$plot_num1)
        )
    }
    
  })
}

shinyApp(ui = ui, server = server)