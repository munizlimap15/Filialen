library(readxl)
library(shiny)
library(leaflet)
library(sf)
library(ggplot2)
library(plotly)
library(dplyr)
library(leaflet.extras)  # Load the leaflet.extras package for heatmap functionality
library(tidyr)
library(DT)
library(shinydashboard)


#setwd("C:/Users/pedro/Desktop/JO_bonus")
source("Geo_code.R")


ui <- dashboardPage(
  dashboardHeader(title = "Spatial Visualization of Store Metrics"),
  dashboardSidebar(
    selectInput(inputId = "selectedState", label = "Select a state:", choices = unique(states_sf$State), selected = "Vienna"),
    selectInput(inputId = "clusterSelect", 
                label = "Select Cluster:", 
                choices = c("Basic", "Bronze", "Silver", "Gold", "All"), 
                multiple = TRUE, 
                selected = c("Basic", "Bronze", "Silver", "Gold")),
    sliderInput("revenueSlider", "Revenue Range:", min = 0, max = 1, value = c(0, 1))
  ),
  dashboardBody(
    tabsetPanel(
      tabPanel("Revenue Metrics",
               fluidRow(
                 box(valueBoxOutput("totalRevenueBox"), width = 10),
                 box(valueBoxOutput("clusterARevenue"), width = 6), 
                 box(valueBoxOutput("clusterBRevenue"), width = 6),
                 box(valueBoxOutput("clusterCRevenue"), width = 6),
                 box(valueBoxOutput("clusterDRevenue"), width = 6) 
               ),
               fluidRow(
                 box(valueBoxOutput("totalRevenueAustriaBox"), width = 10),
                 box(valueBoxOutput("totalRevenueVienna"), width = 6),
                 box(valueBoxOutput("totalRevenueLowerAustria"), width = 6),
                 box(valueBoxOutput("totalRevenueStyria"), width = 6),
                 box(valueBoxOutput("totalRevenueSalzburg"), width = 6),
                 box(valueBoxOutput("totalRevenueUpperAustria"), width = 6),
                 box(valueBoxOutput("totalRevenueTyrol"), width = 6),
                 box(valueBoxOutput("totalRevenueCarinthia"), width = 6),
                 box(valueBoxOutput("totalRevenueVorarlberg"), width = 6),
                 box(valueBoxOutput("totalRevenueBurgenland"), width = 6)
               )
      ),
      tabPanel("Map", 
               leafletOutput("mymap", width = "100%", height = "700px"),
               fluidRow(
                 column(width = 8, offset = 2, 
                        plotlyOutput("stackedBarChart"))
               )
      ),
      
      tabPanel("Revenue relationships", 
               selectInput("selectVar", "Select a variable:", choices = names(merged_data)[4:25]),
               plotlyOutput("regPlot"),
               dataTableOutput("filialTable")
      )
    )
  )
)

















server <- function(input, output, session) {
  state_data <- reactive({
    req(input$selectedState)
    
    if (input$selectedState == "Austria") {
      data <- merged_data
    } else {
      data <- merged_data[merged_data$State == input$selectedState, ]
    }
    
    # Handle 'All' or specific cluster selections
    if ("All" %in% input$clusterSelect) {
      return(data)
    } else {
      return(data[data$Cluster %in% input$clusterSelect, ])
    }
  })%>% debounce(1000)
  
  observe({
    clusters_selected <- input$clusterSelect
    
    # If "All" is selected, choose all clusters
    if ("All" %in% clusters_selected && length(clusters_selected) == 1) {
      updateSelectInput(session, "clusterSelect", selected = c("Basic", "Bronze", "Silver", "Gold"))
    }
    
  })
  
  
  # Update sliderInput based on state-specific data
  observe({
    data <- req(state_data())
    min_val <- round(min(data$Revenue, na.rm = TRUE), -3)
    max_val <- round(max(data$Revenue, na.rm = TRUE), -3)
    
    # Define the step size for intervals 
    step_size <- (max_val - min_val) / 4
    
    updateSliderInput(session, "revenueSlider",
                      min = min_val,
                      max = max_val,
                      step = step_size,
                      value = c(min_val, max_val))
  })
    
  output$stackedBarChart <- renderPlotly({
    data <- req(state_data())
    
    shares_data <- data %>%
      group_by(State, Cluster) %>%
      summarise(
        AShare = round(mean(AShare, na.rm = TRUE), 2),
        BShare = round(mean(BShare, na.rm = TRUE), 2),
        CShare = round(mean(CShare, na.rm = TRUE), 2),
        DShare = round(mean(DShare, na.rm = TRUE), 2),
        EShare = round(mean(EShare, na.rm = TRUE), 2),
        FShare = round(mean(FShare, na.rm = TRUE), 2),
        GShare = round(mean(GShare, na.rm = TRUE), 2),
        NonAlcDrinkShare = round(mean(NonAlcDrinkShare, na.rm = TRUE), 2),
        AlcoholShare = round(mean(AlcoholShare, na.rm = TRUE), 2),
        TotalShare = round(AShare + BShare + CShare + DShare + EShare + FShare + GShare + NonAlcDrinkShare + AlcoholShare, 2),
        OthersShare = round(1 - TotalShare, 2),
        .groups = 'drop'
      ) %>%
      pivot_longer(
        cols = c(AShare , BShare , CShare , DShare , EShare , FShare , GShare , NonAlcDrinkShare, AlcoholShare, OthersShare),
        names_to = "Category",
        values_to = "Value"
      ) %>%
      filter(Cluster %in% input$clusterSelect | "All" %in% input$clusterSelect)
    
    p <- ggplot(shares_data, aes(x = State, y = Value, fill = Category)) +
      geom_bar(stat = "identity", position = "stack") +
      facet_wrap(~Cluster, nrow = 1) +  # Ensures all clusters are in one row
      scale_y_continuous(limits = c(0, 1)) +
      theme_minimal() +
      xlab("State") +
      ylab("Total Share") +
      labs(fill = "Category") +
      scale_fill_brewer(palette = "Paired")
    
    ggplotly(p)
  })
  
  
    
    
    
    output$mymap <- renderLeaflet({
      req(state_data())  # Ensure the subset is ready
      data <- state_data()
      
      # Filter data based on revenue slider
      filtered_data <- data %>%
        filter(Revenue >= input$revenueSlider[1] & Revenue <= input$revenueSlider[2])
      
      if(nrow(filtered_data) == 0) {
        return(NULL)
      }
      
      # Create popup info for each point using filtered data
      popup_info <- sapply(1:nrow(filtered_data), function(i) {
        paste("<strong>", names(filtered_data), ":</strong>", filtered_data[i, ], sep = "", collapse = "<br>")
      })
      
      
     
      leaflet(data = filtered_data,options = leafletOptions(minZoom = 8, maxZoom = 14)) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(
          data = states_sf[states_sf$State == input$selectedState, ],
          fillOpacity = 0, color = "black", weight = 3,
          label = ~State
        ) %>%
        addCircleMarkers(
          lng = ~st_coordinates(geometry)[, 1],
          lat = ~st_coordinates(geometry)[, 2],
          radius = ~NormRevenue,  # Adjust the divisor to scale appropriately
          color = "black",         # Color of the border
          fill = FALSE,            # Do not fill the circles
          fillOpacity = 0,         # No fill opacity since fill is FALSE
          popup = popup_info,      # Popup info
          group = "Revenue/Kund"   # Group name for layer control
        )      %>%
        addCircleMarkers(
          lng = ~st_coordinates(geometry)[, 1],
          lat = ~st_coordinates(geometry)[, 2],
          radius = ~NormRevenue/50,  # Adjust the divisor to scale appropriately
          color = "black",         # Color of the border
          fill = TRUE,            # Do not fill the circles
          fillOpacity = 1         # No fill opacity since fill is FALSE
          )      %>%
        addCircleMarkers(
          lng = ~st_coordinates(geometry)[, 1],
          lat = ~st_coordinates(geometry)[, 2],
          radius = ~Revenue / 40000,  # Adjust the divisor to scale appropriately
          color = "black",
          fill = FALSE,
          fillOpacity = 0,
          popup = popup_info,group = "Revenue"
        ) %>%
        leaflet.extras::addHeatmap(
          lng = ~st_coordinates(geometry)[, 1],
          lat = ~st_coordinates(geometry)[, 2],
          intensity = ~Revenue,  # For heatmap, adjust intensity as required
          radius = 20,           # Increased radius
          blur = 30,             # Increased blur
          max = 0.6,             # Adjusted max to a value less than 1, assuming Revenue is normalized between 0 and 1
          group = "Heatmap"
        ) %>%
        addLayersControl(
          baseGroups = c("Base Map"),
          overlayGroups = c("Heatmap", "Revenue", "Revenue/Kund"),#,
          options = layersControlOptions(collapsed = FALSE)
        )
    })
    
    
    
    output$histogram <- renderPlotly({
      data <- req(state_data())
      p <- ggplot(data, aes(x = Revenue)) +
        geom_histogram(bins = 30, fill = 'blue') +
        ggtitle("Revenue Distribution")
      ggplotly(p)
    })
    
    output$totalRevenueBox <- renderValueBox({
      state_name <- input$selectedState  # get the name of the selected state
      data <- req(state_data())
      sum_revenue <- sum(data$Revenue, na.rm = TRUE)
      
      # Use paste or paste0 to combine strings
      subtitle_text <- paste("Total Revenue in", state_name, "(€);", "N stores=", nrow(data))
      
      valueBox(
        value = formatC(sum_revenue, format = "f", big.mark = ",", digits = 2),
        subtitle = subtitle_text,
        icon = icon("euro"),
        color = "aqua"
      )
    })
    
    output$clusterARevenue <- renderValueBox({
      # Make sure the selection is reactive
      state_name <- input$selectedState
      
      # Filter data for Cluster A and the selected state
      clusterAData <- req(merged_data[merged_data$Cluster == "Basic" & merged_data$State == state_name, ])
      
      # Sum the revenue for Cluster A in the selected state
      totalRevenueClusterA <- sum(clusterAData$Revenue, na.rm = TRUE)
      
      # Create the subtitle text dynamically based on the selected state
      subtitle_text <- if(state_name == "Austria") {
        "Total Revenue in Cluster A (€)"
      } else {
        paste("Total Revenue in Cluster A for", state_name, "(€);", "N stores=", nrow(clusterAData))
      }
      
      # Render the value box with the calculated revenue and subtitle
      valueBox(
        value = formatC(totalRevenueClusterA, format = "f", big.mark = ",", digits = 2),
        subtitle = subtitle_text,
        icon = icon("euro"),
        color = "green"
      )
    })
    
    output$clusterBRevenue <- renderValueBox({
      # Make sure the selection is reactive
      state_name <- input$selectedState
      
      # Filter data for Cluster A and the selected state
      clusterAData <- req(merged_data[merged_data$Cluster == "Bronze" & merged_data$State == state_name, ])
      
      # Sum the revenue for Cluster A in the selected state
      totalRevenueClusterA <- sum(clusterAData$Revenue, na.rm = TRUE)
      
      # Create the subtitle text dynamically based on the selected state
      subtitle_text <- if(state_name == "Austria") {
        "Total Revenue in Cluster B (€)"
      } else {
        paste("Total Revenue in Cluster B for", state_name, "(€);", "N stores=", nrow(clusterAData))
      }
      
      # Render the value box with the calculated revenue and subtitle
      valueBox(
        value = formatC(totalRevenueClusterA, format = "f", big.mark = ",", digits = 2),
        subtitle = subtitle_text,
        icon = icon("euro"),
        color = "green"
      )
    })
    
    output$clusterCRevenue <- renderValueBox({
      # Make sure the selection is reactive
      state_name <- input$selectedState
      
      # Filter data for Cluster A and the selected state
      clusterAData <- req(merged_data[merged_data$Cluster == "Silver" & merged_data$State == state_name, ])
      
      # Sum the revenue for Cluster A in the selected state
      totalRevenueClusterA <- sum(clusterAData$Revenue, na.rm = TRUE)
      
      # Create the subtitle text dynamically based on the selected state
      subtitle_text <- if(state_name == "Austria") {
        "Total Revenue in Cluster C (€)"
      } else {
        paste("Total Revenue in Cluster C for", state_name, "(€);", "N stores=", nrow(clusterAData))
      }
      
      # Render the value box with the calculated revenue and subtitle
      valueBox(
        value = formatC(totalRevenueClusterA, format = "f", big.mark = ",", digits = 2),
        subtitle = subtitle_text,
        icon = icon("euro"),
        color = "green"
      )
    })
    
    output$clusterDRevenue <- renderValueBox({
      # Make sure the selection is reactive
      state_name <- input$selectedState
      
      # Filter data for Cluster A and the selected state
      clusterAData <- req(merged_data[merged_data$Cluster == "Gold" & merged_data$State == state_name, ])
      
      # Sum the revenue for Cluster A in the selected state
      totalRevenueClusterA <- sum(clusterAData$Revenue, na.rm = TRUE)
      
      # Create the subtitle text dynamically based on the selected state
      subtitle_text <- if(state_name == "Austria") {
        "Total Revenue in Cluster D (€)"
      } else {
        paste("Total Revenue in Cluster D for", state_name, "(€);", "N stores=", nrow(clusterAData))
      }
      
      # Render the value box with the calculated revenue and subtitle
      valueBox(
        value = formatC(totalRevenueClusterA, format = "f", big.mark = ",", digits = 2),
        subtitle = subtitle_text,
        icon = icon("euro"),
        color = "green"
      )
    })
    
    
    output$totalRevenueAustriaBox <- renderValueBox({
      # Calculate the sum of revenue for all states in Austria
      sum_revenue_austria <- merged_data %>%
        filter(!State %in% "Austria") %>%
        summarise(TotalRevenue = sum(Revenue, na.rm = TRUE)) %>%
        .[["TotalRevenue"]] # Extract the sum
      
      valueBox(
        value = formatC(sum_revenue_austria, format = "f", big.mark = ",", digits = 2),
        #subtitle = "Total Revenue in Austria (€)",
        subtitle =paste("Total Revenue in Austria (€)", "N stores=", nrow(merged_data)),
        icon = icon("euro"),
        color = "blue"
      )
    })
    
    
    
    
    output$totalRevenueLowerAustria <- renderValueBox({
      state_data <- req(merged_data[merged_data$State == "Lower Austria", ])
      totalRevenue <- sum(state_data$Revenue, na.rm = TRUE)
      valueBox(
        value = formatC(totalRevenue, format = "f", big.mark = ",", digits = 2),
        #subtitle = "Total Revenue in Lower Austria (€)",
        subtitle =paste("Total Revenue in Lower Austria (€)", "N stores=", nrow(state_data)),
        icon = icon("euro"),
        color = "blue"
      )
    })
    
    output$totalRevenueUpperAustria <- renderValueBox({
      state_data <- req(merged_data[merged_data$State == "Upper Austria", ])
      totalRevenue <- sum(state_data$Revenue, na.rm = TRUE)
      valueBox(
        value = formatC(totalRevenue, format = "f", big.mark = ",", digits = 2),
        #subtitle = "Total Revenue in Upper Austria (€)",
        subtitle =paste("Total Revenue in Upper Austria (€)", "N stores=", nrow(state_data)),
        icon = icon("euro"),
        color = "orange"
      )
    })
    
    output$totalRevenueSalzburg <- renderValueBox({
      state_data <- req(merged_data[merged_data$State == "Salzburg", ])
      totalRevenue <- sum(state_data$Revenue, na.rm = TRUE)
      valueBox(
        value = formatC(totalRevenue, format = "f", big.mark = ",", digits = 2),
        #subtitle = "Total Revenue in Salzburg (€)",
        subtitle =paste("Total Revenue in Salzburg (€)", "N stores=", nrow(state_data)),
        icon = icon("euro"),
        color = "red"
      )
    })
    
    output$totalRevenueTyrol <- renderValueBox({
      state_data <- req(merged_data[merged_data$State == "Tyrol", ])
      totalRevenue <- sum(state_data$Revenue, na.rm = TRUE)
      valueBox(
        value = formatC(totalRevenue, format = "f", big.mark = ",", digits = 2),
        #subtitle = "Total Revenue in Tyrol (€)",
        subtitle =paste("Total Revenue in Tyrol (€)", "N stores=", nrow(state_data)),
        icon = icon("euro"),
        color = "purple"
      )
    })
    
    output$totalRevenueCarinthia <- renderValueBox({
      state_data <- req(merged_data[merged_data$State == "Carinthia", ])
      totalRevenue <- sum(state_data$Revenue, na.rm = TRUE)
      valueBox(
        value = formatC(totalRevenue, format = "f", big.mark = ",", digits = 2),
        #subtitle = "Total Revenue in Carinthia (€)",
        subtitle =paste("Total Revenue in Carinthia (€)", "N stores=", nrow(state_data)),
        icon = icon("euro"),
        color = "yellow"
      )
    })
    
    output$totalRevenueVorarlberg <- renderValueBox({
      state_data <- req(merged_data[merged_data$State == "Vorarlberg", ])
      totalRevenue <- sum(state_data$Revenue, na.rm = TRUE)
      valueBox(
        value = formatC(totalRevenue, format = "f", big.mark = ",", digits = 2),
        #subtitle = "Total Revenue in Vorarlberg (€)",
        subtitle =paste("Total Revenue in Vorarlberg (€)", "N stores=", nrow(state_data)),
        icon = icon("euro"),
        color = "green"
      )
    })
    
    output$totalRevenueStyria <- renderValueBox({
      state_data <- req(merged_data[merged_data$State == "Styria", ])
      totalRevenue <- sum(state_data$Revenue, na.rm = TRUE)
      valueBox(
        value = formatC(totalRevenue, format = "f", big.mark = ",", digits = 2),
        #subtitle = "Total Revenue in Styria (€)",
        subtitle =paste("Total Revenue in Styria (€)", "N stores=", nrow(state_data)),
        icon = icon("euro"),
        color = "teal"
      )
    })
    
    output$totalRevenueBurgenland <- renderValueBox({
      state_data <- req(merged_data[merged_data$State == "Burgenland", ])
      totalRevenue <- sum(state_data$Revenue, na.rm = TRUE)
      valueBox(
        value = formatC(totalRevenue, format = "f", big.mark = ",", digits = 2),
        subtitle =paste("Total Revenue in Burgenland (€)", "N stores=", nrow(state_data)),
        #subtitle = "Total Revenue in Burgenland (€)",
        icon = icon("euro"),
        color = "maroon"
      )
    })
    
    output$totalRevenueVienna <- renderValueBox({
      state_data <- req(merged_data[merged_data$State == "Vienna", ])
      totalRevenue <- sum(state_data$Revenue, na.rm = TRUE)
      valueBox(
        value = formatC(totalRevenue, format = "f", big.mark = ",", digits = 2),
        #subtitle = "Total Revenue in Vienna (€)",
        subtitle =paste("Total Revenue in Vienna (€)", "N stores=", nrow(state_data)),
        icon = icon("euro"),
        color = "navy"
      )
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    output$regPlot <- renderPlotly({
      req(input$selectVar)
      data <- req(state_data())
      lm_model <- lm(Revenue ~ get(input$selectVar), data = data)
      
      # Use the model to make predictions
      data$predicted <- predict(lm_model, data)
      
      # Construct the equation string
      intercept <- round(coef(lm_model)[1], 2)
      slope <- round(coef(lm_model)[2], 2)
      eq_label <- paste("Revenue = ", intercept, " + ", slope, " * ", input$selectVar, sep = "")
      
      # Create the plot
      p <- ggplot(data, aes_string(x = input$selectVar, y = "Revenue", color = "Cluster")) +
        geom_point() +
        geom_line(aes_string(y = "predicted"), color = 'blue') +
        labs(x = input$selectVar, y = "Revenue", title = "Revenue vs Selected Variable") +
        annotate("text", x = Inf, y = Inf, label = eq_label, parse = TRUE,
                 hjust = 1.1, vjust = 1.1, size = 4, color = "blue")  # Add the equation annotation
      
      # Convert to ggplotly
      ggplotly(p)
    })
    
    output$filialTable <- renderDT({
      # First, obtain the actual data from the reactive expression
      data <- req(state_data())
      
      # Now, use 'select()' to choose the columns
      selected_data <- data %>% 
        select(-geometry, -Rnk, -B_LAND)  # Adjust this line as needed to exclude or include specific columns
      
      # Use DT::datatable() to enhance table features
      datatable(
        selected_data,
        extensions = 'Responsive',  # Enables the Responsive extension
        options = list(
          autoWidth = TRUE,
          scrollX = TRUE,
          responsive = TRUE,  # The table will be responsive
          pageLength = 25,    # Set the number of rows per page
          dom = 'Bfrtip',     # Enable buttons and other features
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        ),
        class = 'display nowrap'  # Ensures that text in each column does not wrap
      )
    })
    
    
  
    

}

shinyApp(ui, server)
