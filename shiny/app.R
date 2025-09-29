library(shiny)
library(bslib)
library(ggplot2)
library(tidyverse)
library(here)
library(leaflet)
library(plotly)
library(shinyscreenshot)

# Load data
data <- readRDS(here("shiny", "data", "data_for_shiny.rds"))
fec <- readRDS(here("shiny", "data", "weight_adjusted_fecundity.rds"))
rat <- readRDS(here("shiny", "data", "rat.rds"))
rep <- readRDS(here("shiny", "data", "rep.rds"))
temp <-readRDS(here("shiny", "data", "temp.rds"))
sal <-readRDS(here("shiny", "data", "sal.rds"))
legend <- readRDS(here("shiny", "data", "data_for_scale_manual.rds"))



# Define UI
ui <- fluidPage(
  titlePanel("PMAR Wild Oyster Survey"),
  sidebarLayout(
    sidebarPanel(
      # Dropdown to select data type
      selectInput("data_type", "Select Data Type:",
                  choices = c("Fecundity of Females", 
                              "Sperm Rating", 
                              "Reproductively Active",
                              "Temperature",
                              "Salinity"), # Replace with your actual data types
                  selected = "Fecundity of Females"),  # Default selection
      
      # Checkbox group for selecting bays
      checkboxGroupInput("selected_bays", "Select Bays:", 
                         choices = unique(data$bay_full_name), 
                         selected = data %>% 
                           filter(bay %in% c("GAL","LLM")) %>%
                           pull(bay_full_name)),
      # Dynamic selection for sites based on selected bays
      uiOutput("site_selection"),  # Placeholder for dynamic site selection
    
      ), #end of sidebarPanel
    
    mainPanel(
      plotOutput("linePlot"),  # Space for the plot
      leafletOutput("map")      # Space for the map
) # end of mainpanel
) #end of sidebarLayout
) #end of fluidpage
# Define server logic
server <- function(input, output, session) {
  # Reactive expression to filter data 
  filteredData <- reactive({
    req(input$selected_bays)
    
    if (input$data_type == "Fecundity of Females") {
      fec %>% filter(bay_full_name %in% input$selected_bays)
    } else if (input$data_type == "Sperm Rating") {
      rat %>% filter(bay_full_name %in% input$selected_bays)
    } else if (input$data_type == "Reproductively Active") {
      rep %>% filter(bay_full_name %in% input$selected_bays)
    } else if (input$data_type == "Temperature") {
      temp %>% filter(bay_full_name %in% input$selected_bays)
    } else if (input$data_type == "Salinity") {
      sal %>% filter(bay_full_name %in% input$selected_bays)
    }
  })
  
  # Dynamic site selection
  output$site_selection <- renderUI({
    req(input$selected_bays)
    
    # Get sites corresponding to the selected bays
    available_sites <- filteredData() %>%
      arrange(desc(latitude)) %>%
      pull(site_full_name) %>%
      unique()
    
    
    # Create checkbox group for site selection
    checkboxGroupInput("selected_sites", "Select Sites:", 
                       choices = available_sites, 
                       selected = available_sites)  # Default to all available sites
  })
  
  # Render the plot based on selected data type and sites
  output$linePlot <- renderPlot({
    req(input$selected_sites)  # Ensure sites are selected
    data_to_plot <- filteredData() %>% 
      filter(site_full_name %in% input$selected_sites)
    
    # Create a unique list of keys and their corresponding colors and shapes based on the data
    unique_keys <- unique(data_to_plot$key)
    filtered_legend <- legend %>% filter(key %in% unique_keys)
    
    # Convert key to a factor with levels in the desired order
    data_to_plot$key <- factor(data_to_plot$key, levels = filtered_legend$key)
    
    if (input$data_type == "Fecundity of Females") {
      ggplot(data_to_plot, aes(x = date , y = md, 
                      fill= key,
                      shape = key,
                      color = key)) +
        geom_point(size = 5, stroke = 1, color = "black") + 
        geom_line(linewidth = 1) +
        scale_color_manual(values = filtered_legend$color, guide = "none") +
        scale_fill_manual(name = "Sites",
                          labels = filtered_legend$key,
                          values = filtered_legend$color) + 
        scale_shape_manual(name = "Sites",
                           labels = filtered_legend$key,
                           values = filtered_legend$shape) +
        scale_y_continuous(limits = c(0,1000000),
                           breaks = seq(0, 1000000, by = 100000),     
                           labels = scales::label_number(scale = 1e-3, suffix = "K"))+ 
        scale_x_date(
          limits = as.Date(c("2023-11-01", "2024-10-31")),
          date_breaks = "1 month",              
          date_labels = "%b"                        
        ) + 
        ylab("Mean Eggs Per Gram of Tissue") +
        xlab("Date") +
        theme_minimal()
      
    } else if (input$data_type == "Sperm Rating") {
      ggplot(data_to_plot, aes(x = date , y = rating, 
                               fill= key,
                               shape = key,
                               color = key)) +
        geom_point(size = 5, stroke = 1, color = "black") + 
        geom_line(linewidth = 1) +
        scale_color_manual(values = filtered_legend$color, guide = "none") +
        scale_fill_manual(name = "Sites",
                          labels = filtered_legend$key,
                          values = filtered_legend$color) + 
        scale_shape_manual(name = "Sites",
                           labels = filtered_legend$key,
                           values = filtered_legend$shape) +
        scale_y_continuous(limits = c(0,5),
                           breaks = seq(0, 5, by = 1))+
        scale_x_date(
          limits = as.Date(c("2023-11-01", "2024-08-31")),
          date_breaks = "1 month",              
          date_labels = "%b"                        
        ) + 
        ylab("Mean Sperm Rating") +
        xlab("Date") +
        theme_minimal()
      
    } else if (input$data_type == "Reproductively Active") {
      ggplot(data_to_plot, aes(x = date , y = active, 
                               fill= key,
                               shape = key,
                               color = key)) +
        geom_point(size = 5, stroke = 1, color = "black") + 
        geom_line(linewidth = 1) +
        scale_color_manual(values = filtered_legend$color, guide = "none") +
        scale_fill_manual(name = "Sites",
                          labels = filtered_legend$key,
                          values = filtered_legend$color) + 
        scale_shape_manual(name = "Sites",
                           labels = filtered_legend$key,
                           values = filtered_legend$shape) +
        scale_y_continuous(limits = c(0,1),
                           breaks = seq(0, 1, by = 0.2))+
        scale_x_date(
          limits = as.Date(c("2023-11-01", "2024-08-31")),
          date_breaks = "1 month",              
          date_labels = "%b"                        
        ) + 
        ylab("Proportion Reproductively Active") +
        xlab("Date") +
        theme_minimal()
      
    } else if (input$data_type == "Temperature") {
      ggplot(data_to_plot, aes(x = date , y = m_temp, 
                               fill= key,
                               shape = key,
                               color = key)) +
        geom_point(size = 5, stroke = 1, color = "black") + 
        geom_line(linewidth = 1) +
        scale_color_manual(values = filtered_legend$color, guide = "none") +
        scale_fill_manual(name = "Sites",
                          labels = filtered_legend$key,
                          values = filtered_legend$color) + 
        scale_shape_manual(name = "Sites",
                           labels = filtered_legend$key,
                           values = filtered_legend$shape) +
        scale_y_continuous(limits = c(0,35),
                           breaks = seq(0, 35, by = 10))+
        scale_x_date(
          limits = as.Date(c("2023-11-01", "2024-08-31")),
          date_breaks = "1 month",              
          date_labels = "%b"                        
        ) + 
        ylab("Temperature in Celcius") +
        xlab("Date") +
        theme_minimal()
      
    } else if (input$data_type == "Salinity") {
      ggplot(data_to_plot, aes(x = date , y = m_sal, 
                               fill= key,
                               shape = key,
                               color = key)) +
        geom_point(size = 5, stroke = 1, color = "black") + 
        geom_line(linewidth = 1) +
        scale_color_manual(values = filtered_legend$color, guide = "none") +
        scale_fill_manual(name = "Sites",
                          labels = filtered_legend$key,
                          values = filtered_legend$color) + 
        scale_shape_manual(name = "Sites",
                           labels = filtered_legend$key,
                           values = filtered_legend$shape) +
        scale_y_continuous(limits = c(0,55),
                           breaks = seq(0, 55, by = 10))+
        scale_x_date(
          limits = as.Date(c("2023-11-01", "2024-08-31")),
          date_breaks = "1 month",              
          date_labels = "%b"                        
        ) + 
        ylab("Salinity (ppt)") +
        xlab("Date") +
        theme_minimal()
    
    }
    
  })
  
  output$map <- renderLeaflet({
    req(input$selected_sites)  # Ensure sites are selected
    site_data <- data %>% filter(site_full_name %in% input$selected_sites)
    
    leaflet(site_data) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      addCircleMarkers(lng = ~longitude, 
                       lat = ~latitude, 
                       popup = ~site_full_name,
                       fillColor = ~color,
                       color = "black",
                       weight = 0.5,
                       radius = 5) %>%
      fitBounds(lng1 = min(data$longitude),
                lat1 = min(data$latitude),
                lng2 = max(data$longitude),
                lat2 = max(data$latitude))
  }) 
  
}
# Run the application 
shinyApp(ui = ui, server = server)
