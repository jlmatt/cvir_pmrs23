library(shiny)
library(bslib)
library(ggplot2)
library(tidyverse)
library(here)

# Load data
data <- readRDS(here("shiny", "data", "data_for_shiny.rds"))
fec <- readRDS(here("shiny", "data", "weight_adjusted_fecundity.rds"))
fem <- readRDS(here("shiny", "data", "number_of_females.rds"))
legend <- readRDS(here("shiny", "data", "data_for_scale_manual.rds"))


# Define UI
ui <- fluidPage(
  titlePanel("PMAR"),
  sidebarLayout(
    sidebarPanel(
      # Dropdown to select data type
      selectInput("data_type", "Select Data Type:",
                  choices = c("Fecundity", "Females", "Males"), # Replace with your actual data types
                  selected = "Fecundity"),  # Default selection
      
      # Checkbox group for selecting bays
      checkboxGroupInput("selected_bays", "Select Bays:", 
                         choices = unique(data$bay_full_name), 
                         selected = unique(data$bay_full_name)),
      # Dynamic selection for sites based on selected bays
      uiOutput("site_selection")  # Placeholder for dynamic site selection
    ),
    
    mainPanel(
      plotOutput("linePlot")  # Space for the plot
    )
  )
)

# Define server logic
# Define server logic
server <- function(input, output, session) {
  # Reactive expression to filter data 
  filteredData <- reactive({
    req(input$selected_bays)
    
    if (input$data_type == "Fecundity") {
      fec %>% filter(bay_full_name %in% input$selected_bays)
    } else if (input$data_type == "Females") {
      fem %>% filter(bay_full_name %in% input$selected_bays)
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
    
    if (input$data_type == "Fecundity") {
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
          limits = as.Date(c("2023-11-01", "2024-07-31")),
          date_breaks = "1 month",              
          date_labels = "%b"                        
        ) + 
        ylab("Eggs Per Grams of Tissue") +
        theme_minimal()
      
    } else if (input$data_type == "Females") {
      ggplot(data_to_plot, aes(x = date , y = n, 
                      fill= fct_reorder(key,latitude, .desc = TRUE),
                      shape = fct_reorder(key,latitude, .desc = TRUE),
                      color = fct_reorder(bay,latitude, .desc = TRUE))) +
        geom_point(size = 5, stroke = 1, color = "black") + 
        geom_line(linewidth = 1) +
        scale_color_manual(values = legend$color,guide = "none") +
        scale_fill_manual(name = "Sites",
                          labels = legend$key,
                          values = legend$color) + 
        scale_shape_manual(name = "Sites",
                           labels = legend$key,
                           values = legend$shape) +
        theme_minimal()
    }
    
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
