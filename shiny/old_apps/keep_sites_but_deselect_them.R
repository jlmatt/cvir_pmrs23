# THIS CODE CAN BE PUT IN APP TO ENSURE SITES STAY IN THE SITE SELECTION CHECKBOX BUT BECOME DE-SELECTED


# Define server logic
server <- function(input, output, session) {
  # Reactive expression to filter data 
  filteredData <- reactive({
    req(input$selected_bays)
    
    if (input$data_type == "Type 1") {
      fec %>% filter(bay_full_name %in% input$selected_bays)
    } else if (input$data_type == "Type 2") {
      fem %>% filter(bay_full_name %in% input$selected_bays)
    }
  })
  
  # Keep track of all available sites based on the original data
  all_sites <- reactive({
    if (input$data_type == "Type 1") {
      unique(fec$site_full_name)
    } else if (input$data_type == "Type 2") {
      unique(fem$site_full_name)
    } else {
      NULL
    }
  })
  
  # Reactive value to store the available sites based on selected bays
  available_sites <- reactive({
    req(input$selected_bays)
    
    if (input$data_type == "Type 1") {
      unique(fec$site_full_name[fec$bay_full_name %in% input$selected_bays])
    } else if (input$data_type == "Type 2") {
      unique(fem$site_full_name[fem$bay_full_name %in% input$selected_bays])
    } else {
      NULL
    }
  })
  
  # Reactive value to store selected sites
  selected_sites <- reactiveVal(NULL)
  
  # Dynamic site selection UI
  output$site_selection <- renderUI({
    req(all_sites())
    
    # Get the currently available sites based on selected bays
    current_available_sites <- available_sites()
    
    # If no sites have been selected yet, default to sites available from the currently selected bays
    current_selection <- selected_sites()
    if (is.null(current_selection)) {
      current_selection <- current_available_sites
    } else {
      # Ensure current selection is valid based on available sites
      current_selection <- intersect(current_selection, all_sites())
    }
    
    checkboxGroupInput("selected_sites", "Select Sites:", 
                       choices = all_sites(),  # Use all available sites here
                       selected = intersect(current_selection, current_available_sites))  # Pre-select valid options
  })
  
  # Observe changes in the selected sites and update the reactive value
  observeEvent(input$selected_sites, {
    selected_sites(input$selected_sites)  # Update the reactive value
  })
  
  # Render the plot based on selected data type and sites
  output$linePlot <- renderPlot({
    req(input$selected_sites)
    data_to_plot <- filteredData() %>% 
      filter(site_full_name %in% input$selected_sites)  # Filter by selected sites
    
    # Create a unique list of keys and their corresponding colors and shapes based on the data
    unique_keys <- unique(data_to_plot$key)
    filtered_legend <- legend %>% filter(key %in% unique_keys)
    
    # Convert key to a factor with levels in the desired order
    data_to_plot$key <- factor(data_to_plot$key, levels = filtered_legend$key)
    
    if (input$data_type == "Type 1") {
      ggplot(data_to_plot, aes(x = date, y = md, 
                               fill = key,
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
        scale_y_continuous(limits = c(0, 1000000),
                           breaks = seq(0, 1000000, by = 100000),     
                           labels = function(x) paste0(x / 1000, "K")) + 
        scale_x_date(
          limits = as.Date(c("2023-11-01", "2024-07-31")),
          date_breaks = "1 month",              
          date_labels = "%b"                        
        ) + 
        ylab("Eggs Per Grams of Tissue") +
        theme_minimal()
      
    } else if (input$data_type == "Type 2") {
      ggplot(data_to_plot, aes(x = date, y = n, 
                               fill = fct_reorder(key, latitude, .desc = TRUE),
                               shape = fct_reorder(key, latitude, .desc = TRUE),
                               color = fct_reorder(bay, latitude, .desc = TRUE))) +
        geom_point(size = 5, stroke = 1, color = "black") + 
        geom_line(linewidth = 1) +
        scale_color_manual(values = legend$color, guide = "none") +
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
