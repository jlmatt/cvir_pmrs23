library(shiny)
library(bslib)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(here)

data <- readRDS(here::here("shiny","data","clean_data.rds"))

data <- data %>%
  filter(!tube_id == "JAN_UVB_14")

fec <- data %>%
  filter(sex =="F") %>%
  group_by(sample_site,latitude,date,bay) %>%
  summarise(md=median(wt_adj_fec,na.rm = TRUE)) %>%
  mutate(latitude = as.numeric(latitude)) %>%
  ungroup()

# Define UI
ui <- fluidPage(
  titlePanel("Dynamic Line Graph"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("selected_bays", "Select Bays:", 
                         choices = unique(fec$bay), 
                         selected = unique(fec$bay))
    ),
    mainPanel(
      plotOutput("linePlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  filteredData <- reactive({
    req(input$selected_bays) # Ensure input is available
    fec %>% filter(bay %in% input$selected_bays)
  })
  
  output$linePlot <- renderPlot({
    data_to_plot <- filteredData()
    
    # Find the axis limits from the filtered data
    xlim <- range(fec$date)
    ylim <- range(fec$md)
    
    ggplot(data_to_plot, aes(x = date, y = md, 
                             color = fct_reorder(bay, latitude, .desc = TRUE),
                             shape = fct_reorder(sample_site, latitude, .desc = TRUE))) +
      geom_point(size = 5) +
      geom_line() + 
      scale_color_manual(values = bay_colors) +
      labs(color = "Bay", shape = "Site") + 
      scale_shape_manual(values = seq(0, 14)) + 
      theme(panel.background = element_rect(fill = "gray70"),  
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "gray")) +
      scale_x_date(limits = xlim) +
      scale_y_continuous(limits = ylim)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
