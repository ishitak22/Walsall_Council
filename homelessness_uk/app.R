library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

# Load and process the dataset
data <- read_excel("PE1_2009-2018.xlsx")

data[grep("^20", names(data))] <- lapply(data[grep("^20", names(data))], as.character)

melted_data <- data %>%
  pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Homelessness Decisions") %>%
  mutate(
    Year = as.numeric(substr(Year, 1, 4)),
    `Homelessness Decisions` = as.numeric(`Homelessness Decisions`)
  ) %>%
  replace_na(list(`Homelessness Decisions` = NA))

# Create the Shiny app
ui <- fluidPage(
  titlePanel("Homelessness Decisions by Local Authority"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("authorityInput", "Choose a local authority:", choices = unique(melted_data$`Local authority area`)),
      downloadButton("downloadPlot", "Download Plot")
    ),
    
    mainPanel(
      plotOutput("authorityPlot")
    )
  )
)

server <- function(input, output) {
  plot_specific_authority <- function(authority) {
    authority_data <- melted_data %>% filter(`Local authority area` == authority)
    
    plot <- ggplot(authority_data, aes(x = Year, y = `Homelessness Decisions`)) +
      geom_line() + 
      geom_point() +
      ggtitle(paste("Homelessness Decisions in", authority)) +
      xlab("Year") +
      ylab("Number of Homelessness Decisions") +
      theme_minimal()
    
    return(plot)
  }
  
  output$authorityPlot <- renderPlot({
    plot_specific_authority(input$authorityInput)
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste(input$authorityInput, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot_specific_authority(input$authorityInput))
    }
  )
}

shinyApp(ui = ui, server = server)
