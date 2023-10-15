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

# Create the Shiny app UI
ui <- navbarPage("Homelessness Decisions by Local Authority",
                 id = "navBar",
                 
                 # Introduction Tab
                 tabPanel("Introduction", 
                          fluidPage(
                            h1("Homelessness in the UK"),
                            tags$p("Homelessness is a pressing issue in the UK, encompassing a range of situations from rough sleeping to living in temporary accommodations. Over the years, the number of people experiencing homelessness has seen fluctuations, influenced by various economic, social, and political factors."),
                            tags$ul(
                              tags$li(tags$b("Rough Sleeping:"), "This is the most visible form of homelessness, referring to people who sleep on the streets, in doorways, parks, or other places not meant for habitation."),
                              tags$li(tags$b("Temporary Accommodation:"), "This includes individuals or families living in hostels, B&Bs, or other short-term housing solutions provided by local councils or charities."),
                              tags$li(tags$b("Hidden Homelessness:"), "Refers to people who don’t have a place of their own and stay with friends or family, often moving from one place to another.")
                            ),
                            tags$p("Causes:"),
                            tags$ul(
                              tags$li(tags$b("Economic Factors:"), "Unemployment, poverty, and lack of affordable housing are significant contributors."),
                              tags$li(tags$b("Social Factors:"), "Relationship breakdowns, domestic abuse, and health issues, especially mental health, can lead to homelessness."),
                              tags$li(tags$b("Systemic Issues:"), "Gaps in social security benefits, lack of social housing, and other systemic challenges play a role.")
                            ),
                            tags$p("Efforts to Address the Issue:"),
                            tags$ul(
                              tags$li("Various charities, NGOs, and local councils work tirelessly to provide shelter, food, and support to those affected."),
                              tags$li("Government initiatives aim to reduce and prevent homelessness, but challenges remain.")
                            ),
                            tags$p("Understanding the trends and patterns of homelessness is crucial for policymakers, social workers, and charities to make informed decisions and bring about positive change."),
                            
                            # Action button to navigate to the visualization
                            actionButton("goto_visualization", "Go to Data Visualization")
                          )
                 ),
                 
                 # Data Visualization Tab
                 tabPanel("Data Visualization",
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
)

# Server function
server <- function(input, output, session) {
  
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
  
  # When the button is clicked, switch to the Data Visualization tab
  observeEvent(input$goto_visualization, {
    updateNavbarPage(session, "navBar", selected = "Data Visualization")
  })
}

shinyApp(ui = ui, server = server)
