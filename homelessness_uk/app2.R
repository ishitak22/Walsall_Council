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

# Load Data
la_total <- read_excel("detailed_LA_23_total.xlsx", sheet = 1)
traditional_regions <- c("North East", "North West", "Yorkshire and the Humber", "East Midlands", 
                         "West Midlands", "East of England", "London", "South East", "South West")
la_total_geo <- la_total[la_total$Area %in% traditional_regions, ]

# Load Data for Support Needs Analysis
la_support <- read_excel("detailed_LA_23_total.xlsx", sheet = 2)
geo_regions_support_needs <- la_support[la_support$Area %in% traditional_regions, ]

la_age <- read_excel("excel_datasets/detailed_LA_23_total.xlsx", sheet = 3)
geo_regions_age_duty <- la_age[la_age$Area %in% traditional_regions, ]

# Create the Shiny app UI
ui <- navbarPage("Homelessness Decisions by Local Authority",
                 id = "navBar",
                 
                 # Adding custom styles
                 tags$head(
                   tags$link(href = "https://fonts.googleapis.com/css2?family=Open+Sans:wght@400;700&display=swap", rel = "stylesheet"),
                   tags$style(HTML("
                     body {
                       font-family: 'Open Sans', sans-serif;
                       background-color: #eaeaea;
                     }
                     .navbar {
                       background-color: #34495e;
                     }
                     .navbar-default .navbar-nav > .active > a,
                     .navbar-default .navbar-nav > li > a:hover {
                       background-color: #5f2c82;
                       color: white;
                     }
                     .btn-primary {
                       background-color: #5f2c82;
                       border-color: #5f2c82;
                     }
                     .btn-primary:hover {
                       background-color: #3e8e9d;
                       border-color: #3e8e9d;
                     }
                     .sidebar {
                       background-color: #ffffff;
                       padding: 20px;
                       border-radius: 5px;
                     }
                     .tab-content > .active {
                       padding: 20px;
                       background: linear-gradient(to right, #5f2c82, #3e8e9d);
                       color: white;
                       border-radius: 15px;
                       box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);
                     }
                   "))
                 ),
                 
                 # Introduction Tab
                 tabPanel("Introduction", 
                          fluidPage(class = "intro-bg",
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
                 ),
                 
                 # Local Authorities Analysis
                 tabPanel("Local Authorities Analysis",
                          fluidPage(
                            h1("Trend Analysis and Findings of Particular Local Authorities"),
                            tags$br(),
                            fluidRow(
                              column(6,
                                     tags$h2("1. Walsall:"),
                                     tags$ul(
                                       tags$li("- Initial Sharp Decline: Rapid improvements in conditions, such as effective housing policies, job opportunities, or social welfare programs, could have caused this decline."),
                                       tags$li("- Stable Period with Minor Fluctuations: Homelessness levels remainedrelatively unchanged, suggesting steady conditions."),
                                       tags$li("- Gradual Increase: Slowly worsening economic conditions, rising housing costs, or decreasing effectiveness of social programs might be factors.")
                                     )
                              ),
                              column(6,
                                     plotOutput("plotWalsall")
                              )
                            ),
                            fluidRow(
                              column(6,
                                     tags$h2("2. Dudley:"),
                                     tags$ul(
                                       tags$li("- Initial decrease: In Dudley, there was a sudden downfall and then it was very stable and as we know it was because the homelessness was not reported in that time period."),
                                       tags$li("- Stableness: After it is increased, it is back to where it started from so that means the homelessness is kind of stable other than the time period it was not recorded.")
                                     )
                              ),
                              column(6,
                                     plotOutput("plotDudley")
                              )
                            ),
                            fluidRow(
                              column(6,
                                     tags$h2("3. Sandwell:"),
                                     tags$ul(
                                       tags$li("- Initial Decline: The decrease might suggest that policies or economic conditions were improving, leading to a reduction in homelessness."),
                                       tags$li("- Sharp Increase: A sudden spike could indicate an economic crisis, policy changes, or a surge in housing costs.")
                                     )
                              ),
                              column(6,
                                     plotOutput("plotSandwell")
                              )
                            ),
                            fluidRow(
                              column(6,
                                     tags$h2("4. Wolverhampton:"),
                                     tags$ul(
                                       tags$li("- Initial Decrease: The graph starts with a decline in homelessness. This suggests that conditions for vulnerable populations in Wolverhampton were improving during this period."),
                                       tags$li("- Slight Increase: Towards the end, there's a minor uptick in homelessness, hinting at a potential deterioration in conditions.")
                                     )
                              ),
                              column(6,
                                     plotOutput("plotWolverhampton")
                              )
                            ),
                            tags$br(),
                            tags$h2("Potential Reasons for the Trend:"),
                            tags$ul(
                              tags$li("1. Economic Factors: Variations in homelessness often correlate with economic ups and downs. Economic downturns, recessions, or significant job losses can lead to an increase in homelessness."),
                              tags$li("2. Housing Market Dynamics: The availability and affordability of housing play a pivotal role. An increase in rents or a lack of affordable housing options can lead to spikes in homelessness."),
                              tags$li("3. Policy and Social Services: Changes in government policies related to housing, social welfare, and support services can have significant impacts. Effective policies can reduce homelessness, while changes or cuts in support can lead to increases."),
                              tags$li("4. Social and Personal Factors: Issues like family breakdowns, mental health challenges, addiction, or lack of social support can contribute to homelessness."),
                              tags$li("5. External Events: Natural disasters, large-scale industrial or business closures, or other significant events can disrupt communities and lead to temporary or long-term homelessness.")
                            )
                          )
                 ),
                 # Tabs for each analysis
                 tabPanel("Initial Assessment by Area", plotOutput("initialAssessmentPlot")),
                 tabPanel("Prevention and Relief Duties by Area",
                          plotOutput("preventionDutyPlot"),
                          plotOutput("reliefDutyPlot")
                 ),
                 tabPanel("Support Needs Analysis",
                          plotOutput("noSupportNeedsPlot"),
                          plotOutput("unknownSupportNeedsPlot")
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
  
  output$plotWalsall <- renderPlot({
    plot_specific_authority("Walsall")
  })
  
  output$plotDudley <- renderPlot({
    plot_specific_authority("Dudley")
  })
  
  output$plotSandwell <- renderPlot({
    plot_specific_authority("Sandwell")
  })
  
  output$plotWolverhampton <- renderPlot({
    plot_specific_authority("Wolverhampton")
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
  
  
  # Initial Assessment by Area
  output$initialAssessmentPlot <- renderPlot({
    ggplot(la_total_geo, aes(x = Area, y = Initial_assessment)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle("Initial Assessment by Area")
  })
  
  # Prevention Duty by Area
  output$preventionDutyPlot <- renderPlot({
    ggplot(la_total_geo, aes(x = Area, y = Prevention_duty)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle("Prevention Duty by Area")
  })
  
  # Relief Duty by Area
  output$reliefDutyPlot <- renderPlot({
    ggplot(la_total_geo, aes(x = Area, y = Relief_duty)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle("Relief Duty by Area")
  })
  
  # Support Needs Analysis Plots
  output$noSupportNeedsPlot <- renderPlot({
    ggplot(geo_regions_support_needs, aes(x = Area, y = household_no_support)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle("Household with No Support Needs by Geographical Regions")
  })
  
  output$unknownSupportNeedsPlot <- renderPlot({
    ggplot(geo_regions_support_needs, aes(x = Area, y = household_unknown_support)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle("Household with Unknown Support Needs by Geographical Regions")
  })
  
}

shinyApp(ui = ui, server = server)
