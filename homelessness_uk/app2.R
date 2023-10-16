library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(plotly)

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

la_age <- read_excel("detailed_LA_23_total.xlsx", sheet = 3)
geo_regions_age_duty <- la_age[la_age$Area %in% traditional_regions, ]

la_referral <- read_excel("detailed_LA_23_total.xlsx", sheet = 4)
geo_regions_referral_duty <- la_referral[la_referral$Area %in% traditional_regions, ]

get_referral_description <- function(referral) {
  switch(referral,
         "Adult_secure_estate(prison)" = "This indicates which regions have more referrals from prisons, potentially highlighting the need for better reintegration programs.",
         "Youth_secure_estate" = "This indicates which regions have more referrals from youth secure estates.",
         "National_probation_service" = "Regions with higher numbers here may require more focused services for individuals on probation.",
         "Community_rehabilition_company" = "This indicates regions with referrals from community rehabilitation companies.",
         "Ugent_treatment_centres" = "Referrals from urgent treatment centers indicate regions where immediate medical attention was sought.",
         "Mental_health" = "This can indicate the regions where mental health services may be most needed.",
         "Jobcentre_plus" = "This will show which regions have more referrals from employment services, potentially indicating a link between unemployment and homelessness.",
         "Adult_social_services" = "Regions with higher referrals from adult social services may indicate a need for more comprehensive social care programs.",
         "Children_social_services" = "Referrals from children's social services highlight regions with potential vulnerabilities among younger populations.",
         "Children_early_help" = "This indicates regions where early intervention services for children were sought.",
         "Nil_resource_team" = "Referrals from the Nil resource team can provide insights into specific cases without external resource involvement.",
         "State_of_defence" = "Regions with higher referrals from the state of defense may have populations involved in defense services.",
         "Unknown" = "For unknown referrals, the source of the referral wasn't clearly identified.",
         "household_referred_by_agency" = "Regions with more households referred by agencies can provide insights into the involvement of external organizations.",
         "Household_referred_by_local_authority" = "This shows regions where local authorities have been actively referring households.",
         "UNKNOWN REFERRAL"
  )
}

la_ethnicity <- read_excel("detailed_LA_23_total.xlsx", sheet = 5)
geo_regions_ethnicity_duty <- la_ethnicity[la_ethnicity$Area %in% traditional_regions, ]

la_employment <- read_excel("detailed_LA_23_total.xlsx", sheet = 6)
geo_regions_employment_duty <- la_employment[la_employment$Area %in% traditional_regions, ]

house_sale <- read_excel("ppd_data_headers.xlsx")
# Keep only the relevant columns
relevant_columns <- c('unique_id', 'price_paid', 'deed_date', 'postcode', 'property_type', 'new_build', 'estate_type')
house_sale <- house_sale %>%
  select(all_of(relevant_columns))
house_sale <- na.omit(house_sale, cols = "postcode")

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
                 ),
                 
                 tabPanel("Homelessness by Age Group",
                          selectInput("ageGroup", "Select Age Group:", 
                                      choices = c("16-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75plus", "not_known")),
                          plotOutput("agePlot")
                 ),
                 
                 tabPanel("Referrals by Region",
                          selectInput("referralSource", "Select Referral Source:", 
                                      choices = c("Adult_secure_estate(prison)", "Youth_secure_estate", "National_probation_service", 
                                                  "Community_rehabilition_company", "Ugent_treatment_centres", "Mental_health", 
                                                  "Jobcentre_plus", "Adult_social_services", "Children_social_services", 
                                                  "Children_early_help", "Nil_resource_team", "State_of_defence", "Unknown", 
                                                  "household_referred_by_agency", "Household_referred_by_local_authority")),
                          plotOutput("referralPlot"),
                          textOutput("referralDescription")
                 ),
                 
                 tabPanel("Ethnicity Analysis",
                          selectInput("ethnicitySelection", "Select Ethnicity:", 
                                      choices = c("British", "Irish", "Gypsy", "Other_white", "British_African", 
                                                  "British_Caribbean", "Other_black", "British_pakistani", 
                                                  "British_Indian", "British_Bangladeshi", "British_chinese", 
                                                  "Other_asian", "White_black_caribbean", "White_black_african", 
                                                  "White_asian", "Other_multiple_ethnic_background", "Arab", 
                                                  "Other_ethnic_groups", "Unknown")),
                          plotOutput("ethnicityPlot")
                 ),
                 
                 tabPanel("Employment Analysis",
                          selectInput("employmentSelection", "Select Employment Status:", 
                                      choices = c("Full_time", "Part_time", "Student", "Registered_unemployed", 
                                                  "Not_registered_but_seeking", "Not_seeking", "Not_working_due_to_illness", 
                                                  "Retired", "Other", "Unknown")),
                          plotOutput("employmentPlot"),
                          textOutput("employmentDescription")
                 ),
                 
                 tabPanel("Top Local Authorities",
                          plotlyOutput("topAuthoritiesPlot"),  # First plot
                          plotlyOutput("facetedAuthoritiesPlot"),  # Second, faceted plot
                          plotlyOutput("meanHomelessnessPlot")  # Third, mean homelessness plot
                 ),
                 
                 tabPanel("Homelessness vs Property Prices",
                          plotlyOutput("homelessnessVsPropertyPricesPlot")
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
  
  output$agePlot <- renderPlot({
    selected_age <- input$ageGroup
    
    ggplot(geo_regions_age_duty, aes(x = Area, y = get(selected_age))) +
      geom_bar(stat = "identity") +
      ggtitle(paste("Homelessness for Age", selected_age, "by Geographical Regions")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$agePlot <- renderPlot({
    selected_age <- input$ageGroup
    
    ggplot(geo_regions_age_duty, aes(x = Area, y = get(selected_age))) +
      geom_bar(stat = "identity") +
      ggtitle(paste("Homelessness for Age", selected_age, "by Geographical Regions")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$referralPlot <- renderPlot({
    selected_referral <- input$referralSource
    
    ggplot(geo_regions_referral_duty, aes(x = Area, y = get(selected_referral))) +
      geom_bar(stat = "identity") +
      ggtitle(paste("Referrals from", selected_referral, "by Geographical Regions")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$referralDescription <- renderText({
    get_referral_description(input$referralSource)
  })
  
  output$ethnicityPlot <- renderPlot({
    selected_ethnicity <- input$ethnicitySelection
    
    ggplot(geo_regions_ethnicity_duty, aes(x = Area, y = get(selected_ethnicity))) +
      geom_bar(stat = "identity") +
      ggtitle(paste("Homelessness Cases Identifying as", selected_ethnicity, "by Geographical Regions")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$employmentPlot <- renderPlot({
    selected_employment <- input$employmentSelection
    
    ggplot(geo_regions_employment_duty, aes(x = Area, y = get(selected_employment))) +
      geom_bar(stat = "identity") +
      ggtitle(paste("Homelessness Cases with Employment Status:", selected_employment, "by Geographical Regions")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  get_employment_description <- function(employment) {
    descriptions <- list(
      Full_time = "High numbers here could indicate a need for affordable housing or living wage initiatives.",
      Part_time = "High numbers might indicate employment instability as a factor in homelessness.",
      Student = "High number might indicate students who are highly in debt because of their study fees and been homeless because they don't have any source of income",
      Registered_unemployed = "These regions potentially require job training or employment programs.",
      Not_registered_but_seeking = "High number indicates that they is so much difficulty in finding a job that people are homeless because of being unemployed even after getting good quality of education",
      Not_seeking = "The high count in this category possibly points to other factors like illness or disability as the main issue.",
      Not_working_due_to_illness = "High number gives us an idea as to where we need to invest more in terms of healthcare",
      Retired = "This will show which regions have homelessness cases among retirees."
      
    )
    
    return(descriptions[[employment]])
  }
  
  output$employmentDescription <- renderText({
    get_employment_description(input$employmentSelection)
  })
  
  output$topAuthoritiesPlot <- renderPlotly({
    # Calculate the average number of homelessness decisions for each local authority, excluding 'England'
    averages <- melted_data %>%
      filter(`Local authority area` != 'ENGLAND') %>%
      group_by(`Local authority area`) %>%
      summarize(Average = mean(`Homelessness Decisions`, na.rm = TRUE)) %>%
      arrange(desc(Average)) %>%
      head(5)  # Select the top 5
    
    # Filter the data for the selected local authorities
    top_authorities_data <- melted_data %>% 
      filter(`Local authority area` %in% averages$`Local authority area`)
    
    # Create the plot
    p <- ggplot(top_authorities_data, aes(x = Year, y = `Homelessness Decisions`, color = `Local authority area`)) +
      geom_line() +
      geom_point(aes(text = paste('Decisions:', `Homelessness Decisions`))) +  # Add text for hover
      ggtitle("Top 5 Local Authorities by Average Number of Homelessness Decisions (Excluding England)") +
      xlab("Year") +
      ylab("Number of Homelessness Decisions") +
      theme_minimal() +
      theme(legend.position="bottom") +
      scale_color_discrete(name = "Local Authority")
    
    # Convert ggplot object to a plotly object
    ggplotly(p, tooltip = "text")
  })
  
  output$facetedAuthoritiesPlot <- renderPlotly({
    # Create a faceted plot for a subset of local authorities, for example, the top 10
    top_authorities_data <- melted_data %>% 
      filter(`Local authority area` %in% 
               (melted_data %>% 
                  filter(`Local authority area` != 'ENGLAND') %>%
                  group_by(`Local authority area`) %>%
                  summarize(Average = mean(`Homelessness Decisions`, na.rm = TRUE)) %>%
                  arrange(desc(Average)) %>%
                  head(10) %>%  # Select the top 10
                  pull(`Local authority area`)))
    
    p <- ggplot(top_authorities_data, aes(x = Year, y = `Homelessness Decisions`)) +
      geom_line() +
      geom_point() +
      facet_wrap(~`Local authority area`, scales = 'free_y') +  # Create a separate facet for each local authority
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Convert ggplot object to a plotly object
    ggplotly(p)
  })
  
  output$meanHomelessnessPlot <- renderPlotly({
    # Plot the mean number of homelessness decisions per year across all local authorities
    mean_data <- melted_data %>%
      group_by(Year) %>%
      summarize(Mean = mean(`Homelessness Decisions`, na.rm = TRUE))
    
    p <- ggplot(mean_data, aes(x = Year, y = Mean)) +
      geom_line() +
      geom_point() +
      ggtitle("Mean Number of Homelessness Decisions per Year") +
      theme_minimal()
    
    # Convert ggplot object to a plotly object
    ggplotly(p)
  })
  
  output$homelessnessVsPropertyPricesPlot <- renderPlotly({
    # Extract year from deed_date
    house_sale$year <- format(house_sale$deed_date, "%Y")
    
    # Calculate average price by year
    avg_price_by_year <- aggregate(price_paid ~ year, data=house_sale, FUN=mean)
    avg_price_by_year$year <- as.numeric(avg_price_by_year$year)
    
    # Calculate the mean homelessness decisions per year
    mean_homelessness_by_year <- melted_data %>%
      group_by(Year) %>%
      summarize(Mean = mean(`Homelessness Decisions`, na.rm = TRUE))
    
    # Combine the datasets based on year
    combined_data <- left_join(mean_homelessness_by_year, avg_price_by_year, by = c("Year" = "year"))
    
    # Create a basic plot with homelessness decisions
    p <- plot_ly(combined_data, x = ~Year, y = ~Mean, type = 'scatter', mode = 'lines+markers', name = 'Homelessness Decisions', line = list(color = 'blue'))
    
    # Add the property prices as a secondary y-axis
    p <- p %>% add_trace(y = ~price_paid, name = 'Average Property Price', line = list(color = 'red')) %>%
      layout(yaxis2 = list(overlaying = "y", side = "right"))
    
    return(p)
  })
  
}

shinyApp(ui = ui, server = server)
