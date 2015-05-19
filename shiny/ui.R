library(shiny)

# Define UI for application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Case studies for weighted regression"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    selectInput("case", "Choose a site:", 
                list("Elkhorn Slough" = "ELKVM", 
                     "Padilla Bay" = "PDBBY", 
                     "Rookery Bay" = "RKBMB",
                     "Sapelo Island" = "SAPDC"),
                selected = 'Sapelo Island'
                ),
    selectInput("daywin", "Choose a day window:", 
                c(1, 3, 6, 9 , 12)
                ),
    selectInput("hourwin", "Choose an hour window:", 
                c(1, 3, 6, 9, 12)
                ),
    selectInput("tidwin", "Choose a tide window:", 
                c(0.2, 0.4, 0.6, 0.8, 1)
                ),
    dateRangeInput("daterange", "Date range for plot:",
                   start = "2012-01-01",
                   end   = "2012-12-31", 
                   min   = "2012-01-01", 
                   max   = "2012-12-31"),
    checkboxInput('corr', 'Show DO and tide scatterplot only', value = FALSE)
    

  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("resplot", width = "100%")
  )
))