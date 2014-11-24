library(shiny)

# Define UI for application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Tidal height and metabolism correlations as a function of half-window widths"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
  
    radioButtons('day', label = h3('Day'),
      choices = list("one" = 1, "three" = 3, "six" = 6, "nine" = 9, "twelve" = 12), selected = 1),
    
    radioButtons('hour', label = h3('Hour'),
      choices = list("one" = 1, "three" = 3, "six" = 6, "nine" = 9, "twelve" = 12), selected = 1),
    
    radioButtons('tide', label = h3('Tidal range'), 
      choices = list("0.2" = 0.2, "0.4" = 0.4, "0.6" = 0.6, "0.8" = 0.8, "1" = 1), selected = 0.2)
    
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("simplot", width = "100%")
  )
    
))