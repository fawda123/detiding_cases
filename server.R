# packages to use
library(reshape2) 
library(plyr)
library(ggplot2)

load('case_grds.RData')
load('cor_res.RData')

# set ggplot theme
theme_set(theme_bw())

# Define server logic required to generate and plot data
shinyServer(function(input, output) {
  
  output$simplot <- renderPlot({
    
    # plotting code
    
    # input from ui
    day <- input$day
    hour <- input$hour
    tide <- input$tide
    
#     browser()
    
    # plot
    ind <- cor_res$dec_time == as.numeric(day) & cor_res$hour == as.numeric(hour) & cor_res$Tide == as.numeric(tide)
    to_plo <- cor_res[ind, ]
    
    # reassign factor labels
    to_plo$month <- factor(to_plo$month, levels = c('01', '02', '03', '04',
      '05', '06', '07', '08', '09', '10', '11', '12'), 
      labels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11',' 12')
      )
    to_plo$var <- factor(to_plo$var, levels = c('Pg', 'Rt', 'NEM', 'do'), 
      labels = c('Pg', 'Rt', 'NEM', 'DO'))
    to_plo$sub_var <- factor(to_plo$sub_var, levels = c('dtd', 'obs'), 
      labels = c('Filtered', 'Observed'))
    to_plo$site <- factor(to_plo$site, 
      levels = c('ELKVM', 'PDBBY', 'RKBMB', 'SAPDC'),
      labels = c('Elkhorn Slough', 'Padilla Bay', 'Rookery Bay', 
        'Sapelo Island')
      )
    
    p <- ggplot(to_plo, aes(x = factor(month), y = value, group = sub_var, colour = sub_var)) + 
      geom_line() +
      geom_point() + 
      geom_hline(yintercept = 0, linetype = 'dashed') + 
      facet_grid(site ~ var) + 
      scale_y_continuous(limits = c(-1, 1)) +
      ylab('Correlation with tide') +
      xlab('Month') +
      theme(text = element_text(size=18), 
        legend.position = 'top',
        legend.direction = 'horizontal',
        legend.title = element_blank()
        )
    
    print(p)
    
    },height = 600, width = 900)

    })