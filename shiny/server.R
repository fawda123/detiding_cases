source('case_funs.r')
load(file = 'met_ls.RData')
load(file = 'case_grds.RData')

require(ggplot2)
require(scales)
require(gridExtra)
require(reshape2)
require(RColorBrewer)
require(StreamMetabolism)

# Define server logic required to generate and plot data
shinyServer(function(input, output) {
  
  output$resplot <- renderPlot({
    
    # add plotting code here
    
#     # for debugging only
#     input <- list(case = 'ELKVM', daywin = 1,
#                   hourwin = 6, tidwin = 0.25, 
#                   daterange = c('2012-06-15', '2012-07-15')
#                   )
  
    # subset data by window comb and case
    case <- input$case
    daywin <- input$daywin
    hourwin <- input$hourwin
    tidwin <- input$tidwin
    corr <- input$corr
    
    # date ranges for plotting
    dat.rng<-as.Date(c(input$daterange[1],input$daterange[2])) 
    
    # name combination from subsets
    sel_vec <- with(case_grds, 
                    which(dec_time == daywin & hour == hourwin & Tide == tidwin)
    )
    sel_vec <- paste0(case, '_wtreg_', sel_vec, '.RData')
    
    # subset metabolism and inst_flux lists by case and windows
    met_sub <- met_ls[[sel_vec]]
    load(paste0('wtreg/', sel_vec))
    ins_sub <- get(gsub('.RData', '', sel_vec))
    
    # format value column in ins_sub as posix
    tz_tab <- data.frame(case = c('ELKVM', 'SAPDC', 'RKBMB', 'PDBBY'),
      tz = c('Pacific/Pitcairn', 'America/Jamaica', 'America/Jamaica', 'Pacific/Pitcairn'), 
      stringsAsFactors = FALSE
      )
    tz <- tz_tab[tz_tab$case %in% case, 'tz']
    ins_sub$value <- as.POSIXct(ins_sub$value, tz, origin = '1970-01-01')
    
    # subset by date rngs
    met.rng <- met_sub$Date<=dat.rng[2] & met_sub$Date>=dat.rng[1]
    met_sub <- met_sub[met.rng,]
    
    ins_sub$Date <- as.Date(ins_sub$DateTimeStamp)
    ins.rng <- ins_sub$Date<=dat.rng[2] & ins_sub$Date>=dat.rng[1]
    ins_sub <- ins_sub[ins.rng,]
    
    # percent anoms from met_sub
    anom <- round(100*anoms.fun(met_sub),1)
    anom <- paste0('Anom obs % Pg: ', anom[1], ' Rt: ', anom[2])
    anom_after <- round(100*anoms.fun(met_sub, pgvar = "Pg_dtd", rtvar = "Rt_dtd"),1)
    anom_after <- paste0('Anom dtd % Pg: ', anom_after[1], ' Rt: ', anom_after[2])

    # get mean, se vals
    sums <- apply(met_sub[, c('Pg', 'Rt', 'Pg_dtd', 'Rt_dtd')], 
                  2,
                  function(x) data.frame(
                    Mean = mean(0.032 * x,  na.rm =T), 
                    SE = sd(0.032 * x, na.rm=T)/sqrt(sum(!is.na(x)))
                  )
    )
    sums <- format(do.call('rbind', sums), digits = 2)
    sums_obs <- paste0('Obs mean (se) Pg ', sums[1,1], ' (+/-', sums[1,2], ')')
    sums_obs <- paste0(sums_obs, ', Rt ', sums[2,1], ' (+/-', sums[2,2], ')')
    sums_dtd <- paste0('Dtd mean (se) Pg ', sums[3,1], ' (+/-', sums[3,2], ')')
    sums_dtd <- paste0(sums_dtd, ', Rt ', sums[4,1], ' (+/-', sums[4,2], ')')
    
    ##
    # plots 
    
    # custom theme, mod of theme_bw
    my_theme <- theme(
      legend.title = element_blank(),legend.position = 'top',
      axis.title.x = element_blank(),legend.box= 'horizontal',
      plot.margin= unit(c(0, 1, 0, 1), "lines"), 
      text = element_text(size = 16)
    )
    
    # function for setting range on y axis
    rng.fun<-function(vec.in){
      rngs<-range(vec.in,na.rm=T)
      buffs<-0.07*abs(diff(rngs))
      c(rngs[1]-buffs,rngs[2]+buffs)
    }
  
    ##
    # metab plot
    to_plo1 <- melt(met_sub, id.var = c('Date'), 
                    measure.var = grep('Pg|Rt|NEM', names(met_sub), value = T)
    )
    to_plo1$Input <- 'Observed'
    to_plo1$Input[grep('dtd', to_plo1$variable)] <- 'Detided'
    to_plo1$Input <- factor(to_plo1$Input, levels = c('Observed', 'Detided'))
    to_plo1$variable <- gsub('_dtd', '', to_plo1$variable)
    
    ylab<-expression(paste('g ',O [2], ' ', m^-2, d^-1))
    p1 <- ggplot(to_plo1, aes(x = Date, y = 0.032 * value, group = variable,
                              colour = variable)) +
      geom_line() +
      theme_bw() +
      geom_point(size = 2) +
      facet_wrap(~Input, ncol = 1) +
      scale_y_continuous(ylab)  +
      my_theme +
      ggtitle(paste0(sums_obs, '\n', sums_dtd, '\n', anom, '\n', anom_after))
    
    ##
    # DO plot
    to_plo2 <- ins_sub
    names(to_plo2)[names(to_plo2) %in% 'variable'] <- 'solar'
    ggpoly <- poly.fun(to_plo2$solar, to_plo2)
    
    ylab<-expression(paste('DO (mg ',L^-1,')'))
    p2 <- ggplot(to_plo2, aes(x = DateTimeStamp)) + 
      ggpoly +
      geom_line(aes(y = DO_obs, colour = 'Observed')) +
      geom_line(aes(y = DO_nrm, colour = 'Detided')) +
      coord_cartesian(ylim = rng.fun(to_plo2$DO_obs)) +
      scale_fill_manual(values='orange',labels='Day') +
      theme_bw() +
      scale_y_continuous(ylab)  +
      my_theme
    
    ##
    # DO plot
    to_plo3 <- to_plo2
    
    ylab<-expression(paste('DO (mg ',L^-1,')'))
    p3 <- ggplot(to_plo3, aes(x = DateTimeStamp)) + 
      ggpoly +
      geom_line(aes(y = DO_prd, colour = 'Predicted')) +
      geom_line(aes(y = DO_nrm, colour = 'Detided')) +
      coord_cartesian(ylim = rng.fun(to_plo3$DO_obs)) +
      scale_fill_manual(values='orange',labels='Day') +
      theme_bw() +
      scale_y_continuous(ylab)  +
      my_theme
    
    ## 
    # tide plot
    to_plo4 <- to_plo2
    
    ylab<-expression(paste('Tide (m)'))
    p4 <- ggplot(to_plo4, aes(x = DateTimeStamp)) + 
      ggpoly +
      geom_line(aes(y = Tide), size = 1.2) +
      coord_cartesian(ylim = rng.fun(to_plo4$Tide)) +
      scale_fill_manual('', values='orange',labels='Day') +
      theme_bw() +  
      theme(
        legend.position = 'top',
        axis.title.x = element_blank(),legend.box= 'horizontal',
        plot.margin= unit(c(0, 1, 0, 1), "lines"),
#         axis.text = element_text(size = 14), 
        text = element_text(size = 16)
        ) +
      scale_y_continuous(ylab)

    # scatterplot of DO prd/nrm v tide
    to_plo5 <- melt(to_plo2, id.var = 'Tide', measure.var = c('DO_obs', 'DO_nrm'))
    to_plo5$variable <- factor(to_plo5$variable, levels = c('DO_nrm', 'DO_obs'),
                               labels = c('Detided', 'Observed'))
    
    ylab<-expression(paste('DO (mg ',L^-1,')'))
    pcorr <- ggplot(to_plo5, aes(x = Tide, y = value, 
                                 group = variable, colour = variable)) + 
      geom_point() +
      stat_smooth(method = 'lm') +
      theme_bw() +
      scale_y_continuous(ylab)  +
      scale_x_continuous('Tide (m)') +
      theme(
        legend.title = element_blank(),legend.position = 'top',
        legend.box= 'horizontal',
        plot.margin= unit(c(0, 1, 0, 1), "lines"), 
        text = element_text(size = 16)
        )

    # Get the widths
    pA <- ggplot_gtable(ggplot_build(p1))
    pB <- ggplot_gtable(ggplot_build(p2))
    pC <- ggplot_gtable(ggplot_build(p3))
    pD <- ggplot_gtable(ggplot_build(p4))
    maxWidth = unit.pmax(pA$widths[2:3], pB$widths[2:3], 
                         pC$widths[2:3], pD$widths[2:3])
    
    # Set the widths
    pA$widths[2:3] <- maxWidth
    pB$widths[2:3] <- maxWidth
    pC$widths[2:3] <- maxWidth
    pD$widths[2:3] <- maxWidth
    
    if(corr) print(pcorr)
    else grid.arrange(pA, pB, pC, pD, ncol = 1, heights = c(1.7, 1, 1, 1))
    
    },height = 800, width = 700)

    })