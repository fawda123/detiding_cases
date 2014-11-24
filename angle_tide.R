######
# sun angle correlated with tidal height as polar coordinates 
# moving window corrs, done for small, large windows

library(oce)
library(foreach)

cl <- makeCluster(8)
registerDoParallel(cl)

# objects for selecting window width combinations for case studies

cases <- c('ELKVM', 'PDBBY', 'RKBMB', 'SAPDC')
days <- c(1, 12)
its <- expand.grid(days, cases)
its$nms <- paste(its$Var1, its$Var2)
out_ls <- vector('list', nrow(its))
names(out_ls) <- its$nms
for(i in 1:nrow(its)){
  
  # log
  sink('C:/Users/mbeck/Desktop/log.txt')
  cat(i, 'of', nrow(its))
  sink()
  
  # values to iterate
  case <- its$Var2[i]
  daywin <- its$Var1[i]
  
  # file to upload, doesn't matter which
  sel_vec <- paste0(case, '_wtreg_1.RData')
  load(paste0('wtreg/', sel_vec))
  dat <- get(gsub('\\.RData$', '', sel_vec))
  
  # sun angle
  locs <- as.numeric(get_map_meta(case)[, c('Longitude', 'Latitude')])
  utc_time <- as.POSIXlt(dat$DateTimeStamp, tz = 'UTC')
  sun_angle <- sunAngle(utc_time, locs[1], locs[2])$altitude
  
  # polar coords for tidal height, in degrees
  tide_angle <- with(dat, atan(dTide/c(diff(dec_time)[1], diff(dec_time))) * 180/pi)

  # weights
  dec_time <- dec_fun(dat)
  dec_time <- dec_time[, 'dec_time']
  dat_in <- data.frame(dec_time, hour = dat$hour, Tide = dat$Tide)
  
  cor_out <- foreach(row = 1:nrow(dat)) %dopar% {
    
    ref_in <- dat_in[row, ]
    
    wts <- wt_fun(ref_in, dat_in, wins = list(daywin, 1e6, 1e6))
    gr_zero <- which(wts > 0)
    
    sun_in <- (sun_angle)[gr_zero]
    tide_in <- (tide_angle)[gr_zero]
    
    cor(sun_in, tide_in)
    
  }
 
  cor_out <- unlist(cor_out)
  
  out_ls[[its$nms[i]]] <- cor_out
  
}

angle_tide_its <- out_ls
save(angle_tide_its, file = 'angle_tide_its.RData')

tmp <- do.call('cbind', angle_tide_its)

tmp <- data.frame(DateTimeStamp = dat$DateTimeStamp, tmp)
tmp <- melt(tmp, id.var = 'DateTimeStamp')
tmp$daywin <- gsub('^X|\\..*$', '' ,tmp$variable)
tmp$case <- gsub('^X[0-9]*\\.', '', tmp$variable)

pdf('C:/Users/mbeck/Desktop/angle_tide_its.pdf', height = 9, width = 5)
for(daywin in unique(tmp$daywin)){
  
  p1 <- ggplot(tmp[tmp$daywin %in% daywin, ], 
      aes(x = DateTimeStamp, y = value)) + 
    geom_line() + 
    facet_wrap(~ variable, ncol = 1) + 
    theme_bw() + 
    ylab('Correlation') +
    scale_y_continuous(limits= c(-1, 1)) +
    geom_hline(yintercept = 0, linetype = 'dashed', colour = 'red') +
    ggtitle(paste('Day window', daywin))

  print(p1)
  
}
dev.off()

######

tmp <- dat[1:1000, c('dTide', 'Tide', 'dec_time')]

pangle <- with(tmp, atan(dTide/c(diff(dec_time)[1], diff(dec_time))) * 180/pi)

par(mfrow = c(3, 1))
plot(Tide ~ dec_time, data = tmp, type = 'l')
plot(dTide ~ dec_time, data = tmp, type = 'l')
plot(pangle, type = 'l')
