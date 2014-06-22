# library path
.libPaths('C:\\Users\\mbeck\\R\\library')

# startup message
cat('Case the joint...\n')

# packages to use
library(knitr)
library(reshape2) 
library(plyr)
library(ggplot2)
library(scales)
library(doParallel)
library(foreach)
library(Metrics)
library(GGally)
library(gridExtra)
library(ggmap)
library(data.table)

setwd('M:/docs/SWMP/detiding_cases/')

# functions to use
source('case_funs.r')

cases <- c('KACHD', 'PDBBY', 'MARMB', 'WKBFR')

# setup parallel, for ddply in interpgrd 
cl <- makeCluster(8)
registerDoParallel(cl)

# iterate through evaluation grid to create sim series
strt <- Sys.time()

# case_subs <- list(75000:76500, 60000:61500, 10000:11500, 88500:90000)
wins_subs <- list(
  list(4, 6, NULL), 
  list(4, 6, NULL), 
  list(4, 6, NULL), 
  list(4, 6, NULL)
  )

strt <- Sys.time()

# do w/ tide
foreach(case = cases) %dopar% {
   
  to_proc <- prep_wtreg(case)
  subs <- with(to_proc, grepl('2012', format(DateTimeStamp, '%Y'))) #&
#       grepl('6',format(DateTimeStamp, '%m')))
  to_proc <- to_proc[subs, ]
  
  # progress
  sink('log.txt')
  cat('Log entry time', as.character(Sys.time()), '\n')
  cat(which(case == cases), ' of ', length(cases), '\n')
  print(Sys.time() - strt)
  sink()
  
  # get windows
  wins_in <- wins_subs[[which(case == cases)]]
  
  # get pred, norm
  wtreg <- wtreg_fun(to_proc, wins = wins_in, parallel = F, progress = F)
  
  # save results
  wtreg_nm <-paste0(case, '_wtreg') 
  assign(wtreg_nm, wtreg)
  save(
    list = wtreg_nm,
    file=paste0(case,'_wtreg.RData')
    )

  # clear RAM
  rm(list = wtreg_nm)
  
  }

stopCluster(cl)

