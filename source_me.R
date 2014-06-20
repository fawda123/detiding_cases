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
  list(4, 12, NULL), 
  list(4, 12, NULL), 
  list(4, 12, NULL), 
  list(4, 12, NULL)
  )

strt <- Sys.time()

# do w/ tide
foreach(case = cases) %dopar% {
   
  to_proc <- prep_wtreg(case)
  subs <- with(to_proc, as.numeric(format(DateTimeStamp, '%Y')) == 2010 &
      as.numeric(format(DateTimeStamp, '%m')) == 6)
  to_proc <- to_proc[subs, ]
  
  # progress
  sink('log.txt')
  cat('Log entry time', as.character(Sys.time()), '\n')
  cat(which(case == cases), ' of ', length(cases), '\n')
  print(Sys.time() - strt)
  sink()
  
  # get windows
  wins_in <- wins_subs[[which(case == cases)]]
  
  # create wt reg contour surface
  int_proc <- interp_td_grd(to_proc, wins = wins_in)
  
  # get predicted, normalized from interpolation grid
  prd_nrm <- prdnrm_td_fun(int_proc, to_proc)
  
  # save interpolation grid
  int_nm <-paste0(case, '_intgrd_td') 
  assign(int_nm, int_proc)
  save(
    list = int_nm,
    file=paste0(case,'_intgrd_td.RData')
    )
  
  # save predicted, normalized results
  prdnrm_nm <-paste0(case, '_prdnrm_td') 
  assign(prdnrm_nm, prd_nrm)
  save(
    list = prdnrm_nm,
    file=paste0(case,'_prdnrm_td.RData')
    )

  # clear RAM
  rm(list = c(int_nm, prdnrm_nm))
  
  }

stopCluster(cl)

