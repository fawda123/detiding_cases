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

cases <- c('ELKVM', 'PDBBY', 'RKBMB', 'SAPDC')

# setup parallel, for ddply in interpgrd 
cl <- makeCluster(8)
registerDoParallel(cl)

# iterate through evaluation grid to create sim series
strt <- Sys.time()

dy_wins <- c(2, 4)
hr_wins <- c(12, 24)
td_wins <- c(1, 2)
case_grds <- expand.grid(dy_wins, hr_wins, td_wins)
names(case_grds) <- c('dec_time', 'hour', 'Tide')
save(case_grds, file = 'case_grds.RData')

load('case_grds.RData')

# do w/ tide
foreach(case = cases) %dopar% {
   
  to_proc <- prep_wtreg(case)
  subs <- format(to_proc$DateTimeStamp, '%Y') %in% '2012'
  to_proc <- to_proc[subs, ]
  
  # progress
  sink('log.txt')
  cat('Log entry time', as.character(Sys.time()), '\n')
  cat(which(case == cases), ' of ', length(cases), '\n')
  print(Sys.time() - strt)
  sink()
  
  for(i in 1:nrow(case_grds)){
    
    # get windows
    wins_in <- c(case_grds[i,])
    
    # create wt reg contour surface
    int_proc <- interp_grd(to_proc[1:5000,], wins = list(4, 12, 1), 
      parallel = T, 
      progress = T)
  
    # get predicted, normalized from interpolation grid
    prd_nrm <- prdnrm_fun(int_proc, to_proc[1:5000,])
    
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

