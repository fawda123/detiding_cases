rm(list = ls())

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

cases <- c('PDBJE', 'RKBMB', 'SAPDC', 'TJRBR')

# setup parallel backend
cl <- makeCluster(8)
registerDoParallel(cl)

# iterate through evaluation grid to create sim series
strt <- Sys.time()

# do w/ tide
foreach(case = cases) %dopar% {
   
  to_proc <- prep_wtreg(case)
  
  # progress
  sink('log.txt')
  cat('Log entry time', as.character(Sys.time()), '\n')
  cat(which(case == cases), ' of ', length(cases), '\n')
  print(Sys.time() - strt)
  sink()
  
  # get pred, norm
  wtreg <- wtreg_fun(to_proc)
  
  # save results
  wtreg_nm <- paste0(case, '_wtreg') 
  assign(wtreg_nm, wtreg)
  save(
    list = wtreg_nm,
    file=paste0(case,'_wtreg.RData')
    )

  # clear RAM
  rm(list = wtreg_nm)
  
  }
stopCluster(cl)

