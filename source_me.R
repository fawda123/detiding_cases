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

cases <- c('ELKVM','PDBBY', 'RKBMB', 'SAPDC')

# setup parallel backend
cl <- makeCluster(4)
registerDoParallel(cl)

# iterate through evaluation grid to create sim series
strt <- Sys.time()

# windows to check
wins <- c(0.25, 0.5, 1, 2, 5)

# do w/ tide, subset by year
foreach(case = cases) %dopar% {
   
  to_proc <- prep_wtreg(case)
  yr_sel <- as.numeric(format(to_proc$DateTimeStamp, '%Y'))
  yr_sel <- yr_sel %in% 2012
  to_proc <- to_proc[yr_sel, ]
  
  # iterate through window lenghts
  for(i in wins){
  # get pred, norm
  
    wtreg <- wtreg_fun(to_proc, win = i)
  
    # progress
    sink('log.txt')
    cat('Log entry time', as.character(Sys.time()), '\n')
    cat('window', i, which(case == cases), 'of', length(cases), '\n')
    print(Sys.time() - strt)
    sink()
    
    # save results
    wtreg_nm <- paste0(case, '_wtreg') 
    assign(wtreg_nm, wtreg)
    save(
      list = wtreg_nm,
      file=paste(case, which(i == wins), 'wtreg.RData', sep = '_')
      )

  # clear RAM
  rm(list = wtreg_nm)
  
  }
stopCluster(cl)

#####
# get metab ests before and after detiding
# one element per site, contains both metab ests in the same data frame

# setup parallel backend
cl <- makeCluster(4)
registerDoParallel(cl)

# start time
strt <- Sys.time()

# metab ests as list
met_ls <- foreach(case = cases) %dopar% {
  
  # progress
  sink('log.txt')
  cat('Log entry time', as.character(Sys.time()), '\n')
  cat(which(case == cases), ' of ', length(cases), '\n')
  print(Sys.time() - strt)
  sink()
  
  # get data for eval
  nm <- paste0(case, '_wtreg')
  load(paste0(nm, '.RData'))
  dat_in <- get(nm)
  
  # get metab for obs DO
  met_obs <- nem.fun(dat_in, stat = case, DO_var = 'DO_obs')
  met_dtd <- nem.fun(dat_in, stat = case, DO_var = 'DO_dtd')
  
  # combine results
  col_sel <- c('Pg', 'Rt', 'NEM')
  met_obs <- met_obs[, c('Station', 'Date', 'Tide', col_sel)]
  met_dtd <- met_dtd[, col_sel]
  names(met_dtd) <- c('Pg_dtd', 'Rt_dtd', 'NEM_dtd')
  met_out <- cbind(met_obs, met_dtd)

  # return results (first row is last day of 2011)
  met_out[-1, ]

  }
stopCluster(cl)

names(met_ls) <- cases
save(met_ls, file = 'met_ls.RData')

######
# calculate instantaneous flux rates

# setup parallel backend
cl <- makeCluster(4)
registerDoParallel(cl)

# start time
strt <- Sys.time()

# metab ests as list
met_ls_inst <- foreach(case = cases) %dopar% {
  
  # progress
  sink('log.txt')
  cat('Log entry time', as.character(Sys.time()), '\n')
  cat(which(case == cases), ' of ', length(cases), '\n')
  print(Sys.time() - strt)
  sink()
  
  # get data for eval
  nm <- paste0(case, '_wtreg')
  load(paste0(nm, '.RData'))
  dat_in <- get(nm)
  
  # get metab for obs DO
  met_obs <- inst.flux.fun(dat_in, stat = case, DO_var = 'DO_obs')
  met_dtd <- inst.flux.fun(dat_in, stat = case, DO_var = 'DO_dtd')
  
  # combine results, DOF corrects for air-sea xchange
  DOF_obs <- with(met_obs, DOF - D)
  D_obs <- with(met_obs, D)
  DOF_dtd <- with(met_dtd, DOF - D)
  D_dtd <- with(met_dtd, D)

  met_out <- data.frame(met_obs[, 1:17], DOF_obs, D_obs, DOF_dtd, D_dtd)
  
  # return results (first row is last day of 2011)
  met_out[-1, ]

  }
stopCluster(cl)

names(met_ls_inst) <- cases
save(met_ls_inst, file = 'met_ls_inst.RData')