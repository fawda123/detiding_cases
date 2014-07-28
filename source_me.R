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
cl <- makeCluster(8)
registerDoParallel(cl)

# iterate through evaluation grid to create sim series
strt <- Sys.time()
day_wins <- c(1, 2, 5, 10, 15, 30, 45, 60)
tide_wins <- c(0.25, 0.5, 1, 2)

case_grds <- expand.grid(day_wins, tide_wins)
names(case_grds) <- c('Day', 'Tide')
save(case_grds, file = 'case_grds.RData')

# do w/ tide, subset by year
for(case in cases){
    
  # prep input
  to_proc <- prep_wtreg(case)
  yr_sel <- as.numeric(format(to_proc$DateTimeStamp, '%Y'))
  yr_sel <- yr_sel %in% 2012
  to_proc <- to_proc[yr_sel, ]
  
  #iterate through window grids
  foreach(i = 1:nrow(case_grds)) %dopar% {
  
    # progress
    sink('log.txt')
    cat('Log entry time', as.character(Sys.time()), '\n')
    cat('row', i, 'case', which(case == cases), 'of', length(cases), '\n')
    print(Sys.time() - strt)
    sink()
    
    # windows in case_grds
    wins_in <- list(case_grds[i, 1], case_grds[i, 2])
    
    # get pred, norm
    wtreg <- wtreg_fun(to_proc, wins = wins_in)

    # save results
    wtreg_nm <- paste(case, 'wtreg', i, sep ='_') 
    assign(wtreg_nm, wtreg)
    save(
      list = wtreg_nm,
      file=paste(case, '_wtreg_', i, '.RData', sep = '')
      )

    # clear RAM
    rm(list = wtreg_nm)
    
    }
  
  }
stopCluster(cl)

#####
# get metab ests before and after detiding
# one element per site, contains both metab ests in the same data frame

# setup parallel backend
cl <- makeCluster(8)
registerDoParallel(cl)

# start time
strt <- Sys.time()

cases <- list.files(path = getwd(), pattern = '_wtreg_')

# metab ests as list
met_ls <- foreach(case = cases) %dopar% {
  
  # progress
  sink('log.txt')
  cat('Log entry time', as.character(Sys.time()), '\n')
  cat(which(case == cases), ' of ', length(cases), '\n')
  print(Sys.time() - strt)
  sink()
  
  # get data for eval
  load(case)
  nm <- gsub('.RData', '', case)
  stat <- gsub('_wtreg_[0-9]+$', '', nm)
  dat_in <- get(nm)
  
  # get metab for obs DO
  met_obs <- nem.fun(dat_in, stat = stat, 
    DO_var = 'DO_obs')
  met_dtd <- nem.fun(dat_in, stat = stat, 
    DO_var = 'DO_dtd')
  
  # combine results
  col_sel <- c('Pg', 'Rt', 'NEM')
  met_obs <- met_obs[, c('Station', 'Date', 'Tide', col_sel)]
  met_dtd <- met_dtd[, col_sel]
  names(met_dtd) <- c('Pg_dtd', 'Rt_dtd', 'NEM_dtd')
  met_out <- cbind(met_obs, met_dtd)

  # return results
  met_out

  }
stopCluster(cl)

names(met_ls) <- cases
save(met_ls, file = 'met_ls.RData')

######
# calculate instantaneous flux rates

# setup parallel backend
cl <- makeCluster(8)
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
  load(case)
  nm <- gsub('.RData', '', case)
  stat <- gsub('_wtreg_[0-9]+$', '', nm)
  dat_in <- get(nm)
  
  # get inst DOF for obs and dtd
  met_obs <- inst.flux.fun(dat_in, stat = stat, 
    DO_var = 'DO_obs')
  met_dtd <- inst.flux.fun(dat_in, stat = stat, 
    DO_var = 'DO_dtd')
  
  # combine results, DOF corrects for air-sea xchange
  DOF_obs <- with(met_obs, DOF - D)
  D_obs <- with(met_obs, D)
  DOF_dtd <- with(met_dtd, DOF - D)
  D_dtd <- with(met_dtd, D)

  rm_col <- !names(met_obs) %in% c('D', 'DOF')
  met_out <- data.frame(met_obs[, rm_col], DOF_obs, D_obs, DOF_dtd, D_dtd)
  
  # return results
  met_out

  }
stopCluster(cl)

names(met_ls_inst) <- cases
save(met_ls_inst, file = 'met_ls_inst.RData')

