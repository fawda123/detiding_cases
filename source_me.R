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
dy_wins <- c(1, 2, 4, 8)
hr_wins <- c(3, 6, 12, 24)
td_wins <- c(0.25, 0.5, 1, 2)
case_grds <- expand.grid(dy_wins, hr_wins, td_wins)
names(case_grds) <- c('dec_time', 'hour', 'Tide')
save(case_grds, file = 'case_grds.RData')

load('case_grds.RData')

strt <- Sys.time()

# do w/ tide
for(case in cases){
   
  to_proc <- prep_wtreg(case)
  subs <- format(to_proc$DateTimeStamp, '%Y') %in% '2012'
  to_proc <- to_proc[subs, ]
  
  foreach(i = 1:nrow(case_grds)) %dopar% {
    
    # progress
    sink('log.txt')
    cat('Log entry time', as.character(Sys.time()), '\n')
    cat(case, '\n')
    cat(i, ' of ', nrow(case_grds), '\n')
    print(Sys.time() - strt)
    sink()
      
    load('case_grds.RData')
    
    # create wt reg contour surface
    wtreg <- wtreg_fun(to_proc, wins = c(case_grds[i,]), 
      parallel = F, 
      progress = F)
    
    # save results for each window
    wtreg_nm <-paste0(case, '_wtreg_', i) 
    assign(wtreg_nm, wtreg)
    save(
      list = wtreg_nm,
      file=paste0(wtreg_nm, '.RData')
      )

    # clear RAM
    rm(list = c(wtreg_nm))
    
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
    DO_var = 'DO_nrm')
  
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