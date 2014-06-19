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

# do w/ dtide
for(case in cases){
   
  to_proc <- prep_wtreg(case)
  subs <- which(as.character(to_proc$DateTimeStamp) == '2011-01-01 00:00:00')
  subs <- subs:nrow(to_proc)
  to_proc <- to_proc[subs, ]
  
  # create wt reg contour surface
  int_proc <- interp_grd(to_proc, wins = list(4, 12, NULL), parallel = T,
    progress = T)
  
  # save interpolation grid
  int_nm <-paste0(case, '_intgrd_dtd') 
  assign(int_nm, int_proc)
  save(
    list = int_nm,
    file=paste0(case,'_intgrd_dtd.RData')
    )
  
  # get predicted, normalized from interpolation grid
  prd_nrm <- prdnrm_fun(int_proc, to_proc)
  
  # save predicted, normalized results
  prdnrm_nm <-paste0(case, '_prdnrm_dtd') 
  assign(prdnrm_nm, prd_nrm)
  save(
    list = prdnrm_nm,
    file=paste0(case,'_prdnrm_dtd.RData')
    )

  # clear RAM
  rm(list = c(int_nm, prdnrm_nm))
  
  }

# do w/ tide
for(case in cases){
   
  # prep case for wtreg
  to_proc <- prep_wtreg(case)
  subs <- which(as.character(to_proc$DateTimeStamp) == '2011-01-01 00:00:00')
  subs <- subs:nrow(to_proc)
  to_proc <- to_proc[subs, ]
  
  # create wt reg contour surface
  int_proc <- interp_td_grd(to_proc, wins = list(4, 12, NULL), parallel = T,
    progress = T)
  
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
Sys.time() - strt

# # contour
# 
# cont_plo <- dcast(int_proc, DateTimeStamp ~ dTide, value.var = 'DO_pred')
# 
# x.val <- cont_plo[,1]
# y.val <- as.numeric(names(cont_plo)[-1])
# z.val <- as.matrix(cont_plo[,-1])
# 
# filled.contour.hack(x.val, y.val, z.val, nlevels = 100, ylab = 'dTide (m)',
#   key.title = title(main = 'DO (mgl)', cex.main = 0.8, line = 1))
#  
# to_plo <- prdnrm
# 
# ggplot(to_plo, aes(x = DateTimeStamp, y = DO_obs)) + 
#  geom_point() +
#  geom_line(aes(y = DO_pred, colour = 'DO_pred'), size = 1.1) +
#  geom_line(aes(y = DO_nrm, colour = 'DO_nrm'), size = 1.1) +
#  theme_bw() + 
#  theme(legend.title = element_blank())
# 
# pair_plo <- to_plo[,c('Tide', 'dTide', 'DO_obs', 'DO_pred', 'DO_nrm')]
#  
# ggpairs(pair_plo)
# # predicted, normalized