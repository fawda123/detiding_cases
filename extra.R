# 
# ##
# # get tidal summaries
# files <- list.files('M:/wq_models/SWMP/raw/rproc/tide_preds/', pattern = 'R', 
#   full.names = T)
# file_ls <- list()
# for(file in files){
#   load(file)
#   nm <- gsub('.RData', '', basename(file))
#   cat(nm, '\t')
#   tmp <- get(nm)
#   file_ls[[nm]] <- tmp
#   }
# 
# daily_tide <- lapply(file_ls, 
#   function(x) {
#     x$jday <- format(x$DateTimeStamp, '%j')
#     out <- ddply(x,
#       .variable = 'jday',
#       .fun = function(y) diff(range(y$Tide, finite = T))
#       )
#     mean(out$V1)
#     }
#   )
# daily_tide <- melt(daily_tide)
# names(daily_tide) <- c('tide_rng', 'L1')
# 
# ##
# # get DO summaries
# 
# files <- list.files('M:/wq_models/SWMP/raw/rproc/proc5/', pattern = 'R', 
#   full.names = T)
# file_ls <- list()
# for(file in files){
#   load(file)
#   nm <- gsub('.RData', '', basename(file))
#   cat(nm, '\t')
#   tmp <- get(nm)
#   file_ls[[nm]] <- tmp
#   }
# 
# daily_do <- adply(
#   file_ls, 1, 
#   function(x) {
#     x$jday <- format(x$DateTimeStamp, '%j')
#     out <- ddply(x,
#       .variable = 'jday',
#       .fun = function(y){
#         if(sum(is.na(y$DO_mgl)) == length(y$DO_mgl)) NA
#         else diff(c(min(y$DO_mgl, na.rm = T), 
#           max(y$DO_mgl, na.rm = T)))
#         }
#       )
#     mean(out$V1, na.rm = T)
#     }, 
#   .progress = 'tk'
#   )
# 
# names(daily_do) <- c('L1', 'do_rng')
# 
# ##
# # air-sea exchange
# load('M:/wq_models/SWMP/raw/rproc/dat_nem.RData')
# 
# mean_d <- adply(dat.nem, 1,
#   .fun = function(x) mean(x$D, na.rm = T)
#   )
# names(mean_d) <- c('L1', 'mean_d')
# 
# ##
# # perc anom
# load('M:/wq_models/SWMP/raw/rproc/dat_nem.RData')
# 
# anoms <- adply(dat.nem, 1, anoms.fun)
# names(anoms) <- c('L1', 'Pg', 'Rt')
# 
# ##
# #comb all
# 
# to_plo <- merge(daily_tide, daily_do, by = 'L1')
# to_plo <- merge(to_plo, mean_d, by = 'L1')
# to_plo <- merge(to_plo, anoms, by = 'L1')
# to_plo<-melt(to_plo, id.var = 'L1')
# 
# # save
# site_sel <- to_plo
# save(site_sel, file = 'site_sel.RData')
# 
# #reassign factors for ranking in plot
# sort.val<-order(to_plo[to_plo$variable=='tide_rng','value'])
# to_plo$L1<-factor(to_plo$L1)
# to_plo$L1<-factor(
#   to_plo$L1,
#   levels=levels(to_plo$L1)[sort.val],
#   labels=levels(to_plo$L1)[sort.val]
#   )
# levs <- c('tide_rng', 'do_rng','mean_d', 'Pg', 'Rt')
# labs <- c('Mean tidal range', 'Mean DO range', 'Mean gas exchange', 
#   '% negative Pg', '% positive Rt')
# to_plo$variable<-factor(to_plo$variable,levels=levs,labels=labs)
# 
# p<-ggplot(to_plo,aes(x=L1,y=value,group=variable,fill=variable)) + 
#   geom_bar(stat='identity') + 
#   facet_wrap(~variable,ncol=1,scales='free_y') + 
#   scale_x_discrete(name=element_blank()) + 
#   theme_bw() + 
#   theme(
#     axis.text.x = element_text(angle = 90, vjust=0.5,hjust=1,size=7),
#     legend.position='none'
#     )
# print(p)


# low do/high tide, high do/low tide, low do/low tide, high do/low tide
cases <- c('KACHD', 'PDBBY', 'MARMB', 'WKBFR')

subs <- list(75000:76500, 60000:61500, 10000:11500, 88500:90000)
# subs <- c(1:100000)
# subs <- list(subs, subs, subs, subs)

# get tidal summaries
files <- list.files('M:/wq_models/SWMP/raw/rproc/tide_preds/', 
  pattern = paste(cases, collapse = '|'), 
  full.names = T)
file_ls <- list()
for(file in files){
  load(file)
  nm <- gsub('.RData', '', basename(file))
  cat(nm, '\t')
  tmp <- get(nm)[subs[[which(file == files)]], ]
  tmp$row <- subs[[which(file == files)]]
  file_ls[[nm]] <- tmp
  }

# for metab plots
dt_rngs <- lapply(file_ls, function(x) as.Date(range(x$DateTimeStamp)))

tide <- melt(file_ls, id.var = names(file_ls[[1]]))

to_plo1 <- tide
p1 <- ggplot(to_plo1, aes(x = DateTimeStamp, y = Tide,
    colour = L1)) + 
  geom_line() + 
  facet_wrap(~L1, scales = 'free_x', ncol = 1) + 
  scale_x_continuous(name=element_blank()) + 
  theme_bw() +
  theme(legend.position = 'none')

##
# get wq
files <- list.files('M:/wq_models/SWMP/raw/rproc/proc5/', 
  pattern = paste(cases, collapse = '|'), 
  full.names = T)
file_ls <- list()
for(file in files){
  load(file)
  nm <- gsub('.RData', '', basename(file))
  cat(nm, '\t')
  tmp <- get(nm)[subs[[which(file == files)]], c('DateTimeStamp', 'DO_mgl')]
  tmp$row <- subs[[which(file == files)]]
  file_ls[[nm]] <- tmp
  }

DO_mgl <- melt(file_ls, id.var = names(file_ls[[1]]))

to_plo2 <- DO_mgl
p2 <- ggplot(to_plo2, aes(x = DateTimeStamp, y = DO_mgl,
    colour = L1)) + 
  geom_line() + 
  facet_wrap(~L1, scales = 'free_x', ncol = 1) + 
  scale_x_continuous(name=element_blank()) + 
  theme_bw()  +
  theme(legend.position = 'none')

grid.arrange(p1, p2, ncol = 2)

##
# air-sea exchange, Pg, Rt
load('M:/wq_models/SWMP/raw/rproc/dat_nem.RData')

daily_met <- dat.nem[cases]
daily_met <- sapply(1:length(daily_met),
  function(x){
    
    dt_rng <- dt_rngs[[which(names(daily_met) == names(dt_rngs)[x])]]
    out <- daily_met[[x]]
    sel_vec <- out$Date >= dt_rng[1] & out$Date <= dt_rng[2]
    out[sel_vec, c('Date', 'D', 'Pg', 'Rt')]
    
    }, 
  simplify = F
  )
names(daily_met)<- cases
daily_met <- melt(daily_met, id.var = names(daily_met[[1]]))


to_plo3 <- daily_met
to_plo3 <- melt(to_plo3, id.var = c('Date', 'L1'))

p3 <- ggplot(to_plo3, aes(x = Date, y = value, group = L1, colour = L1)) + 
  geom_line(aes(linetype = variable)) + 
  facet_wrap(variable ~ L1, scales='free') + 
  scale_x_date(name=element_blank()) + 
  theme_bw() +
  theme(legend.position = 'none')

print(p3)

######
#

# low do/high tide, high do/low tide, low do/low tide, high do/low tide
cases <- c('KACHD', 'PDBBY', 'MARMB', 'WKBFR')
case <- cases[2]

load(paste0(case, '_prdnrm_td.RData'))
load(paste0(case, '_intgrd_td.RData'))

prdnrm <- get(paste0(case, '_prdnrm_td'))
int_grd <- get(paste0(case, '_intgrd_td'))

# load(paste0(case, '_prdnrm_dtd.RData'))
# load(paste0(case, '_intgrd_dtd.RData'))
# 
# prdnrm <- get(paste0(case, '_prdnrm_dtd'))
# int_grd <- get(paste0(case, '_intgrd_dtd'))

subs <- 7000:8000

to_plo <- prdnrm[subs,]
to_plo <- met.day.fun(to_plo, case)
to_plo$sunrise <- factor(to_plo$variable, levels = c('sunrise', 'sunset'), 
  labels = c('DO_nrm, day', 'DO_nrm, night'))

p1 <- ggplot(to_plo, aes(x = DateTimeStamp, y = DO_obs, colour = 'DO_obs')) + 
  geom_line() +
  geom_line(aes(y = DO_pred, colour = 'DO_pred'), size = 2) +
  geom_line(aes(y = DO_nrm, colour = sunrise, group = 1), size = 2) +
#   scale_y_continuous(limits = c(10, 14)) +
  theme_bw() + 
  theme(legend.title = element_blank(), legend.position = 'top')

p2 <- ggplot(to_plo, aes(x = DateTimeStamp, y = Tide, colour = sunrise, group = 1)) + 
  geom_line() +
  geom_line(aes(y = dTide)) +
  geom_point(aes(y = Depth)) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = 'top')

sim_grd <- int_grd[subs[1]:(10*subs[length(subs)]),]

p3 <- ggplot(sim_grd, aes(x = DateTimeStamp, y = factor(round(Tide,1)), 
    z = DO_pred, fill = DO_pred)) +
  geom_tile() + 
  scale_fill_gradientn(colours=cm.colors(3)) +
  scale_x_datetime(expand = c(0,0)) + 
  scale_y_discrete(expand = c(0,0)) +
  ylab('Tide') +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = 'top')

grid.arrange(p1, p2, p3, ncol = 1)

# get metabs
met_subs <- 1:nrow(prdnrm)
dat_met_obs <- nem.fun(prdnrm[met_subs, ], case, DO_var = 'DO_obs')
dat_met_prd <- nem.fun(prdnrm[met_subs, ], case, DO_var = 'DO_pred')
dat_met_nrm <- nem.fun(prdnrm[met_subs, ], case, DO_var = 'DO_nrm')

anoms.fun(dat_met_obs)
anoms.fun(dat_met_prd)
anoms.fun(dat_met_nrm)

to_plo1 <- melt(dat_met_obs, id.var = 'Date', 
  measure.var = c('NEM', 'Pg', 'Rt'))
to_plo2 <- melt(dat_met_prd, id.var = 'Date', 
  measure.var = c('NEM', 'Pg', 'Rt'))
to_plo3 <- melt(dat_met_nrm, id.var = 'Date', 
  measure.var = c('NEM', 'Pg', 'Rt'))

p1 <- ggplot(to_plo1, aes(x = Date, y = value, group = variable, 
    colour = variable)) +
  geom_line() +
  theme_bw()
p2 <- ggplot(to_plo2, aes(x = Date, y = value, group = variable, 
    colour = variable)) +
  geom_line() +
  theme_bw()
p3 <- ggplot(to_plo3, aes(x = Date, y = value, group = variable, 
    colour = variable)) +
  geom_line() +
  theme_bw()
grid.arrange(p1, p2, p3, ncol = 1)

pair_plo <- prdnrm[,c('Tide', 'dTide', 'DO_obs', 'DO_pred', 'DO_nrm')]
 
ggpairs(pair_plo)

######
# get summary plots of metab before/after

cases <- c('KACHD', 'PDBBY', 'MARMB', 'WKBFR')

met_ls <- vector('list', length = length(cases))
names(met_ls) <- cases

for(case in cases){

  load(paste0(case, '_prdnrm_td.RData'))
  load(paste0(case, '_intgrd_td.RData'))
  
  prdnrm <- get(paste0(case, '_prdnrm_td'))
  int_grd <- get(paste0(case, '_intgrd_td'))
  
  # get metabs
  dat_met_obs <- nem.fun(prdnrm, case, DO_var = 'DO_obs')
  dat_met_prd <- nem.fun(prdnrm, case, DO_var = 'DO_pred')
  dat_met_nrm <- nem.fun(prdnrm, case, DO_var = 'DO_nrm')
  met_out <- list(obs = dat_met_obs, prd = dat_met_prd, 
    nrm = dat_met_nrm)
  met_out <- llply(met_out,
    .fun = function(x) x[, !names(x) %in% c('DO_obs', 'DO_pred', 'DO_nrm')])
    
  met_out <- melt(met_out, id.var = names(met_out[[1]]))
  
  met_ls[[case]] <- met_out
    
  }

tmp <- llply(met_ls, melt)

to_plo <- melt(res_ls)
to_plo$L2 <- factor(to_plo$L2, levels = c('1','2','3'),
  labels = c('DO_obs', 'DO_prd', 'DO_nrm'))
to_plo$met <- rep(c('percent', 'Mean anom', 'Mean regs'), rep = 24)

ggplot(to_plo, aes(x = variable, y = value, fill = L2)) +
    geom_bar(stat = 'identity', position = 'dodge') + 
    facet_grid(met ~ L1)


######
#
# check weights

case <- 'WKBFR'

to_proc <- prep_wtreg(case)

wts <- wt_fun(to_proc[1500,], to_proc, all = T, wins = list(1, 12, NULL), 
  slice = F, subs_only = F)
to_plo <- melt(wts[1:3000,], id.var = 'DateTimeStamp')
ggplot(to_plo, aes(x = DateTimeStamp, y = value, colour = variable, 
  group = variable)) +
  geom_line() +
  facet_wrap(~variable) + 
  theme_bw()

######
# for each obs in each day, what  is frequency of tidal obs???
case <- 'WKBFR'

# get proc data
to_proc <- prep_wtreg(case)

load(paste0(case, '_prdnrm.RData'))
prdnrm <- get(paste0(case, '_prdnrm'))
ggplot(prdnrm, aes(x = Tide)) + 
  geom_histogram() + 
  facet_wrap(~ hour)

# get int grid
load(paste0(case, '_intgrd.RData'))

int_grd <- get(paste0(case, '_intgrd')) 
ggplot(int_grd, aes(x = DateTimeStamp, y = DO_pred, group = Tide,
  colour = Tide)) + 
  geom_point() + 
  theme_bw()

######
# weekly, monthly, seasonal aggregations of metab estimates

load('PDBBY_intgrd_td.RData')
load('PDBBY_prdnrm_td.RData')
grd_tmp <- PDBBY_intgrd_td
prd_tmp <- PDBBY_prdnrm_td

met_obs <- nem.fun(prd_tmp, 'PDBBY', 'DO_obs')
met_dtd <- nem.fun(prd_tmp, 'PDBBY', 'DO_nrm')

# combine results
col_sel <- c('Pg', 'Rt', 'NEM')
met_obs <- met_obs[, c('Station', 'Date', 'Tide', col_sel)]
met_dtd <- met_dtd[, col_sel]
names(met_dtd) <- c('Pg_dtd', 'Rt_dtd', 'NEM_dtd')
met_out <- cbind(met_obs, met_dtd)

nem_agg_fun <- function(nem_in){
  
  met_sub <- llply(nem_in, 
    .fun = function(x){
      
      # add week, month cols
      x$week <- format(x$Date, '%W')
      x$month <- format(x$Date, '%m')
      
      # seas col
      x$seas <- car::recode(x$month, "c('01','02','03') = 'W'; 
        c('04','05','06') = 'Sp'; 
        c('07','08','09') = 'Su'; 
        c('10','11','12') = 'F'")
      
      x
      
    })

  # aggregate by weekly, monthly, seasonal categories
  # for each case...
  agg_sum <- llply(met_sub, 
    .fun = function(x){
      
      # metabolism column names
      met_cols <- c('Pg', 'Rt', 'NEM', 'Pg_dtd', 'Rt_dtd', 'NEM_dtd')
      
      # melt by weekly, monthly, seasonal cats
      x <- melt(x, measure.var = c('week', 'month', 'seas'), 
        id.var = c('Date', met_cols)
        )
      
      agg_res <- dlply(x,
        .var = c('variable'), 
        .fun = function(y){
          
          out <- vector('list', length = length(met_cols))
          names(out) <- met_cols
          for(met in met_cols){
            
            tmp <- summarySE(y, measurevar = met, groupvars = 'value',
              narm = T) 
            names(tmp)[names(tmp) %in% met] <- 'mean'
            out[[met]] <- tmp
             
          }
          
          # combine metabolism categories 
          out <- melt(out, id.var = names(out[[1]]))
          names(out)[names(out) %in% 'L1'] <- 'var'
          out
          
        })
      
      # combine time categories
      agg_res <- melt(agg_res, id.var = names(agg_res[[1]]))
      names(agg_res)[names(agg_res) %in% 'L1'] <- 'cats'
      
      # separate var column into metabolis, obs/dtd
      agg_res$var[!grepl('dtd',agg_res$var)] <- paste0(
        agg_res$var[!grepl('dtd',agg_res$var)], '_obs')
        
      agg_res$sub_var <- gsub('^[A-Z,a-z]*_', '', agg_res$var)
      agg_res$var <- gsub('_[a-z]*$', '', agg_res$var)
      
      agg_res
      
    })
  
  return(agg_sum)
  
  }

agg_sum <- nem_agg_fun(list(met_out))

to_plo <- agg_sum[[1]]

p <- ggplot(to_plo, aes(x = factor(value), y = mean, group = sub_var,
    colour = sub_var)) +
  geom_line() + 
  facet_wrap(cats ~ var, scales = 'free') +
  theme_bw()

######

load('int_proc.RData')
load('prd_nrm.RData') # only rows 1:5000

##
# plot interp grid

to.plo <- int_proc

#interp grid removing extreme values
ylabs <- 'Pred. DO'
ggplot(to.plo, aes(x = DateTimeStamp, y = Tide)) + 
  geom_tile(aes(fill = DO_pred), expand = c(0,0)) +
  scale_fill_gradientn(
    colours = brewer.pal(11, 'Spectral')
    ) +
  theme_bw() +
  scale_x_datetime(
    name='Date',
    expand = c(0,0)
    ) + 
  scale_y_continuous(expand = c(0,0))

par(mfrow = c(2,1))
plot(DO_obs ~ DateTimeStamp, prd_nrm, type = 'l')
plot(DO_pred ~ DateTimeStamp, prd_nrm, type = 'l')
points(DO_nrm ~ DateTimeStamp, prd_nrm, type = 'p', 
  col = as.character(factor(prd_nrm$variable, levels = c('sunrise', 'sunset'),
    labels = c('orange', 'black'))), 
  pch = 16, cex = 0.5
  )

# old with dtd and mtd
load('M:/docs/manuscripts/swmp_noise/ELKVM_wtreg_1.RData')

windows()
old <- ELKVM_wtreg_1

plot(DO_prd ~ DateTimeStamp, old[1:5000,], type = 'l')
plot(DO_nrm ~ DateTimeStamp, old[1:5000,], type = 'l')
