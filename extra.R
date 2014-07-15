#  massive summary plot of all SWMP sites
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

# subs <- list(75000:76500, 60000:61500, 10000:11500, 88500:90000)
subs <- c(100000:200000)
subs <- list(subs, subs, subs, subs)

# get tidal summaries
files <- list.files('M:/wq_models/SWMP/raw/rproc/tide_preds/', 
  pattern = paste(cases, collapse = '|'), 
  full.names = T)
file_ls <- list()
for(file in files){
  load(file)
  nm <- gsub('.RData', '', basename(file))
  cat(nm, '\t')
  tmp <- get(nm)[as.numeric(format(get(nm)$DateTimeStamp, '%Y')) == 2010
    & as.numeric(format(get(nm)$DateTimeStamp, '%m')) == 6,]#subs[[which(file == files)]], ]
#   tmp$row <- subs[[which(file == files)]]
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
  scale_x_datetime(name=element_blank()) + 
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
  tmp <- get(nm)[as.numeric(format(get(nm)$DateTimeStamp, '%Y')) == 2010 &
      as.numeric(format(get(nm)$DateTimeStamp, '%m')) == 6,c('DateTimeStamp', 'DO_mgl')]
#     subs[[which(file == files)]], c('DateTimeStamp', 'DO_mgl')]
#   tmp$row <- subs[[which(file == files)]]
  file_ls[[nm]] <- tmp
  }

DO_mgl <- melt(file_ls, id.var = names(file_ls[[1]]))

to_plo2 <- DO_mgl
p2 <- ggplot(to_plo2, aes(x = DateTimeStamp, y = DO_mgl,
    colour = L1)) + 
  geom_line() + 
  facet_wrap(~L1, scales = 'free_x', ncol = 1) + 
  scale_x_datetime(name=element_blank()) + 
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

# example plots 
cases <- c('PDBJE', 'RKBMB', 'SAPDC', 'TJRBR')
pdf('C:/Users/mbeck/Desktop/exs.pdf', height = 6, width = 10, family = 'serif')
for(case in cases){

load(paste0(case, '_wtreg.RData'))

prdnrm <- get(paste0(case, '_wtreg'))

subs <- 36000:37000#1:nrow(prdnrm)

to_plo <- prdnrm[subs,]
to_plo <- met.day.fun(to_plo, case)
to_plo$sunrise <- factor(to_plo$variable, levels = c('sunrise', 'sunset'), 
  labels = c('DO_dtd, day', 'DO_dtd, night'))

fmt <- function(){
    function(x) format(x, digits = 2)
}

p1 <- ggplot(to_plo, aes(x = DateTimeStamp, y = DO_obs, colour = 'DO_obs')) + 
  geom_line() +
  geom_line(aes(y = DO_prd, colour = 'DO_prd'), size = 1) +
  geom_line(aes(y = DO_nrm, colour = 'DO_nrm'), size = 1) +
  geom_line(aes(y = DO_dtd, colour = sunrise, group = 1), size = 1) +
  scale_y_continuous(labels = fmt()) +
  theme_bw() + 
  theme(legend.title = element_blank(), legend.position = 'top') +
  ggtitle(case)

to_plo$sunrise <- factor(to_plo$variable, levels = c('sunrise', 'sunset'), 
  labels = c('day', 'night'))
p2 <- ggplot(to_plo, aes(x = DateTimeStamp, y = Tide, group = 1, 
  colour = 'Predicted tide')) + 
  geom_line() +
#   geom_line(aes(y = dTide)) +
  geom_line(aes(y = Depth, colour = 'Observed height'), size = 1) +
  scale_y_continuous(labels = fmt()) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = 'top')

grid.arrange(p1, p2, ncol = 1)

}
dev.off()

##

######
# get summary plots of metab before/after

cases <- c('PDBJE', 'RKBMB', 'SAPDC', 'TJRBR')

met_ls <- vector('list', length = length(cases))
names(met_ls) <- cases

for(case in cases){

  load(paste0(case, '_wtreg.RData'))

  prdnrm <- get(paste0(case, '_wtreg'))
  
  # get metabs
  dat_met_obs <- nem.fun(prdnrm, case, DO_var = 'DO_obs')
  dat_met_dtd <- nem.fun(prdnrm, case, DO_var = 'DO_dtd')
  met_out <- list(obs = dat_met_obs, dtd = dat_met_dtd)

  met_ls[[case]] <- met_out
    
  }

save(met_ls, file = 'met_ls.RData')

# add cat variable for anom v non-anom
# calculate summary data for each category combo
to_plo <- llply(met_ls, 
  .fun = function(x) {
    
    names(x[[1]])[names(x[[1]]) %in% 'DO_dtd'] <- 'DO'
      
    to_eval <- melt(x, id.vars = names(x[[1]]))
    
    Pg_out <- summarySE(
      to_eval, measurevar = 'Pg', groupvars = c('L1'), narm = T
      )
    names(Pg_out)[names(Pg_out) %in% c('L1','Pg')] <- c('DO_typ', 'mean')
    Pg_out$var <- 'Pg'
    Rt_out <-summarySE(
      to_eval, measurevar = 'Rt', groupvars = c('L1'), narm = T
      )
    names(Rt_out)[names(Rt_out) %in% c('L1','Rt')] <- c('DO_typ', 'mean')
    Rt_out$var <- 'Rt'
    
    rbind(Pg_out, Rt_out)
      
    }
  )

to_plo <- melt(to_plo, id.var = names(to_plo[[1]]))
to_plo$DO_typ <- factor(to_plo$DO_typ, levels = c('obs', 'dtd'), 
  labels = c('Obs', 'Dtd'))

# metab summary
ggplot(to_plo, aes(x = var, y = mean, fill = DO_typ)) +  
  geom_bar(stat = 'identity', position = position_dodge(.9)) + 
  geom_errorbar(position = position_dodge(.9), width=.25, 
    aes(ymin = mean - sd, ymax = mean + sd)) +
  facet_grid( ~ L1) +
  theme_bw() +
  theme(legend.title = element_blank())

load('met_ls.RData')
# metabolism data
pdf('C:/Users/mbeck/Desktop/cases_metabs.pdf', height = 5, width = 8, 
  family = 'serif')
for(case in 1:length(met_ls)){
  
# get metabs
dat_met_obs <- met_ls[[case]][[1]]
dat_met_dtd <- met_ls[[case]][[2]]
  
to_plo1 <- melt(dat_met_obs, id.var = 'Date', 
  measure.var = c('NEM', 'Pg', 'Rt'))
to_plo2 <- melt(dat_met_dtd, id.var = 'Date', 
  measure.var = c('NEM', 'Pg', 'Rt'))

p1 <- ggplot(to_plo1, aes(x = Date, y = value, group = variable, 
    colour = variable)) +
  geom_line() +
  theme_bw() +
  ggtitle(names(met_ls)[case]) 
  
p2 <- ggplot(to_plo2, aes(x = Date, y = value, group = variable, 
    colour = variable)) +
  geom_line() +
  theme_bw()

grid.arrange(p1, p2, ncol = 1)
}
dev.off()

######

# DO boxplots by tidal quantile by site
cases <- c('PDBJE', 'RKBMB', 'SAPDC', 'TJRBR')
dat <- vector('list', length = length(cases))
names(dat) <- cases
for(case in cases){
  load(paste0(case, '_wtreg.RData'))
  dat[[case]] <- data.frame(case = case, get(paste0(case, '_wtreg')))
  }
dat <- llply(dat, 
  .fun = function(x){
    tide_quants <- quantile(x$Tide, probs = seq(0, 1, by = 0.25))
    x$Tide_q <- cut(x$Tide, tide_quants)
    x$Tide_q <- factor(x$Tide_q, levels = levels(x$Tide_q), 
      labels = seq(1, length(levels(x$Tide_q))))
    x
    }
  )
dat <- do.call('rbind', dat)
dat <- na.omit(melt(dat, id.vars = c('case', 'Tide_q'), 
  measure.vars = c('DO_dtd', 'DO_obs')))

ggplot(dat, aes(x = Tide_q, y = value, fill = variable)) + 
  geom_boxplot() + 
  facet_wrap(~case, scales = 'free_y')

##
# scatterplots of DO with tide using obs and dtd by month

cases <- c('PDBJE', 'RKBMB', 'SAPDC', 'TJRBR')

dat <- vector('list', length = length(cases))
names(dat) <- cases

for(case in cases){

  load(paste0(case, '_wtreg.RData'))

  prdnrm <- get(paste0(case, '_wtreg'))

  dat[[case]] <- prdnrm
    
  }

to_plo <- melt(dat, id.var = names(dat[[1]]))
to_plo$Month <- format(to_plo$DateTimeStamp, '%m')
to_plo <- melt(to_plo, id.var = c('L1', 'Month', 'Tide'), 
  measure.var = c('DO_obs', 'DO_dtd'))

to_plo <- to_plo#[sample(1:nrow(to_plo), 50000, replace = F), ]
ggplot(to_plo, aes(x = Tide, y = value, colour = variable)) + 
  geom_point(size = 0.1, alpha = 0.5) +
  stat_smooth(method = 'lm', colour = 'black') + 
  facet_grid(L1 + variable ~ Month, scales = 'free_y') + 
  theme_bw()


# ccf plots between tide and DO before/after detiding
cases <- c('PDBJE', 'RKBMB', 'SAPDC', 'TJRBR')
dat <- vector('list', length = length(cases))
names(dat) <- cases
for(case in cases){
  load(paste0(case, '_wtreg.RData'))
  dat[[case]] <- data.frame(case = case, get(paste0(case, '_wtreg')))
  }
pdf('C:/Users/mbeck/Desktop/cors_bf.pdf', height = 5,  width = 6,
  family = 'serif')
for(i in 1:length(dat)){

    x <- dat[[i]]
    nm <- names(dat)[i]
  
    par(mfrow = c(2, 1), mar = c(4.5,4.5, 3, 1))
    with(x, ccf(Depth, DO_obs, na.action = na.pass, main = nm))
    with(x, ccf(Depth, DO_dtd, na.action = na.pass))

    }
dev.off()


######
# quantify correlatoin of tide w/ DO for all sites
##
# get tidal summaries
files <- list.files('M:/wq_models/SWMP/raw/rproc/tide_preds/', pattern = 'R', 
  full.names = T)
files <- basename(files)
file_ls <- list()
for(file in files){
  
  nm <- gsub('.RData', '', file)
  cat(nm, '\t')
  
  load(paste0('M:/wq_models/SWMP/raw/rproc/tide_preds/', file))
  tmp <- get(nm)
  tide <- tmp
  
  load(paste0('M:/wq_models/SWMP/raw/rproc/proc5/', file))
  tmp <- get(nm)
  dat <- tmp
  
  out <- data.frame(dat, Tide = tide$Tide)
  
  file_ls[[nm]] <- out
  
  }

tmp <- ldply(file_ls, 
  .fun = function(x) {
    
    ccf_dat <- with(x, ccf(Depth, DO_mgl, na.action = na.pass, plot = F))
    out <- range(ccf_dat$acf)
    out
    
    })
head(tmp[order(tmp$V1, decreasing = T), ])
head(tmp[order(tmp$V2, decreasing = T), ])

pdf('C:/Users/mbeck/Desktop/depth_tide_cors.pdf', height = 6,  width = 5,
  family = 'serif')
for(i in 1:length(file_ls)){

    x <- file_ls[[i]]
    nm <- names(file_ls)[i]
  
    par(mfrow = c(4, 1), mar = c(4.5,4.5, 3, 1))
    ccf_dat <- with(x, ccf(Depth, DO_mgl, na.action = na.pass, plot = F))
    rng <- range(ccf_dat$acf)
    plot(ccf_dat, main = paste(c(nm, round(rng, 2)), collapse = ' '))
    
    strt <- which(!is.na(x$Depth))[1]
    subs <- strt:(strt + 5000)
    
    plot(DO_mgl ~ DateTimeStamp,  x[subs,], type = 'l')  
    plot(Tide ~ DateTimeStamp, x[subs, ], type = 'l')
    plot(Depth ~ DateTimeStamp, x[subs, ], type = 'l')

    }
dev.off()
subs <- 10000:11000
par(mfrow = c(3, 1))
plot(DO_mgl ~ DateTimeStamp,  x[subs,], type = 'l')  
plot(Tide ~ DateTimeStamp, x[subs, ], type = 'l')
plot(Depth ~ DateTimeStamp, x[subs, ], type = 'l')

######
# table of case study characteristics

##
# get amps (m) of dominant tidal constituents

files <- list.files('M:/wq_models/SWMP/raw/rproc/proc5/', 
  pattern = paste(cases, collapse = '|'), 
  full.names = T)
file_ls <- list()
for(file in files){
  load(file)
  nm <- gsub('.RData', '', basename(file))
  cat(nm, '\t')
  tmp <- get(nm)
  mod <- tidem(tmp$Depth, tmp$DateTimeStamp, 
    constituents = c('P1', 'O1', 'M2', 'S2'))
  const <- attr(mod, 'data')$amplitude[-1]
  names(const) <- attr(mod, 'data')$name[-1]
    
  file_ls[[nm]] <- const
  }
tide_comps<- data.frame(do.call('rbind', file_ls))
tide_comps$site <- rownames(tide_comps)

##
# get mean daily DO range

files <- list.files('M:/wq_models/SWMP/raw/rproc/proc5/', 
  pattern = paste(cases, collapse = '|'), 
  full.names = T)
file_ls <- list()
for(file in files){
  load(file)
  nm <- gsub('.RData', '', basename(file))
  cat(nm, '\t')
  tmp <- get(nm)
  file_ls[[nm]] <- tmp
  }

daily_do <- adply(
  file_ls, 1, 
  function(x) {
    x$jday <- format(x$DateTimeStamp, '%j')
    out <- ddply(x,
      .variable = 'jday',
      .fun = function(y){
        if(sum(is.na(y$DO_mgl)) == length(y$DO_mgl)) NA
        else {
          rng <- diff(c(min(y$DO_mgl, na.rm = T), 
            max(y$DO_mgl, na.rm = T)))
          avg <- mean(y$DO_mgl, na.rm = T)
          c(rng, avg)
          }
        }
      )
    colMeans(out[, -1], na.rm = T)
    }, 
  .progress = 'tk'
  )
names(daily_do) <- c('site', 'daily_rng', 'daily_avg')

##
# get metab summaries