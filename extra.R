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
# for each obs in each day, what  is frequency of tidal obs???
case <- 'PDBBY'

# get proc data
to_proc <- prep_wtreg(case)
subs <- format(to_proc$DateTimeStamp, '%Y') %in% '2012'
to_proc <- to_proc[subs, ]

to_plo <- to_proc
ggplot(to_plo, aes(x = Tide)) + 
  geom_histogram(binwidth = diff(range(to_plo$Tide))/10) + 
  facet_wrap(~ hour)

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

case <- 'PDBBY'
i <- 1

load(paste0(case, '_intgrd_', i, '.RData'))
load(paste0(case, '_prdnrm_', i, '.RData'))

int_proc <- get(paste0(case, '_intgrd_', i))
prd_nrm <- get(paste0(case, '_prdnrm_', i))
  
##
# plot interp grid

to.plo <- int_proc

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

# prd and nrm ts
par(mfrow = c(3,1))
plot(Tide ~ DateTimeStamp, prd_nrm, type = 'l')
plot(DO_obs ~ DateTimeStamp, prd_nrm, type = 'l')
plot(DO_pred ~ DateTimeStamp, prd_nrm, type = 'l')
points(DO_nrm ~ DateTimeStamp, prd_nrm, type = 'p', 
  col = as.character(factor(prd_nrm$variable, levels = c('sunrise', 'sunset'),
    labels = c('orange', 'black'))), 
  pch = 16, cex = 0.5
  )

######
# figure of of DO/metab correlations before after, detiding
# note that tide in met_ls is daily average of hourly tidal change

# metab and inst flux data
load('met_ls.RData')
# load('met_ls_inst.RData')
load('case_grds.RData')

# go through each site for DO cors, use metab list for metab cors
case_regs <- list.files(getwd(), 'PDBBY_prdnrm_[0-9]*.RData')
cor_res <- alply(matrix(case_regs),
  1, 
  .fun = function(x){
  
    # load wtreg data
    load(x)
    nm <- gsub('.RData', '', x)
    dat_in <- get(nm)
      
    # DO obs v tide
    do_obs <- with(dat_in, 
      cor.test(DO_obs, Tide)
      )
    
    # DO dtd v tide
    do_dtd <- with(dat_in, 
      cor.test(DO_nrm, Tide)
      )
    
    # get tidal range for metabolic day/night periods from flux_in
    # for correlation with daily integrated metab
    tide_rngs <- ddply(dat_in, 
      .variables = c('met.date'),
      .fun = function(x){
#         sunrise <- suppressWarnings(diff(range(x[x$variable %in% 'sunrise', 'Tide'])))
#         sunset <- suppressWarnings(diff(range(x[x$variable %in% 'sunset', 'Tide'])))
#         if(sunrise == 'Inf') sunrise <- NA
#         if(sunset == 'Inf') sunset <- NA
#         daytot <- diff(range(x$Tide))
        sunrise <- mean(diff(x[x$variable %in% 'sunrise', 'Tide'], na.rm = T))
        sunset <- mean(diff(x[x$variable %in% 'sunset', 'Tide'], na.rm = T))
        if(sunrise == 'Inf') sunrise <- NA
        if(sunset == 'Inf') sunset <- NA
        daytot <- mean(diff(x$Tide, na.rm = T))
        
        c(daytot, sunrise, sunset)
        }
      )
    names(tide_rngs) <- c('Date','daytot', 'sunrise', 'sunset')
    
    # get metab data from list
    dat_in <- met_ls[[x]]
    dat_in <- merge(dat_in, tide_rngs, by = 'Date', all.x = T)
    
    # as list for all metab correlations
    # Pg values correlated with tidal range during sunlight hours
    # Rt values correlated with tidal range during night hours
    # NEM values correlated with metabolic daily tidal range
    met_cor <- list(
      
      Pg_obs = with(dat_in, 
        cor.test(Pg, sunrise)
        ),
    
      Rt_obs = with(dat_in, 
        cor.test(Rt, sunset)
        ),
    
      NEM_obs = with(dat_in, 
        cor.test(NEM, daytot)
        ),
    
      Pg_dtd = with(dat_in, 
        cor.test(Pg_dtd, sunrise)
        ),
    
      Rt_dtd = with(dat_in, 
        cor.test(Rt_dtd, sunset)
        ),
    
      NEM_dtd = with(dat_in, 
        cor.test(NEM_dtd, daytot)
        )
      
      )
    
    # DO and metab corrs combined
    all_ls <- c(do_obs = list(do_obs), do_dtd = list(do_dtd),
      met_cor)
    
    # convert the stats for each wtreg to data frame
    res_sum <- ldply(all_ls, 
      function(x) with(x, c(estimate, p.value))
      )
    names(res_sum) <- c('var', 'cor', 'pval')

    res_sum
    
  })
names(cor_res) <- case_regs

# melt and make separate columns for site and window comb value
cor_res <- melt(cor_res, id.var = names(cor_res[[1]]))  
cor_res$site <- gsub('_prdnrm_[0-9]*.RData', '', cor_res$L1)
cor_res$wins <- as.numeric(gsub('^.*_prdnrm_|.RData', '', cor_res$L1))

# merge with case_grds
case_grds$wins <- as.numeric(row.names(case_grds))
cor_res <- merge(cor_res, case_grds, by = 'wins', all.x = T)

# create columns for variable (DO, flux, etc.) and sub variable (obs, dtd)
cor_res$sub_var <- gsub('^.*_', '', cor_res$var)
cor_res$var <- gsub('_.*$', '', cor_res$var)

save(cor_res, file = 'cor_res.RData')

to_plo <- cor_res
to_plo$group_var <- paste(to_plo$Tide, to_plo$sub_var)
to_plo_obs <- to_plo[to_plo$sub_var %in% 'obs', ]
p1 <- ggplot(to_plo[to_plo$sub_var %in% 'dtd',], 
    aes(x = dec_time, y = cor, colour = Tide, group = group_var)) +
  geom_line() + 
  geom_line(data = to_plo_obs, 
    aes(x = dec_time, y = cor, group = group_var), 
    colour = 'black', size = 1) +
  geom_point(aes(pch = sub_var)) +
  facet_grid(hour ~ var) +
  ylim(c(-1, 1))

######
# fig

# load metabolism data
load('met_ls.RData')

# subset metab estimates by case and window comb
load('case_grds.RData')
sel_vec <- with(case_grds, which(dec_time == 2 & hour == 6 & Tide == 0.5))
sel_vec <- paste0('PDBBY', '_prdnrm_', sel_vec)

met_sub <- met_ls[grep(sel_vec, names(met_ls))]

# creat weekly, monthly, seasonal categories
met_sub <- llply(met_sub, 
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

to_plo <- agg_sum[[1]]

p <- ggplot(to_plo, aes(x = factor(value), y = mean, group = sub_var,
    colour = sub_var)) +
  geom_line() + 
  facet_wrap(cats ~ var, scales = 'free') +
  theme_bw()

