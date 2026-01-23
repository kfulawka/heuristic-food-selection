# init --------------------------------------------------------------------

rm(list = ls())

library(data.table)
library(stringr)

# metadata
source('analyses/00_metadat.R')
nutr_cats = data.frame(nutr_cats)
colnames(nutr_cats) = c('nutr', 'nutr_name')

#
RES_GRPS = c(C1 = 'C1', C12 = 'C12M')[2]

# data read and prep ------------------------------------------------------

dr_res = lapply(RES_GRPS, function(C) {
  
  f = list.files(paste0('results/rds_', C), full.names = F)
  
  r = lapply(f, function(ff) {
    
    # read in the results
    d = readRDS(paste0(paste0('results/rds_', C, '/'), ff))
    #
    # Choose nutrients of interest
    d = d[,c(nutr_cats[,1], 'f_subcat')]
    #
    d$C1 = substr(d$f_subcat, 1, 1)
    
    ############### DATA CLEANING ##################
    #
    # RM OILS
    d = d[d$C1 != 'Q', ]
    #
    # remove all meats and oil from evaluation of fibre and sugar heuristics
    if(grepl('fibre|sugar', ff)) {
      
      d = d[ !(d$C1 %in% c('T', 'U', 'V', 'W')), ]
      
    }
    # # remove dairy from fibre heuristic (?)
    # if(grepl('fibre', ff)) d = d[d$C1 != 'M', ]
    #
    # exclude sugars and fibre form meats and oil
    d[d$C1 %in% c('T', 'U', 'V', 'W'), c('KMD', 'ZB')] = NA
    #
    # exclude salt and saturated fats from fruits
    d[d$C1 %in% c('F'), c('GMKO', 'FS')] = NA
    #
    # filter ties
    df = d[apply(d[-9], 1, function(x) sum(is.na(x))!=8), ]
    
    # 
    print( paste0(gsub('DE_|(_.*0k.*)', '', ff), ' --- ',
                  round( (1 - (nrow(df)/nrow(d)))*100, 2), '% of ties' ) )

    
    # too long format
    dl = data.frame(melt(data.table(df),
                         id.vars = c('C1', 'f_subcat'),
                         variable.name = 'nutr',
                         value.name = 'nutr_ratio'))
    
    # add nutr names
    dl = merge(dl, nutr_cats)
    
    # nutrient groups
    dl$nutr_g = ifelse(dl$nutr_name %in% c('Protein', 'Fibre', 'Vitamins', 'Minerals'),
                       'positive', 'negative')
    
    # nutr name as ordered factor
    dl$nutr_name = factor(dl$nutr_name,
                          levels = nutr_cats$nutr_name,
                          ordered = T)
    
    return(dl)
    
  })
  names(r) = gsub('DE_|_.*0k.*', '', f)
  
  return(r)
  
}); dr_res = dr_res[[1]]

# combine breads with other grains
dr_res[['brown']] = rbind(dr_res$breads, dr_res$brown)
dr_res$breads = NULL

# set C1 as the results grouping variable
for(i in c('brown', 'legs124')) {
  dr_res[[i]]$C1 = dr_res[[i]]$f_subcat
}

# remove fibre and sugar from meat- heuristics
for(i in c('legs124', 'wildMeat')) {
  dr_res[[i]] = dr_res[[i]][!dr_res[[i]]$nutr %in% c('ZB', 'KMD'),]
}

# remove eggs from Raw heursitic
dr_res$RawFrHeu = dr_res$RawFrHeu[dr_res$RawFrHeu$f_subcat != 'E1',]

# combine wild meats with wild fruits
dr_res[['wild']] = rbind(dr_res$wildFruits, dr_res$wildMeat)
dr_res$wildMeat = NULL; dr_res$wildFruits = NULL

saveRDS(dr_res, paste0('results/heu_res.rds'))


# zeros and infinities ----------------------------------------------------

zis = lapply(names(dr_res), function(H) {

  dl = dr_res[[H]]
  #
  da = aggregate(nutr_ratio ~ 1,
                 data = dl,
                 FUN = function(x) {
                   c(
                     zeros = sum(x==0, na.rm = T)/length(x),
                     infinities = sum(is.infinite(x), na.rm = T)/length(x),
                     missings = sum(is.na(x))/length(x),
                     nrL18 = sum(x < 1/8, na.rm = T)/length(x),
                     nrM8 = sum(x > 8, na.rm = T)/length(x)
                     )
                 })
  da = do.call(data.frame, da)
  colnames(da) = c('Zs', 'Infs', 'NAs', 'L18', 'M8')
  da$I8 = 1 - (da$L18 + da$M8)
  da$H = H
  
  return(da)
  
}); zis = do.call(rbind, zis)

(zis[,1:6] = round(zis[,1:6] * 100, 2))

# QUANTILES ---------------------------------------------------------------

dq_list = lapply(names(dr_res), function(H, xl = 8, N = 100, t = .51) {
  
  dl = dr_res[[H]]
  
  # clip
  dl$nutr_ratio[dl$nutr_ratio > xl] = xl
  dl$nutr_ratio[dl$nutr_ratio < 1/xl]= 1/xl
  
  # get the n
  dn = aggregate(nutr_ratio ~ nutr_name + C1,
                 data = dl, 
                 FUN = length)
  colnames(dn)[3] = 'n'
  #
  print(paste0(H, ' --- ', 
               round(sum(dn$n < N)/nrow(dn)*100, 2),
               '% of dropped categories') )
  
  # get the quantiles
  dq = aggregate(nutr_ratio ~ nutr_name + nutr_g + C1, 
                 data = dl,
                 FUN = quantile,
                 probs = c(.1, .2, .25, .5, .75, .8, .9))
  dq = do.call(data.frame, dq); 
  colnames(dq) = gsub('\\.', '', colnames(dq))
  colnames(dq)[-c(1:3)] = paste0('q', 1:7)
  #
  dq = merge(dq, dn)
  dq = dq[dq$n > N, ]
  
  # variables to color green/red stripes
  dq$lr_col = ifelse(dq$nutr_g == 'positive', 'bad', 'good')
  dq$rr_col = ifelse(dq$nutr_g == 'positive', 'good', 'bad')
  
  dq$lr_min = ifelse(dq$q1 > 1, 1, dq$q1)
  dq$lr_max = ifelse(dq$q7 < 1, dq$q7, 1)
  dq$rr_min = ifelse(dq$q1 > 1, dq$q1, 1)
  dq$rr_max = ifelse(dq$q7 < 1, 1, dq$q7)
  
  # order nutr cat
  dq$nutr_name = factor(dq$nutr_name,
                        levels = nutr_cats[,2],
                        labels = nutr_cats[,2],
                        ordered = T)
  
  # # order food categories
  # dq = dq[order(dq$nutr_name, dq$q4), ]
  # 
  # dq$ii = factor(dq$C1,
  #                levels = dq$C1[grepl(substr(H, 1, 3), dq$nutr_name, T) ],
  #                ordered = T)
  # #
  # if(any(is.na(dq$ii))) {
    dq$ii = factor(dq$C1,
                   levels = sort(unique(dq$C1)),
                   ordered = T)
  # }
  
  # add percentage of 'significant' changes
  da = aggregate(nutr_ratio ~ nutr_name + nutr_g,
                 data = dl,
                 FUN = function(x) c(more1 = sum(x > 1)/length(x[x!=1]), 
                                     less1 = sum(x < 1)/length(x[x!=1])))
  da = do.call(data.frame, da)
  colnames(da) = gsub('nutr_ratio\\.', '', colnames(da))
  
  # Choose proportions for display
  da$prop_show = NA; da$change = NA
  for(i in 1:nrow(da)) {
    
    if(da$less1[i] > t) {
      
      da$prop_show[i] = da$less1[i]
      da$change[i] = ifelse(da$nutr_g[i] =='negative', 'good', 'bad')
      
    }
    
    if(da$more1[i] > t) {
      
      da$prop_show[i] = da$more1[i]
      da$change[i] = ifelse(da$nutr_g[i] =='negative', 'bad', 'good')
      
    }
    
  }
  da$prop_show_l = paste0(round(da$prop_show*100), '%')
  
  # print(H)
  
  return(list(dq = dq, da = da))
  
}); names(dq_list) = names(dr_res)

saveRDS(dq_list, paste0('results/heu_res_preped.rds'))