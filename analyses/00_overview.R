rm(list = ls())

library(data.table)
library(ggplot2)
library(future.apply)
library(patchwork)
library(stringr)
library(viridis)

# data --------------------------------------------------------------------

# commerecially available food products
dat = read.csv('data/bls_clean/dProd.csv', sep = ';')

# metadata
source('analyses/00_metadat.R')

# select the nutrients for presentation
nutr_cats # view for the row numbers
# select row numbers in order you want them on the figure

# axis settings
nutr_cats = data.frame(cbind(nutr_cats, scale = NA, unit = NA))

# 
nutr_cats$cols = c(rep('tomato', 4), rep('seagreen', 2), rep('seagreen', 2))

nutr_cats[nutr_cats[,'code'] == 'GCAL', c('scale', 'unit')] = c(1, 'kcal/100g')
nutr_cats[nutr_cats[,'code'] == 'VT', c('scale', 'unit')] = c(1e3, 'mg/100g')
nutr_cats[is.na(nutr_cats$scale), c('scale')] = 1e3
nutr_cats[is.na(nutr_cats$unit), c('unit')] = 'g/100g'

nutr_cats$scale = as.numeric(nutr_cats$scale)

#
nutr_cats[nutr_cats[,'code'] == 'FS', 'name'] = 'Saturated Fats'

#
fg_names[fg_names$cat_c=='K','short_name'] = 'Potatoes,\nRoots & Shrooms'

# lond data
dl = data.frame(melt(data.table(dat),
                id.vars = c('C1'), 
                measure.vars = nutr_cats$code,
                variable.name = 'code',
                value.name = 'nutr_val'))

# get the quantiles
dq = aggregate(nutr_val ~ code + C1, 
               data = dl,
               FUN = quantile,
               probs = c(.05, .25, .5, .75, .95))
dq = do.call(data.frame, dq); 
colnames(dq)[-c(1:2)] = paste0('q', 1:5)

# get the n
dn = aggregate(nutr_val ~ code + C1,
               data = dl, 
               FUN = length)
colnames(dn)[3] = 'n'

dq = merge(dq, fg_names)

#
dq = merge(dq, dn); rm(dn)

# figure ------------------------------------------------------------------

strp_col = c('grey80', 'grey30', 'white')
strp_alpha = .8; bg_alpha = .3
interval_wd = 5

plts = lapply(nutr_cats$code, function(nutr) {
  
  nc = nutr_cats[nutr_cats$code == nutr, ]
  
  d = dq[dq$code == nutr, ]
  d[,paste0('q',1:5)] = d[,paste0('q',1:5)]/nc$scale
  
  plt = ggplot(d,
               mapping = aes(y = short_name)) +
    geom_linerange(aes(xmin = q1,
                       xmax = q5,
                       linewidth = sqrt(n)),
                   # linewidth = interval_wd,
                   color = strp_col[1],
                   alpha = .5)  +
    geom_linerange(aes(xmin = q2,
                       xmax = q4,                       
                       linewidth = sqrt(n)),
                   # linewidth = interval_wd,
                   color = strp_col[2],
                   alpha = strp_alpha)  +
    geom_linerange(aes(xmin = q1,
                       xmax = q5,
                       linewidth = sqrt(n)),
                   color = nc$cols,
                   # linewidth = interval_wd,
                   alpha = bg_alpha) +
    geom_point(aes(x = q3),
               shape = 21,
               fill = strp_col[3]) +
    xlab(nc$unit) +
    ggtitle(nc$name) +
    guides(linewidth = 'none') +
    theme_bw() +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_text(size = 9),
          axis.text = element_text(size = 9),
          plot.title = element_text(size = 12, face = 'bold', hjust = .5),
          legend.position = 'bottom') 
  
  
  if(nutr == 'GMKO') plt = plt + coord_cartesian(xlim = c(0, 10)) + scale_x_continuous(breaks = seq(0, 10, 2))
  if(nutr == 'ZB') plt = plt + coord_cartesian(xlim = c(0, 30))
  if(nutr == 'VT') plt = plt + coord_cartesian(xlim = c(0, 100))
  
  return(plt)
  
}); names(plts) = nutr_cats$code

# source('99_legend.R')
# plts[['legend_plt']]  = stripe_legend_plot(title = 'Nutrient distribution') + 
#   theme(plot.title = element_blank())

#
des = c(
'ABCDEFGH'
)

fig1 = wrap_plots(plts, axes = 'collect_y') +
  plot_layout(design = des, heights = c(15, 1))
# fig1

# into PDF file
ggsave(paste0('figures/Fig1.pdf'),
       fig1,
       device = 'pdf',
       units = 'cm',
       width = 16,
       height = 3.5,
       scale = 2)