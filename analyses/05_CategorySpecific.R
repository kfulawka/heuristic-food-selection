# init --------------------------------------------------------------------

rm(list = ls())

library(data.table)

# metadata
source('analyses/00_metadat.R')
nutr_cats = data.frame(nutr_cats)
colnames(nutr_cats) = c('nutr', 'nutr_name')

nutrs = nutr_cats$nutr_name
names(nutrs) = nutrs

# SET names
h_names = c(
  energy = "Choose Less Energy",
  sugar = "Choose Less Sugar",
  salt = "Choose Less Salt",
  sat = "Choose Less Saturated Fats",
  fibre = "Choose More Fibre",
  protein = "Choose More Protein",
  RawFrHeu = "Choose Raw Foods",
  brown = "Choose Darker Grains",
  wild = 'Choose Wild Foods'
); 
heus = names(h_names)
names(heus) = heus


# data --------------------------------------------------------------------

dr_res = readRDS('results/heu_res.rds')

# add proper names to wild categories
dr_res$wild$C1 = factor(dr_res$wild$f_subcat,
                        levels = c('birds', 'mammals', 'F'),
                        labels = c('BRD', 'MML', 'F'),
                        ordered = T)

# aggregate ---------------------------------------------------------------

#
prop_agr = function(x) {
  
  p = mean(x)
  
  se = sqrt( (p*(1-p))/length(x) )
  
  r = c(p = p, 
        li = max(c(p - 1.96*se, 0)),
        ui = min(c(p + 1.96*se, 1)))
  
  return(r)
}

ests = lapply(heus, function(H) {
  
  d = dr_res[[H]]
  
  d = d[complete.cases(d), ]
  d$y = ifelse(d$nutr_ratio > 1, 1, 0)

  aa = aggregate(y ~ nutr + C1,
                 data = d,
                 FUN = prop_agr)
  #
  aa = do.call(data.frame, aa)
  colnames(aa)[3:5] = c('est', 'li', 'ui')
  #
  aa$nutr = factor(aa$nutr,
                   levels = nutr_cats$nutr[8:1],
                   labels = nutr_cats$nutr_name[8:1],
                   ordered = T)
  #
  return(aa)
  
}); names(ests) = names(heus)

# plts --------------------------------------------------------------------

library(ggplot2)
library(patchwork)

# set colors
meta_group_col = viridis::turbo(5, 1, .2, 1)
names(meta_group_col) = sort(unique(fg_names$meta_group))

# add extra groups for wild and brown heuristics
fg_add = data.frame(C1 = c('BRD', 'MML', 'B12', 'B13', 'B23'),
                    name = c('', '', '', '', ''),
                    short_name = c('Birds', 'Mammals', 
                                   'Whole-Brown', 'Whole-White', 'Brown-White'),
                    meta_group = c('Animals', 'Animals', 'Grains', 'Grains', 'Grains'))
fg_names = rbind(fg_names, fg_add); rm(fg_add)

plts = lapply(heus, function(H) {
  
  print(H)
  ff = ests[[H]]
  
  # add meta-group
  ff = merge(ff, fg_names[, c('C1', 'meta_group')])
  
  # 
  p = ggplot(data = ff,
             mapping = aes(x = est,
                           y = C1)) +
    geom_vline(xintercept = c(.5), 
               lty = 1,
               col = 'pink',
               alpha = .9) +
    # geom_linerange(mapping = aes(xmin = li,
    #                              xmax = ui,
    #                              col = meta_group),
    #                alpha = .25,
    #                lwd = .5) +
    geom_text(mapping = aes(label = C1,
                            col = meta_group),
              size = 2)+
    scale_color_manual('', values = meta_group_col) +
    scale_x_continuous('Prroportion of product choices with higher nutrient density',
                       limits = 0:1,
                       breaks = seq(0, 1, .5),
                       labels = c('0', '.5', '1'),
                       minor_breaks = seq(0, 1, .1)) +
    facet_wrap(~nutr, 
               nrow = 1,
               scales = 'free_y',
               strip.position = 'top') +
    ggtitle(h_names[H]) +
    theme_bw() +
    theme(axis.title.y = element_blank(),
          axis.text.y  = element_blank(),
          axis.text.x  = element_text(size = 7),
          axis.ticks.y = element_blank(),
          strip.background = element_blank(),
          strip.placement  = "outside",           
          strip.text.x.top = element_text(size = 8),
          strip.clip = "off",
          panel.grid.major.y = element_blank(),
          plot.title = element_text(size = 8, face = 'bold'))
  
  if(!H %in% c('energy','sugar')) p = p + guides(col = 'none')
  
  return(p)
  
})

ests_plt_A1 = wrap_plots(plts$energy, plts$fibre, plts$protein,
                         plts$RawFrHeu, plts$brown, plts$wild,
                         ncol = 1,
                         axis_titles = 'collect',
                         # axes = 'collect',
                         guides = 'collect',
                         heights = c(2, 1.5, 2, 1.3, 1.3, .75)
                         ) & theme(legend.position = 'bottom')
#
# ests_plt_A1

ggsave('figures/Fig-A2.pdf', 
       plot = ests_plt_A1,
       width = 16,
       height = 17,
       units = 'cm',
       scale = 1.25)

# ests_plt_A2 = wrap_plots(plts[c(2:4)], 
#                          ncol = 1,
#                          axis_titles = 'collect',
#                          guides = 'collect')&
#   theme(legend.position = 'bottom')
# #
# # ests_plt_A2
# 
# ggsave('DE_res/figs/Fig-A3.pdf', 
#        plot = ests_plt_A2,
#        width = 16,
#        height = 9.7,
#        units = 'cm',
#        scale = 1.25)