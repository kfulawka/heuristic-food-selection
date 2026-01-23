# init --------------------------------------------------------------------

rm(list = ls())

library(data.table)
library(ggplot2)
library(ggforce)
library(patchwork)
library(stringr)

# metadata
source('analyses/00_metadat.R')
nutr_cats = data.frame(nutr_cats)
colnames(nutr_cats) = c('nutr', 'nutr_name')

# SET names
h_names = c(
  energy = "Choose Less Energy",
  sugar = "Choose Less Sugar",
  salt = "Choose Less Salt",
  sat_fats = "Choose Less Saturated Fats",
  fibre = "Choose More Fibre",
  protein = "Choose More Protein",
  tot_fats = 'Choose Less Fats',
  RawFrHeu = "Choose Raw Foods",
  brown = "Choose Darker Grains",
  wild = 'Choose Wild Foods',
  legs124 = 'Choose Fewer Legs'
  ); 
heus = names(h_names)

# RESULTS
# source('DE_03_NutrFolk-Results_prep.R')

# PLT ---------------------------------------------------------------------

# minor prep
dq_list = readRDS('results/heu_res_preped.rds')
names(dq_list)[grepl('sat', names(dq_list))] = 'sat_fats'
# names(dq_list)[grepl('tot', names(dq_list))] = 'tot_fats'

#
dq_list$wild$dq$ii = factor(dq_list$wild$dq$ii, 
                            levels = c('F', 'b', 'm'),
                            ordered = T)

source('analyses/99_legend.R')
source('heuristics_functions/99_heu_vis_dist.R')

plts = lapply(heus, function(H) {
  
  iw = .55; med_size = .75; dw = .8
  da = dq_list[[H]]$da
  
  if(H == 'RawFrHeu') iw = 1
  if(H == 'brown') iw = 1
  if(H %in% c('wild', 'legs124')) { iw = 2; med_size = 1.5 }
  
  p = heu_vis_dist(d = dq_list[[H]]$dq,
                   da = da,
                   colors = c('seagreen', 'grey30', 'tomato'),
                   bg_alpha = .3,
                   strp_alpha = .8,
                   strp_col = c('grey95', 'grey50', 'grey30'),
                   interval_wd = iw,
                   dodge_wdth = dw,
                   med_size = med_size,
                   title = h_names[H])
  
  if(!H %in% c('protein', 'sugar')) p = p + guides(color = 'none')
  
  return(p)
  
})
names(plts) = heus

# add guide areas and dist legend
plts[['col_legend']] = guide_area()
plts[['legend']] = stripe_legend_plot(title = 'Nutrient ratio distribution',
                                      per_pos = .3)

# Fig 2 -------------------------------------------------------------------

des = "
ABC
DEF
G#H
"

fig2 = wrap_plots(plts$energy, plts$fibre, plts$protein,
                  plts$RawFrHeu, plts$brown, plts$wild, 
                  plts$col_legend, plts$legend) +
  plot_layout(axes = 'collect_y',
              axis_titles = 'collect_x',
              guides = 'collect',
              design = des,
              heights = c(9, 7, 1)) &
  theme(legend.position = 'bottom')
fig2
# 
# into PDF file
ggsave(paste0('figures/Fig2.pdf'),
       fig2,
       device = 'pdf',
       units = 'cm',
       width = 16,
       height = 13,
       scale = 1.5)

# Fig A1 -------------------------------------------------------------------

des = "
ABC
D##
E#F
"

figA1 = wrap_plots(plts$sat_fats, plts$sugar, plts$salt,
                   plts$legs124, plts$col_legend, plts$legend) +
  plot_layout(axes = 'collect_y',
              axis_titles = 'collect_x',
              guides = 'collect',
              design = des,
              heights = c(9, 7,1)) &
  theme(legend.position = 'bottom')
figA1
# 
# into PDF file
ggsave(paste0('figures/Fig-A1.pdf'),
       figA1,
       device = 'pdf',
       units = 'cm',
       width = 16,
       height = 13,
       scale = 1.5)