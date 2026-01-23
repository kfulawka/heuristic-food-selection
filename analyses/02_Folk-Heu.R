rm(list = ls())

library(data.table)
library(future.apply)
library(stringr)
library(dplyr)

source('heuristics_functions/99_heuristics.R')

# data --------------------------------------------------------------------

# metadata
source('analyses/00_metadat.R')

# maximum number of pairwise choices
N = 5e4

# eat wild fruits ---------------------------------------------------------

load("data/dat_eatWild.Rdata")
(table(dWild[dWild$C1=='F', c('quality', 'C1')]))

# RUN PAIRWISE COMPARISONS
eatWildFruits_heu = heu_data_apply(
  dat = dWild[dWild$C1 == 'F', ], #select fruits
  heu = 'quality',
  heu_sign = 1,
  nutr = nutr,
  N = N,
  f_subcat = 'C1', # all against all
  value_based = F
)

saveRDS(eatWildFruits_heu, file = 'results/rds_C12M/DE_wildFruits_50k_QH_n00_Inf.rds')

# eat wild meats ---------------------------------------------------------

load("data/dat_eatWild.Rdata")
table(dWild$C1[dWild$C1!='F'], dWild$quality[dWild$C1!='F'])

# RUN PAIRWISE COMPARISONS
eatWildMeat_heu = heu_data_apply(
  dat = dWild[dWild$C1 != 'F', ], #select meat
  heu = 'quality',
  heu_sign = 1,
  nutr = nutr,
  N = N,
  f_subcat = 'C1',
  value_based = F
)

saveRDS(eatWildMeat_heu, file = 'results/rds_C12M/DE_wildMeat_50k_QH_n00_Inf.rds')

# eat brown over white foods ---------------------------------------------------------

load("data/dat_eatWholemeal.Rdata")
table(dBrown$quality, dBrown$C1)

# RUN PAIRWISE COMPARISONS
eatBrown_heu = heu_data_apply(
  dat = dBrown,
  heu = 'quality',
  heu_sign = 1,
  nutr = nutr,
  N = N,
  f_subcat = 'C1', 
  value_based = F
)

saveRDS(eatBrown_heu, file = 'results/rds_C12M/DE_brown_50k_QH_n00_Inf.rds')

# eat wholemeal > brown > white bread ---------------------------------------------------------

load("data/dat_Bread.Rdata")
table(dBread$C12, dBread$quality)

# RUN PAIRWISE COMPARISONS
eatBread_heu = heu_data_apply(
  dat = dBread, 
  heu = 'quality',
  heu_sign = 1,
  nutr = nutr,
  N = N,
  f_subcat = 'C12', 
  value_based = F
)

saveRDS(eatBread_heu, file = 'results/rds_C12M/DE_breads_50k_QH_n00_Inf.rds')

# legs heuristic ----------------------------------------------------------

load("data/dat_Legs1v2v4.Rdata")
table(dLegs$HEU, dLegs$quality)

# RUN PAIRWISE COMPARISONS
legs_heu = heu_data_apply(
  dat = dLegs, 
  heu = 'quality',
  heu_sign = 1,
  nutr = nutr,
  N = N,
  f_subcat = 'HEU',
  value_based = F
)

saveRDS(legs_heu, file = paste0('results/rds_C12M/DE_legs124_50k_C1_QH_n00_Inf.rds'))

# RAW V PROCESSED ---------------------------------------------------------

load("data/dat_eatRaw.Rdata")
table(dRaw$C12M, dRaw$quality)

# RUN PAIRWISE COMPARISONS
raw_heu = heu_data_apply(
  dat = dRaw, 
  heu = 'quality',
  heu_sign = 1,
  nutr = nutr,
  N = N,
  f_subcat = 'C12M',
  value_based = F
)

saveRDS(raw_heu, file = paste0('results/rds_C12M/DE_RawFrHeu_50k_C12_QH_n00_Inf.rds'))