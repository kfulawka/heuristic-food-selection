rm(list = ls())

library(data.table)
library(ggplot2)
library(future.apply)
library(patchwork)
library(stringr)

source('heuristics_functions/99_heuristics.R')

# data --------------------------------------------------------------------

# commerecially available food products
dat = read.csv('data/bls_clean/dProd.csv', sep = ';')

# rm subgroups with not enough products
C12_ft = table(dat$C12M)
#
df = dat[dat$C12M %in% names(C12_ft)[which(C12_ft >= 10)], ]
print( paste0(round( (100 - nrow(df)/nrow(dat) * 100), 2 ),
              '% data dropped / ',
              nrow(dat) - nrow(df), ' data points,') )

# metadata
source('analyses/00_metadat.R')

#
print( paste0('Unique food categoires = ', length(unique(df$C1)),
              '; Unique food subcategoires = ', length(unique(df$C12M)))
       )

# no of possible pairwise comparisons per subcat
n_pcs = sapply(unique(df$C12M), function(i) choose(sum(df$C12M==i), 2))
#
quantile(n_pcs, seq(0, 1, .1))
#
sort(n_pcs)

# NUTRIENT HEURISTICS ------------------------------------------------------

# 
NUTRIENT_HEURISTICS = list( 
  GCAL = list(v = -1, nm = 'energy'),
  FS = list(v = -1, nm = 'sat_fats'),
  # ZF = list(v = -1, nm = 'tot_fats'),
  KMD = list(v = -1, nm = 'sugar'),
  GMKO = list(v = -1, nm = 'salt'),
  ZE = list(v = 1, nm = 'protein'),
  ZB = list(v = 1, nm = 'fibre')
  )

# similar products = products in the same subcategory (C12M)
RES_GRPS = c(C1 = 'C1', C12 = 'C12M')[2]

# maximum number of pairwise choices
N = 5e4

#
lapply(RES_GRPS, function(C) {
  
  # APPLY ALL NUTRIENT HEURISTICS 
  lapply(names(NUTRIENT_HEURISTICS), function(H) {
    
    nm = NUTRIENT_HEURISTICS[[H]]$nm
    
    # RUN PAIRWISE COMPARISONS
    value_heu = heu_data_apply(
      dat = df,
      heu = H,
      heu_sign = NUTRIENT_HEURISTICS[[H]]$v,
      nutr = nutr,
      N = N,
      f_subcat = C,
      value_based = T
    )
    saveRDS(value_heu, file = paste0('results/rds_', C, '/DE_', nm, 
                                     '_', N/1e3,'k_VH_n00_Inf.rds'))
    
  })
  
})
