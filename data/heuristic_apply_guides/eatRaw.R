rm(list = ls())

# commerecially available food products
dat = read.csv('data/bls_clean/dProd.csv', sep = ';')

# For foods in the groups F0 to F7, G0 to G7, H0 to H7, K1, K4, K7, T0 to T7, U0 to U8 
# (except U20, U40, U60, U80), and V0 to V7, the 5th position has the following meanings:
# •	0: Commercially available
# •	1: Raw, unprocessed
# •	2: Deep-frozen, frozen fillet
# •	3-0: various types of processing
xx = c(paste0('F', 0:7), paste0('G', 0:7), paste0('H', 0:7),
       'K1', 'K4', 'K7', paste0('T', 0:7), paste0('U', 0:8), paste0('V', 0:7))

# exclusions
dRaw = dat[dat$C12 %in% xx, ]
dRaw$C123 = paste0(dRaw$C1, dRaw$C2, dRaw$C3)
dRaw = dRaw[!dRaw$C123 %in% c('U20', 'U40', 'U60', 'U80'), ]
dRaw = dRaw[dRaw$C5 != 0, ]
# (2 includes deep-frozen)
dRaw$quality = ifelse(dRaw$C5 %in% 0:2, 1, -1)

# uncomment below to view frequencies
# table(dRaw$C12M, dRaw$quality)

# exclude subcategories with zero processed or unprocessed
dRaw = dRaw[!dRaw$C12M %in% c('G1', 'H09', 'V0', 'V1', 'V3', 'V6'),]

# exclude subcategories with only 1 unprocessed
dRaw = dRaw[!dRaw$C12M %in% c('V2', 'U0'),]

save(dRaw, file = "data/dat_eatRaw.Rdata")