rm(list = ls())

# commerecially available food products
dat = read.csv('data/bls_clean/dProd.csv', sep = ';')

# meats
dd = dat[dat$C1 %in% c('T', 'U', 'V'), ]

# remove convenience foods
dd = dd[dd$C2 != 9, ]

# weird meet products..
dd = dd[dd$C12 != 'V8', ]

# rm shellfish and fish processed products
dd = dd[!dd$C12 %in% paste0('T', 7:9), ]

# ONE V TWO LEGS
d1 = dd
d1$quality = NA
d1$quality[d1$C1 == "T"] = 1 # Fish
d1$quality[d1$C12 %in% c('V3', 'V4', 'V6')] = -1 # Poultry and birds
d1$HEU = 'L12'
d1 = d1[!is.na(d1$quality), ]

# TWO V FOUR LEGS
d2 = dd
d2$quality = NA
d2$quality[d2$C12 %in% c('V3', 'V4', 'V6')] = 1 # Poultry and birds
d2$quality[d2$C1 == "U"] = -1 # 4 legs
d2$quality[d2$C12 %in% c('V1', 'V2', 'V5')] = -1 # GAME 4 LEGS
d2$HEU = 'L24'
d2 = d2[!is.na(d2$quality), ]

# ONE V FOUR LEGS
d3 = dd
d3$quality = NA
d3$quality[d3$C1 == "T"] = 1 # Fish
d3$quality[d3$C1 == "U"] = -1 # 4 legs
d3$quality[d3$C12 %in% c('V0', 'V1', 'V2', 'V5')] = -1 # GAME 4 LEGS
d3$HEU = 'L14'
d3 = d3[!is.na(d3$quality), ]

dLegs = rbind(d1, d2, d3)
rm(d1, d2, d3, dd)

# limit to raw / unprocessed
dLegs = dLegs[dLegs$C5 %in% 0:2, ]

save(dLegs, file = "data/dat_Legs1v2v4.Rdata")