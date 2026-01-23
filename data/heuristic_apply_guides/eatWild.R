# EAT WILD

rm(list = ls())

# Heuristic: Eat wild foods while you can

# This heuristic is about opting for wild foods over domestic foods 
# (because wild plants have more phytochemicals and wild animals and fish 
#   have lower saturated fats). 

# load data
dProd = read.csv('data/bls_clean/dProd.csv', sep = ';')

# FRUITS
dFruits = dProd[dProd$C1 == 'F', ]
# wild fruits are C4
dFruits$quality = ifelse(dFruits$C2 == 4, 1, -1) 


# MEATS
# we compare only game mammals (V2) v beef, veal, pork, mutton/lamb (U1357)
# and game birds (V3) v poultry (V4)
game_and_meat = c('V2', 'V3', 'V4', 'U1', 'U3', 'U5', 'U7')
dMeats = dProd[dProd$C12 %in% game_and_meat, ]
dMeats$quality = ifelse(dMeats$C12 %in% c('V2', 'V3'), 1, -1)

# finally, select only the products which in raw state
# incld deep-frozen (b/c game stuff is only in these)
# this makes the comparison most 'honest'
dMeats = dMeats[dMeats$C5 %in% 0:2, ]

# put into a single category for analyses
dMeats$C1 = ifelse(dMeats$C12 %in% c('V3', 'V4'), 'birds', 'mammals')


# 
dWild = rbind(dFruits, dMeats)
#
save(dWild, file = "data/dat_eatWild.Rdata")