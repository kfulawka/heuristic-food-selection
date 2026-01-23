rm(list = ls())

library(data.table)
library(ggplot2)
library(future.apply)

dd = fread('data/BLS_3.02/BLS_3.02.txt',
           encoding = 'Latin-1')
dd = data.frame(dd)

# split code into columns
dd_codes = do.call(rbind, strsplit(dd$SBLS, split = ""))
colnames(dd_codes) = paste0("C", seq_len(ncol(dd_codes)))

# merge with codes
dd = cbind(dd_codes, dd); rm(dd_codes)

# clean a bit
dd$C7 = NULL; dd$SBLS = NULL; dd$ST = NULL

# ALSO ADDED TO NUTR METADATA! ---
# add total vitamins...
dd$VT = dd$VA + dd$VD + dd$VE + dd$VK + 
  dd$VB1 + dd$VB2 + dd$VB3 + dd$VB5 + dd$VB6 + dd$VB7 + dd$VB9G + dd$VB12 +
  dd$VC 
# excluded are VAR, VAC, VEAT, and VB3A

# and complex carbs index
dd$KP_O = rowSums(dd[,c('KP', 'KPOR')], na.rm = T)

# change MJ to MI (Iodine symbol is I!)
# also adjusted in nutr metadata!
colnames(dd)[which(colnames(dd) == 'MJ')] = 'MI'

# adjust the total mineral score so that it EXCLUDES sodium and chloride!
dd$ZM_r = dd$ZM
dd$ZM = dd$MK + dd$MCA + dd$MMG + dd$MP + dd$MS + 
  (( dd$MFE + dd$MZN + dd$MCU + dd$MMN + dd$MF + dd$MI) / 1000) 
  # + dd$MNA + dd$MCL

# subcategories of products
dd$C12 = paste0(dd$C1, dd$C2)

# MEALS -------------------------------------------------------------------

# data with meals (C2 == 9 is industrially produced ready meals)
meals = dd[dd$C1 %in% c('X', 'Y') | dd$C2 == 9, ]

# PRODUCTS ----------------------------------------------------------------

# data w/o meals
dd = dd[!(dd$C1 %in% c('X', 'Y')), ]

# only commercially available (groceries?)
dProd = dd[dd$C6 %in% 0:1, ]

# exclude oils, alcohol, spices, and beverages
dProd = dProd[!dProd$C1 %in% c('Q', 'P', 'R', 'N'), ]

# small subcategories merging ---------------------------------------------

table(dProd$C12)

# new variable 
dProd$C12M = dProd$C12

#
print( paste0(length(unique(dProd$C12)), ' of initial food subcategories') )

# D5 Cakes and tarts of special batter (n=8)
# D6 Pastries of special batter (n=67)
dProd$C12M[dProd$C12M %in% c('D5', 'D6')] = 'D56'

# E5 wholemeal pasta (n=6) -> E4 Pasta (n=24)
dProd$C12M[dProd$C12M %in% c('E4', 'E5')] = 'E45'

# E7 special wholemeal pasta (n=5)
# E6 Special Pasta (n=38)
dProd$C12M[dProd$C12M %in% c('E6', 'E7')] = 'E67'

# F8 fruit products (n=7)
# F9 Convenience food based on fruit (n=43)
dProd$C12M[dProd$C12M %in% c('F8', 'F9')] = 'F89'

# H0 other (n=9) -> H9 dishes (n=20)
dProd$C12M[dProd$C12M %in% c('H0', 'H9')] = 'H09'

# K3 Convenience food based on potatoes (n=6) 
# K5 Products of starchy plants (n=5)
# K9 Convenience food based on mushrooms (n=2)
dProd$C12M[dProd$C12M %in% c('K3', 'K5', 'K9')] = 'K359'

# # Q1 Vegetable oils, containing < 30 % linoleic acid (n=7)
# # Q2 Vegetable oils, containing 30 - 60 % linoleic acid (n=9)
# # Q3 Vegetable oils, containing > 60 % linoleic acid (n=5)
# dProd$C12M[dProd$C12M %in% c('Q1', 'Q2', 'Q3')] = 'Q123'
# 
# # Q7 animal oils (n=5) Q=8 animal fats (n=8)
# dProd$C12M[dProd$C12M %in% c('Q7', 'Q8')] = 'Q78'

# S6 Chocolate products, chocolates (n=54)
# S8 Confectioneries (n=8)
dProd$C12M[dProd$C12M %in% c('S6', 'S8')] = 'S68'

# V8 Meat products (n=2)
# V9 Convenience food based on game meat and poultry (n=11)
dProd$C12M[dProd$C12M %in% c('V8', 'V9')] = 'V89'

# W5 Meat products (n=11)
# W6 Cured meat (n=5)
dProd$C12M[dProd$C12M %in% c('W5', 'W6')] = 'W56'

print( paste0(length(unique(dProd$C12M)), ' of final food subcategories') )

# save files ---------------------------------------------------------------

# cleaned up data
write.table(dProd, 
            'data/bls_clean/dProd.csv', row.names = F,
            sep = ';')

# data for folk heuristics ------------------------------------------------

# script names
folk_dats = c('eatBrown.R', 'eatWild.R', 'Legs1v2v4.R', 'eatRaw.R')

for(i in folk_dats) source(paste0('data/heuristic_apply_guides/', i))

rm(list = ls())