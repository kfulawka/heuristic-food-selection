rm(list = ls())

library(data.table)
library(ggplot2)
library(future.apply)
library(patchwork)
library(stringr)

# commerecially available food products
dat = read.csv('data/bls_clean/dProd.csv', sep = ';')

# categories, subcategories, no. of prods and choices ---------------------

# metadata
source('analyses/00_metadat.R')

# product data --- cats, subcats, no of products
prod_dat = aggregate(STE ~ C12M + C1, 
                     FUN = length,
                     data = dat)
#
colnames(prod_dat)[3] = 'n_prods'
# no of pairwise choices
prod_dat$n_choices = choose(prod_dat$n_prods, 2)

# add names
colnames(prod_dat)[1] = 'C12'
prod_dat = merge(prod_dat, fg_sc_names)
colnames(prod_dat)[5] = 'subcategory_name'
prod_dat = merge(prod_dat, fg_names[,c('C1', 'name')])
colnames(prod_dat)[6] = 'category_name'
#
colnames(prod_dat)[1:2] = c('category_code', 'subcategory_code')

# rearrange columns
prod_dat = prod_dat[,c(1:2,6,5,3,4)]

# save
write.table(prod_dat, 'figures/si_tabs/Tab02.csv', row.names = F, sep = ';')