# nutrient metadata
nutr = read.csv('data/bls_clean/nutr_metadat.csv', sep = ';')

filterd_categories = c('N', 'Q', 'P', 'R', 'X', 'Y')

# food categories names
fg_names = read.csv('data/bls_clean/food_category_names.csv', sep = ';')
fg_names = fg_names[!fg_names$C1 %in% filterd_categories, ]
fg_names$short_name = factor(fg_names$short_name,
                             levels = fg_names$short_name,
                             ordered = T)

# food subcategories
fg_sc_names = read.csv('data/bls_clean/food_subcategory_names.csv', sep = ';')
fg_sc_names = fg_sc_names[!fg_sc_names$C1 %in% filterd_categories, ]

# nutrient groups for analyses (ordered as desired on the figure)
# from the handbook
nutr_cats = c(
  GCAL = 'Energy',
  FS = 'Sat. Fats',
  KMD = 'Sugars', 
  GMKO = 'Salt',
  ZE = 'Protein',
  ZB = 'Fibre',
  VT = 'Vitamins', # manually added!
  ZM = 'Minerals'
  # FU = 'MUS FA',
  # FP = 'PUS FA',
  # FO3 = 'Omega-3',
  # FO6 = 'Omega-6',
)

#
nutr_cats = cbind(code = names(nutr_cats), 
                  name = nutr_cats)

rownames(nutr_cats) = 1:nrow(nutr_cats)