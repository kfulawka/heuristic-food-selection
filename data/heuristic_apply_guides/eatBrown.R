rm(list = ls())

library(dplyr)

# Heuristic: The whiter the bread, the sooner you’ll be dead

# This heuristic is about white flour being unhealthy 
# (almost like sugar), and whole grains are healthy. 

# load data
dProd = read.csv('data/bls_clean/dProd.csv', sep = ';')

# white vs. brown vs. wholemeal breads
# white vs. brown

dat_whitebrown <- dProd %>% 
  
  #select only white and brown breads
  dplyr::filter(C12 %in% c("B2", "B3")) %>% 
  
  #create quality code and score brown subgroups as 1 and white subgroups as -1
  dplyr::mutate(quality = case_when(C12 %in% c("B2") ~ 1,
                                    C12 %in% c("B3") ~ -1,
                                    TRUE ~ NA_real_ )) %>% 
  
  #make the products be part of the same subgroup so that they are not compared against each other
  dplyr::mutate(C12 = "B23")

#save(dat_whitebrown, file = "DE_dat/dat_whitebrown.Rdata")
  
# brown vs. wholemeal

dat_brownwholemeal <- dProd %>% 
  
  #select only wholemeal and brown breads
  dplyr::filter(C12 %in% c("B2", "B1")) %>% 
  
  #create quality code and score wholemeal subgroups as 1 and brown subgroups as -1
  dplyr::mutate(quality = case_when(C12 %in% c("B1") ~ 1,
                                    C12 %in% c("B2") ~ -1,
                                    TRUE ~ NA_real_ )) %>% 
  
  #make the products be part of the same subgroup so that they are not compared against each other
  dplyr::mutate(C12 = "B12")

dat_wholemealwhite <- dProd %>% 
  
  #select only wholemeal and white breads
  dplyr::filter(C12 %in% c("B3", "B1")) %>% 
  
  #create quality code and score wholemeal subgroups as 1 and white subgroups as -1
  dplyr::mutate(quality = case_when(C12 %in% c("B1") ~ 1,
                                    C12 %in% c("B3") ~ -1,
                                    TRUE ~ NA_real_ )) %>% 
  
  #make the products be part of the same subgroup so that they are not compared against each other
  dplyr::mutate(C12 = "B13")

# save(dat_brownwholemeal, file = "DE_dat/dat_brownwholemeal.Rdata")

# merge these two dfs
dBread <- bind_rows(dat_whitebrown, dat_brownwholemeal, dat_wholemealwhite)

save(dBread, file = "data/dat_Bread.Rdata")


# deal with other products

dBrown <- dProd %>% 
  
  # remove breads that have been processed above, also other bread products, we only look at rolls
  dplyr::filter(!C12 %in% c("B1", "B2", "B3", "B6", "B7", "B8", "B9")) %>% 

  #rolls
  dplyr::mutate(quality = case_when(C12 %in% c("B4") ~ 1, #wholemeal rolls
                                    C12 %in% c("B5") ~ -1, #white rolls
                                    
                                    #pastas
                                    C12 %in% c("E5", "E7") ~ 1, #wholemeal pasta
                                    C12 %in% c("E4", "E6") ~ -1, #white pasta
                                    TRUE ~ NA_real_ )) %>%
  
  #create new C12s such that similar items are grouped together
  dplyr::mutate(C12 = case_when(C12 %in% c("B4") ~ "B45", #wholemeal rolls
                                       C12 %in% c("B5") ~ "B45", #white rolls
                                       
                                       C12 %in% c("E5", "E7") ~ "E4567", #wholemeal pasta
                                       C12 %in% c("E4", "E6") ~ "E4567", #white pasta
                                       TRUE ~ C12 )) %>%
   
    #select only subgroups that have not yet received a quality score
    #dplyr::group_by(C12) %>% 
    #mutate(select_subgroup = any(is.na(quality))) %>%
    
    #create quality code and score wholemeal products as 1 and white as -1
    dplyr::mutate(quality = case_when(
    !is.na(quality) ~ quality,
    grepl("whole grain|wholemeal", STE, ignore.case = TRUE) ~ 1,
    !grepl("whole grain|wholemeal", STE, ignore.case = TRUE) ~ -1,
    TRUE ~ NA_real_)) %>%
    
    #remove products that are left with NA
    dplyr::filter(!is.na(quality)) %>% 
    #remove column
    #dplyr::select(-select_subgroup) %>% 
  data.frame() # turn into df

# inspect which f_groups to keep
table(dBrown$C1, dBrown$quality)

# remove f_groups with no quality score
dBrown <- dBrown %>%
  dplyr::filter(C1 %in% c("B", "C", "D", "E")) 

# remove eggs
dBrown = dBrown[!dBrown$C12 %in% c('E1', 'E3'), ]

save(dBrown, file = "data/dat_eatWholemeal.Rdata")