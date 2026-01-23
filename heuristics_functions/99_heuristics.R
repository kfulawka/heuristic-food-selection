
# MAIN FUNCTIONS ----------------------------------------------------------

# heuristic product comparison
heuristic_single = function(p1, p2, heu, heu_sign = 1) {
  
  # p1 & p2: single-row data.frames / food products
  # heu: heuristic nutrient/quality variable name
  # heu_sign: 1 - more-is-better; -1 - less-is-better
  # output: NA, 'none', 'p1', 'p2'
  
  
  # heuristic value for each product
  heu_vals = c(p1[,heu], p2[,heu])
  
  # set heu_val sign (more / less is better depending on heu_sign)
  heu_vals = heu_vals * heu_sign
  
  
  # return NA if any of the heu_vals are NA
  if( any(is.na(heu_vals)) ) return(NA)
  
  # return 'none' if the heu_vals are equal
  if( heu_vals[1] == heu_vals[2] ) { return('none')
    
    # otherwise return which product is better according to heu_val
  } else { return( ifelse(heu_vals[1] > heu_vals[2], 'p1', 'p2') ) }
  
}

# nutrient comparison metric
nutr_comp = function(p1, p2, nutrs) {
  
  # p1 & p2: single-row data.frames / food products
  # nutrs: nutrient colnames for comparison
  # output: r - single-row data.frame with nutrient ratios
  
  # ratios
  r = p1[,nutrs] / p2[,nutrs]
  
  # rNaN = which(is.nan(unlist(r))) # note 0/0 = NaN
  # r[rNaN] = 1 # temporary substitution
  # 
  # # set (r = Inf) = 64 and (r = 0) = 1/64
  # r[, r == Inf] = 64; r[, r == 0] = 1/64
  # r[rNaN] = NA # the products do not differ!!!
  
  # OUTPUT
  return(r)
  
}


# comparison of product nutrients
# for a single pair of products p1 and p2
heu_comp_pairwise = function(p1, 
                             p2,
                             heu, 
                             heu_fun = heuristic_single,
                             heu_sign = 1,
                             nutrs) {
  
  # identify better product according to heu
  bp = heu_fun(p1, p2, heu, heu_sign)
  
  # if tie or NA
  if( is.na(bp) | bp == 'none') {
    
    r = data.frame(matrix(NA, ncol = length(nutrs)))
    
    # GET NUTRIENT DIFFERENCES IN FAVOR OF THE BETTER PROD
  } else if(bp == 'p1') { r = nutr_comp(p1, p2, nutrs)
  } else if(bp == 'p2') { r = nutr_comp(p2, p1, nutrs)
  }
  
  # RETURN 
  return(as.matrix(r))
  
}


# WRAPPERS AND HELPERS ----------------------------------------------------

# wrapper for nutrient heuristics
# sets up and runs ALL heuristic-based comparisons for a given data
heu_comp_data = function(dat,
                         heu, 
                         heu_sign = 1,
                         heu_fun = heuristic_single,
                         nutrs,
                         N = NULL,
                         value_based = F) {
  
  # dat: data.frame of food products
  # heu: nutrient/quality variable name
  # heu_sign: 1 - more-is-better; -1 - less-is-better
  # nutrs: colnames with nutrients
  # N: max number of pairwise comparisons
  # value_based: boolean, set to:
  # - T: value-based heuristic
  # - F: quality-based heuristic
  

  # SET UP COMPARISONS
  # if value-based take all possible pairwise comparisons
  if(value_based) { 

    comps = combn(1:nrow(dat), 2)
    
  # if quality-based, set comparisons between qualities  
  } else {
    
    q = sort( unique(dat[,heu]) )
    
    # 
    if(length(q) != 2) stop( 'ERROR: more or less than two heuristic qualities: [', 
                            paste(q, collapse = ', '),']' ) 
    
    # get indices of heu == 0 & heu == 1
    h0 = which(dat[,heu] == q[1]); h1 = which(dat[,heu] == q[2])
    
    # into pairwise comparison set
    comps = t(as.matrix( expand.grid(h0, h1) ))
    
  }
  
  
  # REDUCE THE NUMBER OF COMPARISONS IF TOO LARGE
  if( !is.null(N) ) {
    if( N < ncol(comps) ) {
      
      # take N random comparisons if ncol(comps) too large
      comps = comps[,sample(1:ncol(comps), N, replace = F)]
      
    }
  }
  
  
  # RUN THE PAIRWISE COMPARISONS
  plan(multisession)
  #
  rr = future_sapply(1:ncol(comps), function(i) {
    
    # products for comparison
    p1 = dat[comps[1,i], ] 
    p2 = dat[comps[2,i], ] 
    
    # apply the heuristic
    r = heu_comp_pairwise(p1, p2,
                          heu, 
                          heu_sign,
                          heu_fun = heu_fun,
                          nutrs)
    return(r)
    
  }); 
  plan(sequential)
  
  rownames(rr) = nutrs
  # OUTPUT
  return(rr)
  
}

# FINALL WRAPPER
# 1. APPLIES ALL OF THE ABOVE TO DATA WITH FOOD PRODUCTS, BY FOOD SUBCATEGORY
# 2. COMPUTES SUMMARY STATS
# 3. RETURNS RAW AND SUMMARIZED RESULTS MERGED WITH PROPER LABELS 
heu_data_apply = function(dat,
                          heu, 
                          heu_sign = 1,
                          heu_fun = heuristic_single,
                          nutr,
                          N = NULL,
                          value_based = F,
                          f_subcat) {
  
  # dat,
  # heu: heuristic variable name 
  # heu_sign: 1 - more-is-better; -1 - less-is-better
  # nutr: metadata for nutrients, with column nutr_c matching nutr codes
  # N: max number of pairwise comps 
  # f_subcat: category variable name
  
  
  # get subgroups
  f_subcats = unique(dat[[f_subcat]])  # Use double brackets to extract as vector
  f_sg_n = length(f_subcats)
  
  # LOOP OVER SUBGROUPS
  res = lapply(1:f_sg_n, function(i) {
    
    sc = f_subcats[i]
    dd = dat[dat[[f_subcat]] == sc, ]  # Use double brackets here as well

    # APPLY HEURISTIC TO EACH SUBGROUP
    r = heu_comp_data(dd, heu, heu_sign, heu_fun,
                      nutrs = nutr[,'nutr_c'],
                      N = N,
                      value_based = value_based)
    
    # add subcategory info to raw data
    r = data.frame(t(r)); r$f_subcat = sc
    
    # counter
    cat(paste0(round(i/f_sg_n, 3) * 100, '% --- ', sc, '\n') )
    
    # 
    return(r)
    
  })
  
  # OUTPUT
  dr = data.frame( rbindlist(res) )
  #
  return(dr)
  
}