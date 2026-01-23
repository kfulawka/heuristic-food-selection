heu_vis_dist = function(d,
                        da = NULL,
                        colors = c('seagreen', 'grey30', 'tomato'),
                        bg_alpha = .3,
                        strp_alpha = .8,
                        strp_col = c('grey95', 'grey50', 'grey30'),
                        interval_wd = .55,
                        med_col = 'white',
                        med_pch = 21,
                        med_size = 1,
                        subtitle = 'NQ',
                        dodge_wdth = .8,
                        xaxt_lim = 8,
                        xlab = 'Nutrient ratio',
                        show_t = .6,
                        title) {
  
  
  # add info on Ns
  if (subtitle == 'NQ') subtitle = paste0('N = ', format(sum(d$n[d$nutr_name == 'Energy']),
                                                         big.mark = ','))
  
  plt = ggplot(d,
               mapping = aes(y = nutr_name)) +
    geom_vline(xintercept = c(1/1.1, 1.1),
               color = 'grey20', 
               alpha = 1,
               lty = 1) +
    geom_linerange(aes(xmin = q1,
                       xmax = q7,
                       group = ii),
                   linewidth = interval_wd,
                   color = strp_col[1],
                   alpha = strp_alpha,
                   position = position_dodge(dodge_wdth))  +
    # geom_linerange(aes(xmin = q2,
    #                    xmax = q6),
    #                linewidth = interval_wd,
    #                color = strp_col[2],
    #                alpha = strp_alpha,
    #                position = position_dodge(dodge_wdth))  +
    geom_linerange(aes(xmin = q3,
                       xmax = q5,
                       group = ii),
                   linewidth = interval_wd,
                   color = strp_col[3],
                   alpha = strp_alpha,
                   position = position_dodge(dodge_wdth))  +
    geom_linerange(aes(xmin = lr_min,
                       xmax = lr_max,
                       color = lr_col,
                       group = ii),
                   linewidth = interval_wd,
                   alpha = bg_alpha,
                   position = position_dodge(dodge_wdth)) +
    geom_linerange(aes(xmin = rr_min,
                       xmax = rr_max,
                       color = rr_col,
                       group = ii),
                   linewidth = interval_wd,
                   alpha = bg_alpha,
                   position = position_dodge(dodge_wdth))  +
    geom_point(aes(x = q4, group = ii), 
               fill = med_col,
               shape = med_pch,
               size = med_size,
               position = position_dodge(dodge_wdth)) +
    ggtitle(title, subtitle = subtitle) +
    scale_x_continuous(xlab,
                       breaks = c(1/8, 1/4, 1/2, 1, 2, 4, 8),
                       labels = c('1/8', '1/4', '1/2', '1', '2', '4', '8'),
                       minor_breaks = NULL,
                       transform = 'log2',
                       expand = expansion(c(0, .1))) +
    scale_color_manual(name = 'Nutrient profile:',
                       values = c("good" = colors[1],
                                  "bad" = colors[3]),
                       labels = c('good' = 'healthier',
                                  'bad' = 'less healthy')) +
    facet_grid(rows = vars(nutr_name), 
               scales = 'free',
               space = 'free') +
    # guides(color = 'none') +
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_text(size = 10),
          axis.text = element_text(size = 10),
          plot.title = element_text(size = 12, face = 'bold'),
          plot.subtitle = element_text(size = 8, hjust = 0),
          strip.background = element_rect(linetype = NULL,
                                          colour = NA,
                                          fill = rgb(1, 1, 1, 0)),
          strip.text = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.spacing.y = unit(2, "pt"),
          plot.margin = margin(5, 9, 5, 10),
          legend.position = 'bottom',
          legend.title = element_text(size = 11),
          legend.text = element_text(size = 11)
    )  +
    coord_cartesian(xlim = c(1/xaxt_lim, xaxt_lim),
                    clip = 'off')
  
  if(!is.null(da)) {
    
    # filter 
    da = da[complete.cases(da), ]
    da = da[da$prop_show >= show_t, ]
    
    plt = plt + geom_text(data = da,
                          mapping = aes(y = nutr_name,
                                        x = xaxt_lim + .5,
                                        label = prop_show_l,
                                        col = change
                                        ),
                          size.unit = 'pt',
                          size = 9,
                          hjust = 0,
                          show.legend = F)
    
  }
  
  return(plt)
  
}