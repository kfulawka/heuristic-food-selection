stripe_legend_plot <- function(strp_col = c("grey70", "grey40"),
                               med_col  = "white",
                               q = c(.1, .25, .5, .75, .9),
                               title = "Percentiles",
                               per_pos = 0.5,
                               interval_wd = 3) {
  
  # Dummy data for one stripe
  df <- data.frame(
    y  = 1,
    q1 = 0.10,   
    q2 = 0.25,   
    q3 = 0.50,
    q4 = 0.75,   
    q5 = 0.90 
  )
  
  # Border labels
  borders <- data.frame(
    x     = c(df$q1, df$q2, df$q3, df$q4, df$q5),
    y     = rep(df$y, ncol(df)-1 ),
    label = paste0( round(df[,paste0('q', 1:5)]*100), '%')
  )
  
  ggplot(df, aes(y = y)) +
    # outer 10–90 band
    geom_linerange(aes(xmin = q1, xmax = q5),
                   linewidth = interval_wd,
                   colour = strp_col[1]) +
    # inner 25–75 band
    geom_linerange(aes(xmin = q2, xmax = q4),
                   linewidth = interval_wd,
                   colour = strp_col[2]) +
    # median line
    geom_point(mapping = aes(x = q3),
               col = med_col) +
    geom_text(aes(x = q1, label = title),
              hjust = 1.1) +
    # border ticks
    # geom_segment(data = borders,ą
    #              aes(x = x, xend = x,
    #                  y = y - 0.12, yend = y + 0.12),
    #              linewidth = 0.4,
    #              inherit.aes = FALSE) +
    # labels
    geom_text(data = borders,
              aes(x = x, y = y - per_pos, label = label),
              inherit.aes = FALSE, size = 3) +
    coord_cartesian(xlim = c(0, 1),
                    ylim = c(0.6, 1.3),
                    clip = "off") +
    theme_void() +
    theme(
      plot.margin   = margin(2, 10, 2, 2)
    )
}