plot_means <- function(m, trans = 'none'){
  means <- estimate_means(m, levels = 'red_list_status', transform = trans)
  m$model %>% 
    ggplot() +
    geom_violin(aes(red_list_status, `log10(Rank)`, fill = red_list_status)) +
    geom_jitter(aes(red_list_status, `log10(Rank)`), width = 0.05, alpha = 0.25, col = 'gray35') +
    geom_line(data = means, aes(red_list_status, Mean, group = 1), size = 1) +
    geom_pointrange(data = means, aes(red_list_status, Mean, ymin = CI_low, ymax = CI_high), size = 0.5, color = "black") +
    monkey_theme +
    theme(
      legend.position = 'none', 
      plot.title = element_text(hjust = 0.5)
    ) +
    xlab('IUCN conservation status') +
    ylab(expression('log'[10]*'(Rank)')) +
    scale_fill_manual(values = wes_palette('Zissou1', 5))
}

plot_means2 <- function(m, trans = 'none'){
  means <- estimate_means(m, levels = 'IUCN')
  m$model %>% 
    ggplot() +
    geom_violin(aes(`IUCN`, Rank, fill = `IUCN`)) +
    geom_jitter(aes(`IUCN`, Rank), width = 0.05, alpha = 1, col = 'gray35') +
    geom_line(data = means, aes(`IUCN`, Mean, group = 1), size = 1) +
    geom_pointrange(data = means, aes(`IUCN`, Mean, ymin = CI_low, ymax = CI_high), size = 0.5, color = "black") +
    monkey_theme +
    theme(
      legend.position = 'none', 
      plot.title = element_text(hjust = 0.5)
    ) +
    xlab('IUCN conservation status') +
    ylab(expression('log'[10]*'(Rank)')) +
    scale_fill_manual(values = wes_palette('Zissou1', 5))
} 
