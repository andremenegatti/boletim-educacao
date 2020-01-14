dotplot_regions <- function(df, var_plot, breaks = NA, palette = NA,
                            vline = 0, point_alpha = 0.6,
                            labs_x = NA, labs_y = NA,
                            labs_title = NA, labs_subtitle = NA,
                            labs_caption= NA) {
  
  var_plot <- enquo(var_plot)
  
  if (is.na(labs_x)) {
    labs_x = deparse(var_plot) %>%
      str_remove('^~') %>%
      str_remove_all('`')
  }
  
  if (is.na(labs_y)) {
    labs_y = 'Região de Governo'
  }
  
  df <- df %>%
    mutate(regiao_governo = fct_reorder(regiao_governo, !! var_plot)) 
  
  if (identical(breaks, NA) & identical(palette, NA))
  {
    plot <- ggplot(data = df) +
      geom_point(aes(x = !! var_plot, y = regiao_governo),
                 size = 5, shape = 21, fill = 'steelblue', alpha = point_alpha) +
      geom_vline(xintercept = vline, col = 'darkred', alpha = 0.5) +
      custom_theme() +
      theme(legend.position = 'none') +
      labs(x = labs_x, y = labs_y)  
  } else if (length(palette) == 1){
    plot <- ggplot(data = df) +
      geom_point(aes(x = !! var_plot, y = regiao_governo),
                 size = 5, shape = 21, fill = palette, alpha = point_alpha) +
      geom_vline(xintercept = vline, col = 'darkred', alpha = 0.5) +
      custom_theme() +
      theme(legend.position = 'none') +
      labs(x = labs_x, y = labs_y)    
  } else {
    df <- df %>%
      mutate(grupo_saldo = group_by_breaks(!! var_plot, breaks = breaks) %>%
               as.factor() %>% fct_reorder(!! var_plot))
    
    plot <- ggplot(data = df) +
      geom_point(aes(x = !! var_plot, y = regiao_governo, fill = grupo_saldo),
                 size = 5, shape = 21, col = 'gray', alpha = point_alpha) +
      geom_vline(xintercept = vline, col = 'darkred', alpha = 0.5) +
      scale_fill_manual(values = palette) +
      custom_theme() +
      theme(legend.position = 'none') +
      labs(x = labs_x, y = labs_y)  
  }
  
  plot <- plot +
    scale_x_continuous(labels = function(x) formatC(x, digits = 3,
                                                    big.mark = '.',
                                                   decimal.mark = ','))
  
  if (!is.na(labs_title)) {
    plot <- plot + labs(title = labs_title)
  }
  
  if (!is.na(labs_subtitle)) {
    plot <- plot + labs(subtitle = labs_subtitle)
  }
  
  if (!is.na(labs_caption)) {
    plot <- plot + labs(caption = labs_caption)
  }
  
  plot
}
