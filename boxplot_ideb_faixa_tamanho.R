library(cagedExplorer)

df_iniciais <- readRDS('data/df_dados_completos_anos_iniciais.rds')

df_iniciais %>% 
  ggplot(aes(x = faixa_tamanho2, y = ideb, 
             fill = faixa_tamanho2)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 1) +
  custom_theme() +
  coord_flip() +
  scale_fill_manual(values = rev(RColorBrewer::brewer.pal(8, 'Dark2'))) +
  labs(y = 'Nota IDEB',
       x = 'Faixa populacional',
       title = 'Desempenho dos municípios paulistas no IDEB, por faixa populacional',
       subtitle = 'Média IDEB 2017 - Anos Iniciais do Ensino Fundamental',
       caption = 'Fonte: Elaboração própria a partir de dados do INEP e do IBGE.') +
  theme(legend.position = 'none',
        panel.grid = element_blank())

# ggsave('plots/boxplot_ideb_anos_iniciais_faixa_tamanho.png', height = 7, width = 7)