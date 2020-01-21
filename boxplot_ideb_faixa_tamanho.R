library(cagedExplorer)
source("dotplot_regions.R")

df_iniciais <- readRDS('data/df_dados_completos_anos_iniciais.rds')

glimpse(df_iniciais)

df_iniciais %>% 
  group_by(faixa_tamanho) %>% 
  summarise(n = n(),
            media_ideb = mean(ideb),
            std = sd(ideb))


df_iniciais %>% 
  ggplot(aes(x = faixa_tamanho, y = ideb, fill = faixa_tamanho)) +
  geom_boxplot() +
  custom_theme() +
  coord_flip() +
  scale_fill_brewer(palette = 'Spectral') +
  labs(y = 'Média IDEB',
       x = 'Números de habitantes',
       title = 'Desempenho dos municípios paulistas no IDEB, por faixas de tamanho',
       subtitle = 'Média IDEB 2017 - Anos Iniciais do Ensino Fundamental') +
  theme(legend.position = 'none')

ggsave('boxplot_ideb_anos_iniciais_faixa_tamanho.png', height = 7, width = 7)
