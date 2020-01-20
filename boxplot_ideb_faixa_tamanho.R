library(cagedExplorer)
source("dotplot_regions.R")

df_iniciais <- readRDS('data/df_dados_completos_anos_iniciais.rds')

glimpse(df_iniciais)


df_iniciais <- df_iniciais %>% 
  mutate(faixa_tamanho =
           cut(pop_ibge,
               breaks = c(0, 5e+3, 25e+3, 50e+3, 100e+3, 250e+3, 500e+3, 12106920),
               labels = c('Até 5 mil',
                          '5 mil a 25 mil',
                          '25 mil a 50 mil',
                          '50 mil a 100 mil',
                          '100 mil a 250 mil',
                          '250 mil a 500 mil',
                          'Mais de 500 mil'))
  )


df_iniciais %>% 
  group_by(faixa_tamanho) %>% 
  summarise(n = n(),
            media_ideb = mean(ideb),
            std = sd(ideb))


df_iniciais %>% 
  mutate(faixa_tamanho = fct_reorder(faixa_tamanho, pop_ibge, .fun = function(x) - mean(x, na.rm = TRUE))) %>% 
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
