library(tidyverse)

df_iniciais <- readRDS('data/df_dados_completos_anos_iniciais.rds')

# Agregando regioes
media_pond <- partial(weighted.mean, na.rm = TRUE)

df_reg <- df_iniciais %>% 
  group_by(regiao_governo) %>% 
  summarise(media_ideb = media_pond(ideb, w = pop_ibge),
            media_municipalizacao = media_pond(municipalizacao, w = pop_ibge),
            pop_ibge = sum(pop_ibge))

# Plotting
df_reg %>% 
  ggplot(aes(x = media_municipalizacao * 100, y = media_ideb)) +
  geom_point(aes(size = pop_ibge),
             alpha = 0.7, shape = 1) +
  geom_smooth(method = 'lm', col = 'darkred',
              fill = 'lightgray', se = TRUE, alpha = 0.5) +
  custom_theme() +
  scale_x_continuous(labels = function(x) paste0(x, '%')) +
  scale_y_continuous(labels = 
                       function(x) formatC(x, big.mark = '.',
                                           decimal.mark = ',')) +
  scale_size_continuous(name = 'Habitantes',
                        breaks = c(1e+6, 5e+6, 1e+7, 2e+7),
                        labels = c('1 milhão', '5 milhões', '10 milhões', '20 milhões')) +
  labs(
    x = 'Taxa de Municipalização - Anos Iniciais',
    y = 'Nota IDEB 2017 - Anos Iniciais',
    title = 'Relação entre municipalização e desempenho educacional',
    subtitle = 
      'Médias das Regiões de Gov. de SP, IDEB 2017 Anos Iniciais',
    caption = 'Notas:
    i) Elaboração própria a partir de dados do INEP e do IBGE;
    ii) Médias ponderadas pela população dos municípios de cada região;
    iii) A área sombreada corresponde ao intervalo de confiança de 95%.') +
  theme(panel.grid = element_blank())

ggsave('plots/scatterplot_ideb_vs_municipalizacao.png', height = 6, width = 6)
