library(cagedExplorer)
source("dotplot_regions.R")

df_iniciais <- readRDS('data/df_dados_completos_anos_iniciais.rds')

glimpse(df_iniciais)

df_iniciais %>% 
  select(Codmun7, municipio_clean, perc_adequado_mat, perc_adequado_port, pop_ibge, regiao_governo) %>% 
  pivot_longer(cols = c(perc_adequado_mat, perc_adequado_port), names_to = 'disciplina',
               names_prefix = 'perc_adequado_', values_to = 'perc_adequado') %>% 
  mutate(disciplina = ifelse(disciplina == 'mat', 'Matemática', 'Português')) %>% 
  ggplot() +
  geom_density(aes(x = perc_adequado, fill = disciplina),
                 alpha = 0.5, position = 'identity') +
  custom_theme() +
  # theme(legend.position = 'bottom') +
  scale_fill_manual(values = c('deepskyblue', 'firebrick'), name = 'Prova') +
  scale_y_continuous(labels = function(x) formatC(x, digits = 3, big.mark = '.', decimal.mark = ',')) +
  scale_x_continuous(labels = function(x) paste0(x, '%')) +
  labs(x = 'Percentual de Alunos com Desempenho Adequado',
       y = 'Densidade',
       title = 'Percentual de Alunos com Desempenho Adequado no SAEB',
       subtitle = 'Distribuição dos resultados dos municípios paulistas no SAEB 2017 - Anos Iniciais')
  
ggsave('densidade_alunos_desempenho_adequado.png', height = 6, width = 7)




df_iniciais %>% 
  ggplot() +
  geom_density(aes(x = ideb),
               alpha = 0.5, position = 'identity') +
  custom_theme() +
  # theme(legend.position = 'bottom') +
  scale_fill_manual(values = c('deepskyblue', 'firebrick'), name = 'Prova') +
  scale_y_continuous(labels = function(x) formatC(x, digits = 3, big.mark = '.', decimal.mark = ',')) +
  scale_x_continuous(labels = function(x) paste0(x, '%')) +
  labs(x = 'Percentual de Alunos com Desempenho Adequado',
       y = 'Densidade',
       title = 'Percentual de Alunos com Desempenho Adequado no SAEB',
       subtitle = 'Distribuição dos resultados dos municípios paulistas no SAEB 2017 - Anos Iniciais')


library(RColorBrewer)
display.brewer.pal(n = 9, name = 'RdBu')
brewer.pal(n = 9, name = 'RdBu')
