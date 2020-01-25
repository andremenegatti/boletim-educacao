library(cagedExplorer)

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
  theme(panel.grid = element_blank()) +
  scale_fill_brewer(name = 'Prova', palette = 'Dark2') +
  scale_y_continuous(labels = function(x) formatC(x, digits = 3, big.mark = '.', decimal.mark = ',')) +
  scale_x_continuous(labels = function(x) paste0(x, '%')) +
  labs(x = 'Percentual de Alunos com Desempenho Adequado',
       y = 'Densidade',
       title = 'Percentual de Alunos da Rede Pública com Desempenho Adequado no SAEB',
       subtitle = 'Distribuição dos resultados dos municípios paulistas no SAEB 2017 - Anos Iniciais',
       caption = 'Fonte: Elaboração própria a partir de dados do QEdu.')
  
ggsave('plots/densidade_alunos_desempenho_adequado.png', height = 6, width = 7)
