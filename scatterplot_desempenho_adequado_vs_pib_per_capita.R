library(cagedExplorer)

df_iniciais <- readRDS('data/df_dados_completos_anos_iniciais.rds')

df_iniciais %>% 
  pivot_longer(cols = c(perc_adequado_mat, perc_adequado_port),
               names_to = 'Disciplina', names_prefix = 'perc_adequado_',
               values_to = 'perc_adequado') %>% 
  mutate(Disciplina = ifelse(Disciplina == 'mat', 'Matemática', 'Português')) %>% 
  ggplot(aes(x = pib / pop_ibge,
           y = perc_adequado)) +
  geom_point(aes(col = faixa_tamanho, size = pop_ibge), alpha = 0.3) +
  geom_point(aes(col = faixa_tamanho, size = pop_ibge), shape = 1, alpha = 0.6) +
  geom_smooth(se = FALSE, col = 'red') +
  facet_wrap(~ Disciplina) +
  scale_x_log10() +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  labs(x = 'PIB per capita em milhares de reais (escala logarítmica)',
       y = 'Percentual de alunos com desempenho adequado',
       title = 'Relação entre PIB per capita e desempenho educacional',
       subtitle = 'Resultados dos municípios paulistas no SAEB 2017 - Anos Iniciais',
       caption = 'Fonte: Elaboração própria a partir de dados do QEdu e do IBGE\nNota: A área dos pontos é proporcional à população dos municípios.') +
  # scale_color_brewer(name = 'Faixa de população', palette = 'Spectral') +
  scale_color_manual(name = 'Faixa de população', values = RColorBrewer::brewer.pal(8, 'Spectral')[2:8]) +
  guides(size = FALSE) +
  custom_theme() +
  theme(panel.grid = element_blank())
  # theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'gray90'))
  
ggsave('scatterplot_desempenho_adequado_vs_pib_per_capita.png', height = 6, width = 8)