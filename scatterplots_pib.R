library(cagedExplorer)

df_iniciais <- readRDS('data/df_dados_completos_anos_iniciais.rds')

# SCATTERPLOT: % DESEMPENHO ADEQUADO VS PIB PER CAPITA ------------------------
# FACETED: ~ DISCIPLINA
df_iniciais %>% 
  pivot_longer(cols = c(perc_adequado_mat, perc_adequado_port),
               names_to = 'Disciplina', names_prefix = 'perc_adequado_',
               values_to = 'perc_adequado') %>% 
  mutate(Disciplina = ifelse(Disciplina == 'mat', 'Matemática', 'Português')) %>% 
  ggplot(aes(x = pib / pop_ibge,
           y = perc_adequado)) +
  geom_point(aes(size = pop_ibge), shape = 1, alpha = 0.2) +
  geom_smooth(se = FALSE, col = 'darkred', method = 'loess', alpha = 0.7) +
  facet_wrap(~ Disciplina) +
  scale_x_log10() +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  labs(x = 'PIB per capita em milhares de reais (escala logarítmica)',
       y = 'Percentual de alunos com desempenho adequado',
       title = 'Relação entre PIB per capita e desempenho educacional, por disciplina',
       subtitle = 'Resultados dos municípios paulistas no SAEB 2017 - Anos Iniciais',
       caption = 'Fonte: Elaboração própria a partir de dados do QEdu e do IBGE.\nNota: A área dos pontos é proporcional à população dos municípios.') +
  scale_color_manual(name = 'Faixa de população', values = RColorBrewer::brewer.pal(8, 'Dark2')) +
  guides(size = FALSE) +
  custom_theme() +
  theme(panel.grid = element_blank())

# ggsave('scatterplot_desempenho_adequado_vs_pib_per_capita.png', height = 6, width = 6.5)

# SCATTERPLOT: % SAEB VS PIB PER CAPITA ---------------------------------------
df_iniciais %>% 
  ggplot(aes(x = pib / pop_ibge,
             y = saeb)) +
  geom_point(aes(size = pop_ibge), shape = 1, alpha = 0.2) +
  geom_smooth(se = FALSE, col = 'darkred', method = 'loess', alpha = 0.7) +
  scale_x_log10() +
  labs(x = 'PIB per capita em milhares de reais (escala logarítmica)',
       y = 'Média SAEB 2017 - Anos Iniciais',
       title = 'Relação entre PIB per capita e desempenho educacional',
       subtitle = 'Resultados dos municípios paulistas no SAEB 2017 - Anos Iniciais',
       caption = 'Fonte: Elaboração própria a partir de dados do INEP e do IBGE.\nNota: A área dos pontos é proporcional à população dos municípios.') +
  scale_color_manual(name = 'Faixa de população', values = RColorBrewer::brewer.pal(8, 'Dark2')) +
  guides(size = FALSE) +
  custom_theme() +
  theme(panel.grid = element_blank())

# ggsave('scatterplot_saeb_vs_pib_per_capita.png', height = 6, width = 5.5)
