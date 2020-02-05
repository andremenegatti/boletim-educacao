library(cagedExplorer)

df_iniciais <- readRDS('data/df_dados_completos_anos_iniciais.rds')

# Agregando dados por região --------------------------------------------------
df_reg <- df_iniciais %>% 
  group_by(regiao_governo) %>% 
  summarise(pib_total = sum(pib),
            pop_total = sum(pop_ibge),
            matriculas_total = sum(matriculas),
            media_ideb = weighted.mean(ideb, w = matriculas, na.rm = TRUE),
            media_saeb = weighted.mean(saeb, w = matriculas, na.rm = TRUE))

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
  geom_smooth(se = TRUE, col = 'darkred', fill = 'lightgray',
              method = 'loess', alpha = 0.7) +
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

# ggsave('plots/desempenho_adequado/scatterplot_desempenho_adequado_vs_pib_per_capita.png', height = 6, width = 6.5)

# SCATTERPLOT: SAEB VS PIB PER CAPITA - Municípios ----------------------------
df_iniciais %>% 
  ggplot(aes(x = pib / pop_ibge,
             y = saeb)) +
  geom_point(aes(size = pop_ibge), shape = 1, alpha = 0.3) +
  geom_smooth(se = TRUE, col = 'darkred', fill = 'lightgray',
              method = 'lm', alpha = 0.7) +
  scale_x_log10() +
  labs(x = 'PIB per capita em milhares de reais (escala logarítmica)',
       y = 'Média SAEB 2017 - Anos Iniciais',
       title = 'Relação entre PIB per capita e desempenho educacional',
       subtitle = 'Resultados dos municípios paulistas no SAEB 2017 - Anos Iniciais',
       caption = 'Notas:
       i) Elaboração própria a partir de dados do INEP e do IBGE;
       ii) A área dos pontos é proporcional à população dos municípios;
       iii) A área sombreada corresponde ao intervalo de confiança de 95%.') +
  scale_color_manual(name = 'Faixa de população', values = RColorBrewer::brewer.pal(8, 'Dark2')) +
  guides(size = FALSE) +
  custom_theme() +
  theme(panel.grid = element_blank())

# ggsave('plots/scatterplot_saeb_vs_pib_per_capita.png', height = 6, width = 5.5)

# SCATTERPLOT: IDEB VS PIB PER CAPITA - Municípios ----------------------------
df_iniciais %>% 
  ggplot(aes(x = pib / pop_ibge,
             y = ideb)) +
  geom_point(shape = 1, alpha = 0.3) +
  geom_smooth(se = TRUE, col = 'darkred', fill = 'lightgray',
              method = 'lm', alpha = 0.8) +
  scale_x_log10() +
  labs(x = 'PIB per capita em milhares de reais (escala logarítmica)',
       y = 'Média IDEB 2017 - Anos Iniciais',
       title = 'Relação entre PIB per capita e desempenho educacional',
       subtitle = 'Resultados dos municípios paulistas no IDEB 2017 - Anos Iniciais',
       caption = 'Notas:
       i) Elaboração própria a partir de dados do INEP e do IBGE;
       ii) A área sombreada corresponde ao intervalo de confiança de 95%.') +
  scale_color_manual(name = 'Faixa de população', values = RColorBrewer::brewer.pal(8, 'Dark2')) +
  custom_theme() +
  theme(panel.grid = element_blank())

# ggsave('plots/scatterplot_ideb_vs_pib_per_capita_municipios.png',
#        height = 6, width = 6)

# SCATTERPLOT: IDEB VS PIB PER CAPITA - Regiões -------------------------------
df_reg %>% 
  ggplot(aes(x = pib_total / pop_total,
             y = media_ideb)) +
  geom_point(aes(size = pop_total), shape = 1, alpha = 0.7) +
  geom_smooth(se = TRUE, col = 'darkred', fill = 'lightgray',
              method = 'lm', alpha = 0.6) +
  scale_x_log10() +
  labs(x = 'PIB per capita em milhares de reais (escala logarítmica)',
       y = 'Média IDEB 2017 - Anos Iniciais',
       title = 'Relação entre PIB per capita e desempenho educacional',
       subtitle = 'Médias das Regiões de Governo de SP, IDEB 2017 - Anos Iniciais',
       caption = 'Notas:
       i) Elaboração própria a partir de dados do INEP e do IBGE;
       ii) A área sombreada corresponde ao intervalo de confiança de 95%;
       iii) A área dos pontos é proporcional à população de cada região;
       iv) Médias regionais do IDEB ponderadas por matrículas em cada município (anos iniciais, rede pública)') +
  scale_color_manual(name = 'Faixa de população', values = RColorBrewer::brewer.pal(8, 'Dark2')) +
  custom_theme() +
  guides(size = FALSE) +
  theme(panel.grid = element_blank())

ggsave('plots/scatterplot_ideb_vs_pib_per_capita_regioes.png', height = 6, width = 6)
