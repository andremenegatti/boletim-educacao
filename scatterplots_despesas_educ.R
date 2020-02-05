library(cagedExplorer)

df_iniciais <- readRDS('data/df_dados_completos_anos_iniciais.rds')

# SCATTERPLOT: % GASTOS EDUC VS POPULACAO -------------------------------------
ggplot(df_iniciais,
       aes(x = pop_ibge/1000, y = perc_despesas_educacao * 100, size = pop_ibge)) +
  geom_point(shape = 1, alpha = 0.5) +
  geom_smooth(se = TRUE, fill = 'lightgray', col = 'darkred', method = 'loess', alpha = 0.7) +
  custom_theme() +
  theme(panel.grid = element_blank()) +
  scale_color_manual(name = 'Faixa de população', values = RColorBrewer::brewer.pal(8, 'Dark2')) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  scale_x_log10() +
  guides(size = FALSE) +
  # scale_x_log10(labels = function(formatC(x, big.mark = '.', decimal.mark = ','))) +
  labs(x = 'Milhares de habitantes (escala logarítmica)',
       y = 'Despesas com educação (percentual do total) ',
       title = 'Relação entre gastos com educação e população',
       subtitle = 'Municípios paulistas, 2017 - Regressão não-paramétrica',
       caption = 'Notas:
       i) Elaboração própria a partir de dados da Finbra e do IBGE;
       ii) A área dos pontos é proporcional à população dos municípios;
       iii) A área sombreada corresponde ao intervalo de confiança de 95%.')

# ggsave('plots/scatterplot_gastos_educacao_vs_pop.png', height = 6, width = 6)

# SCATTERPLOT: IDEB VS % GASTOS EDUC, FACETED ~ FAIXA TAMANHO -----------------
df_iniciais %>% 
  mutate(faixa_tamanho2 = fct_reorder(faixa_tamanho2, pop_ibge, .fun = function(x) mean(x, na.rm = TRUE))) %>% 
  ggplot(aes(x = perc_despesas_educacao,
             y = ideb)) +
  geom_point(aes(col = faixa_tamanho2, size = pop_ibge), alpha = 0.2) +
  geom_point(aes(col = faixa_tamanho2, size = pop_ibge), shape = 1, alpha = 0.5) +
  geom_smooth(se = FALSE, col = 'darkred', method = 'lm', alpha = 0.8) +
  scale_x_log10(breaks = c(0.2, 0.3, 0.4), labels = c('20%', '30%', '40%')) +
  labs(x = 'Despesas com educação como percentual do total (escala logarítmica)',
       y = 'Média IDEB 2017 - Anos Iniciais',
       title = 'Relação entre despesas e desempenho educacional, por faixa populacional',
       subtitle = 'Resultados dos municípios paulistas no IDEB 2017 - Anos Iniciais',
       caption = 'Fonte: Elaboração própria a partir de dados do INEP, IBGE e Finbra.\nNota: A área dos pontos é proporcional à população dos municípios.') +
  # scale_color_brewer(name = 'Faixa de população', palette = 'Spectral') +
  scale_color_manual(name = 'Faixa de população', values = RColorBrewer::brewer.pal(8, 'Dark2')) +
  guides(size = FALSE) +
  custom_theme() +
  theme(panel.grid = element_blank(),
        legend.position = 'none',
        panel.spacing = unit(2, 'pt')) +
  facet_wrap(~ faixa_tamanho2, nrow = 2)

# ggsave('scatterplot_ideb_vs_despesas_educ_faceted.png', height = 6.5, width = 7)

# SCATTERPLOT: % DESEMPENHO ADEQUADO % GASTOS EDUC ----------------------------
# FACETED: DISCIPLINA ~ FAIXA TAMANHO
df_iniciais %>% 
  pivot_longer(cols = c(perc_adequado_mat, perc_adequado_port),
               names_to = 'Disciplina', names_prefix = 'perc_adequado_',
               values_to = 'perc_adequado') %>% 
  mutate(Disciplina = ifelse(Disciplina == 'mat', 'Matemática', 'Português')) %>% 
  mutate(faixa_tamanho2 = fct_reorder(faixa_tamanho2, pop_ibge, .fun = function(x) mean(x, na.rm = TRUE))) %>% 
  ggplot(aes(x = perc_despesas_educacao,
             y = perc_adequado)) +
  geom_point(aes(col = faixa_tamanho2, size = pop_ibge), alpha = 0.2) +
  geom_point(aes(col = faixa_tamanho2, size = pop_ibge), shape = 1, alpha = 0.5) +
  geom_smooth(se = FALSE, col = 'darkred', method = 'lm', alpha = 0.8) +
  facet_wrap(~ Disciplina) +
  scale_x_log10(breaks = c(0.2, 0.3, 0.4), labels = c('20%', '30%', '40%')) +
  scale_y_continuous(breaks = seq(0, 100, by = 20), labels = function(x) paste0(x, '%')) +
  labs(x = 'Despesas com educação como percentual do total (escala logarítmica)',
       y = 'Percentual de alunos com desempenho adequado',
       title = 'Relação entre despesas e desempenho educacional, por disciplina e faixa populacional',
       subtitle = 'Resultados dos municípios paulistas no SAEB 2017 - Anos Iniciais',
       caption = 'Fonte: Elaboração própria a partir de dados do QEdu, Finbra e IBGE\nNota: A área dos pontos é proporcional à população dos municípios.') +
  # scale_color_brewer(name = 'Faixa de população', palette = 'Spectral') +
  scale_color_manual(name = 'Faixa de população', values = RColorBrewer::brewer.pal(8, 'Dark2')) +
  guides(size = FALSE) +
  custom_theme() +
  theme(panel.grid = element_blank(),
        legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.spacing = unit(2, 'pt')) +
  facet_grid(Disciplina ~ faixa_tamanho2)

# ggsave('scatterplot_perc_adequado_vs_despesas_educ_faceted.png', height = 6.5, width = 8)
