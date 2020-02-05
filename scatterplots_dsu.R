library(cagedExplorer)

df_iniciais <- readRDS('data/df_dados_completos_anos_iniciais.rds')

# SCATTERPLOT: DESEMPENHO ADEQUADO VS DSU -------------------------------------
# FACETED: ~ DISCIPLINA
df_iniciais %>% 
  pivot_longer(cols = c(perc_adequado_mat, perc_adequado_port),
               names_to = 'Disciplina', names_prefix = 'perc_adequado_',
               values_to = 'perc_adequado') %>% 
  mutate(Disciplina = ifelse(Disciplina == 'mat', 'Matemática', 'Português')) %>% 
  ggplot(aes(x = doc_sup, y = perc_adequado)) +
  geom_point(alpha = 0.13, shape = 1) +
  geom_smooth(method = 'lm', col = 'darkred', fill = 'lightgray',
              se = TRUE, alpha = 0.8) +
  custom_theme() +
  scale_x_continuous(labels = function(x) paste0(x, '%'), breaks = seq()) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  labs(
    x = 'Percentual de docentes com curso superior',
    y = 'Percentual de alunos com desempenho adequado',
    title = 'Relação entre formação dos docentes e desempenho educacional',
    subtitle = 
      'Resultados dos municípios paulistas no SAEB 2017 - Anos Iniciais',
    caption = 
      'Fonte: Elaboração própria a partir de dados do INEP e do QEdu.\nNota: As áreas sombreadas correspondem ao intervalo de confiança de 95%.'
    ) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~ Disciplina)

# ggsave('plots/desempenho_adequado/scatterplot_desempenho_adequado_vs_dsu.png',
#        height = 6, width = 6)

# SCATTERPLOT: DESEMPENHO ADEQUADO VS DSU -------------------------------------
# WEIGHT: pop_ibge
# FACETED: ~ DISCIPLINA
df_iniciais %>% 
  pivot_longer(cols = c(perc_adequado_mat, perc_adequado_port),
               names_to = 'Disciplina', names_prefix = 'perc_adequado_',
               values_to = 'perc_adequado') %>% 
  mutate(Disciplina = ifelse(Disciplina == 'mat',
                             'Matemática', 'Português')) %>% 
  ggplot(aes(x = doc_sup, y = perc_adequado)) +
  geom_point(aes(size = pop_ibge), alpha = 0.2, shape = 1) +
  geom_smooth(aes(weight = pop_ibge), method = 'lm',
              col = 'darkred', fill = 'lightgray',
              se = TRUE, alpha = 0.8) +
  custom_theme() +
  scale_x_continuous(labels = function(x) paste0(x, '%'), breaks = seq()) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  labs(x = 'Percentual de docentes com curso superior',
       y = 'Percentual de alunos com desempenho adequado',
       title = 'Relação entre formação dos docentes e desempenho educacional',
       subtitle = 'Resultados dos municípios paulistas no SAEB 2017 - Anos Iniciais',
       caption = 'Notas:
       i) Elaboração própria a partir de dados do INEP e do QEdu;
       ii) Regressão linear ponderada pela população de cada município;
       iii) As áreas sombreadas correspondem ao intervalo de confiança de 95%.') +
  theme(panel.grid = element_blank()) +
  facet_wrap(~ Disciplina)

# ggsave('plots/desempenho_adequado/scatterplot_desempenho_adequado_vs_dsu_weighted.png',
#        height = 6, width = 6)

# VIOLIN PLOT: DSU VS FAIXA POP -----------------------------------------------
df_iniciais %>% 
  mutate(faixa_tamanho2 = 
           fct_reorder(faixa_tamanho2, pop_ibge,
                       .fun = function(x) mean(x, na.rm = TRUE))) %>% 
  ggplot(aes(x = faixa_tamanho2, y = doc_sup)) +
  geom_violin(aes(fill = faixa_tamanho2), alpha = 0.5)  +
  custom_theme() +
  theme(panel.grid = element_blank()) +
  stat_summary(fun.y=median, geom = "point", shape = 23) +
  scale_fill_brewer(palette = 'Dark2') +
  labs(x = 'Faixa populacional',
       y = 'Percentual de docentes com curso superior') +
  guides(fill = FALSE)

# AGREGANDO POR REGIOES -------------------------------------------------------
# Media simples
df_regioes <- df_iniciais %>% 
  group_by(regiao_governo) %>% 
  summarise(ideb = mean(ideb, na.rm = TRUE),
            saeb = mean(saeb, na.rm = TRUE),
            perc_adequado_avg = mean(perc_adequado_avg, na.rm = TRUE),
            perc_adequado_mat = mean(perc_adequado_mat, na.rm = TRUE),
            perc_adequado_port = mean(perc_adequado_port, na.rm = TRUE),
            matriculas_5ano = sum(matriculados_5ano, na.rm = TRUE),
            pop_ibge = sum(pop_ibge),
            nota_ird = mean(nota_ird),
            doc_sup = mean(doc_sup),
            pib_pc = sum(pib) / sum(pop_ibge))

# Ponderando medias por populacao dos municipios
media_pond <- partial(weighted.mean, na.rm = TRUE)

df_regioes_pond <- df_iniciais %>% 
  group_by(regiao_governo) %>% 
  summarise(ideb = media_pond(ideb, pop_ibge),
            saeb = media_pond(saeb, pop_ibge),
            perc_adequado_mat = media_pond(perc_adequado_mat, pop_ibge),
            perc_adequado_port = media_pond(perc_adequado_port, pop_ibge),
            nota_ird = media_pond(nota_ird, pop_ibge),
            doc_sup = media_pond(doc_sup, pop_ibge),
            matriculas_5ano = sum(matriculados_5ano, na.rm = TRUE),
            pop_ibge = sum(pop_ibge),
            pib_pc = sum(pib) / sum(pop_ibge))

# SCATTERPLOT: DESEMPENHO ADEQUADO VS DSU -------------------------------------
# FACETED: ~ DISCIPLINA
# SMOOTH: loess
df_regioes_pond %>% 
  pivot_longer(cols = c(perc_adequado_mat, perc_adequado_port),
               names_to = 'Disciplina', names_prefix = 'perc_adequado_',
               values_to = 'perc_adequado') %>% 
  mutate(Disciplina = ifelse(Disciplina == 'mat', 'Matemática', 'Português')) %>% 
  ggplot(aes(x = doc_sup, y = perc_adequado)) +
  geom_point(aes(size = pop_ibge), alpha = 0.7, shape = 1) +
  geom_smooth(method = 'loess', col = 'darkred', fill = 'lightgray',
              se = TRUE, alpha = 0.6) +
  custom_theme() +
  scale_x_continuous(labels = function(x) paste0(x, '%')) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  scale_size_continuous(name = 'Habitantes', breaks = c(1e+6, 5e+6, 1e+7, 2e+7),
                        labels = c('1 milhão', '5 milhões', '10 milhões', '20 milhões')) +
  labs(x = 'Percentual de docentes com curso superior',
       y = 'Percentual de alunos com desempenho adequado',
       title = 'Relação entre formação dos docentes e desempenho educacional',
       subtitle = 'Médias  das Regiões de Governo de São Paulo, SAEB 2017 - Anos Iniciais',
       caption = 'Fonte: Elaboração própria a partir de dados do INEP e do QEdu.\nNota: Médias ponderadas pela população dos municípios.') +
  theme(panel.grid = element_blank()) +
  facet_grid(~ Disciplina)

# ggsave('plots/desempenho_adequado/scatterplot_desempenho_adequado_vs_dsu_regioes.png',
#        height = 6, width = 6)

# SCATTERPLOT: IDEB VS DSU ----------------------------------------------------
# SMOOTH: loess
df_regioes_pond %>% 
  ggplot(aes(x = doc_sup, y = ideb)) +
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
    x = 'Percentual de docentes com curso superior',
    y = 'Nota IDEB 2017 - Anos Iniciais',
    title = 'Relação entre formação dos docentes e desempenho educacional',
    subtitle = 
      'Médias das Regiões de Gov. de SP, IDEB 2017 Anos Iniciais',
    caption = 'Notas:
    i) Elaboração própria a partir de dados do INEP e do IBGE;
    ii) Médias ponderadas pela população dos municípios;
    iii) A área sombreada corresponde ao intervalo de confiança de 95%.') +
  theme(panel.grid = element_blank())

# ggsave("plots/scatterplot_ideb_vs_dsu_regioes.png",
#        height = 6, width = 6)
