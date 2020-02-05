library(tidyverse)

# Importando e juntando dados -------------------------------------------------
dados_demograficos <- 
  readxl::read_excel('data/dados_educa_pop_sp_2017.xls',
                     sheet = 1, range = 'B1:L646', col_names = TRUE) %>% 
  select(Codmun7, prop_jovens = prop00142017, prop_idosos = prop65m2017,
         prop_urb = propurb2017, pop_finbra)

df_iniciais <- readRDS('data/df_dados_completos_anos_iniciais.rds') %>% 
  select(-pop_finbra) %>% 
  left_join(dados_demograficos, by = 'Codmun7') %>% 
  mutate(prop_urb = ifelse(prop_urb > 1, 1, prop_urb),
         n_jovens = prop_jovens * pop_finbra,
         n_idosos = prop_idosos * pop_finbra,
         razao_jovens_idosos = n_jovens / n_idosos)

# Agregando dados por região --------------------------------------------------
df_reg <- df_iniciais %>% 
  group_by(regiao_governo) %>% 
  summarise(
    media_ideb = weighted.mean(ideb, w = matriculas, na.rm = TRUE),
    media_saeb = weighted.mean(saeb, w = matriculas, na.rm = TRUE),
    total_matriculas = sum(matriculas),
    total_pop = sum(pop_finbra),
    prop_urb = weighted.mean(prop_urb, w = pop_finbra, na.rm = TRUE),
    prop_jovens = sum(n_jovens) / sum(pop_finbra),
    prop_idosos = sum(n_idosos) / sum(pop_finbra),
    razao_jovens_idosos = sum(n_jovens) / sum(n_idosos)
    )

# Scatterplot Urbanização - Regiões -------------------------------------------
df_reg %>% 
  ggplot(aes(x = prop_urb * 100, y = media_ideb)) +
  geom_point(aes(size = total_pop), alpha = 0.5, shape = 1) +
  geom_smooth(method = 'lm', fill = 'lightgray', col = 'darkred') +
  cagedExplorer::custom_theme() +
  guides(size = FALSE) +
  theme(panel.grid = element_blank()) +
  scale_x_continuous(labels = function(x) str_c(x, '%')) +
  scale_y_continuous(labels = function(x) str_replace(x, '\\.', ',')) +
  labs(
    x = 'Taxa de urbanização',
    y = 'Média IDEB 2017 Anos Iniciais',
    title = 'Relação entre desempenho educacional e urbanização',
    subtitle = 'Médias das Regiões de Governo de SP',
    caption = 'Notas:
    i) Elaboração própria a partir de dados do INEP e Finbra;
    ii) Médias regionais do IDEB ponderadas por matrículas dos municípios (anos iniciais e rede pública);
    iii) Médias regionais de taxa de urbanização ponderadas pela população dos municípios;
    iv) A área sombreada representa o intervalo de confiança de 95%;
    v) A área dos círculos é proporcional à população de cada região.'
  )

ggsave('plots/outros/scatterplot_urbanização_regioes.png', height = 6.5, width = 6)

# Scatterplot Urbanização - Municípios ----------------------------------------
df_iniciais %>% 
  ggplot(aes(x = prop_urb * 100, y = ideb)) +
  geom_point(alpha = 0.3, shape = 1) +
  geom_smooth(method = 'lm', col = 'darkred', fill = 'lightgray') +
  cagedExplorer::custom_theme() +
  theme(panel.grid = element_blank()) +
  scale_x_continuous(labels = function(x) str_c(x, '%')) +
  scale_y_continuous(labels = function(x) str_replace(x, '\\.', ',')) +
  labs(
    x = 'Taxa de urbanização',
    y = 'Média IDEB 2017 Anos Iniciais',
    title = 'Relação entre desempenho educacional e urbanização',
    subtitle = 'Municípios paulistas',
    caption = 'Notas:
    i) Elaboração própria a partir de dados do INEP e Finbra;
    ii) A área sombreada representa o intervalo de confiança de 95%.'
  )

ggsave('plots/outros/scatterplot_urbanização_municipios.png',
       height = 6.5, width = 6)

# Scatterplot Proporção de Jovens - Regiões -----------------------------------
df_reg %>% 
  ggplot(aes(x = prop_jovens * 100, y = media_ideb)) +
  geom_point(aes(size = total_pop), alpha = 0.5, shape = 1) +
  geom_smooth(method = 'lm', fill = 'lightgray', col = 'darkred') +
  cagedExplorer::custom_theme() +
  guides(size = FALSE) +
  theme(panel.grid = element_blank()) +
  scale_x_continuous(labels = function(x) str_c(x, '%')) +
  scale_y_continuous(labels = function(x) str_replace(x, '\\.', ',')) +
  labs(
    x = 'Proporção de jovens na população total',
    y = 'Média IDEB 2017 Anos Iniciais',
    title = 'Relação entre desempenho educacional e proporção de jovens',
    subtitle = 'Médias das Regiões de Governo de SP',
    caption = 'Notas:
    i) Elaboração própria a partir de dados do INEP e Finbra;
    ii) Médias regionais do IDEB ponderadas por matrículas dos municípios (anos iniciais e rede pública);
    iii) A área sombreada representa o intervalo de confiança de 95%;
    iv) A área dos círculos é proporcional à população de cada região.'
  )

# ggsave('plots/outros/scatterplot_prop_jovens_regioes.png',
#        height = 6.5, width = 6)

# Scatterplot Urbanização - Municípios ----------------------------------------
df_iniciais %>% 
  ggplot(aes(x = prop_jovens * 100, y = ideb)) +
  geom_point(alpha = 0.3, shape = 1) +
  geom_smooth(method = 'lm', col = 'darkred', fill = 'lightgray') +
  cagedExplorer::custom_theme() +
  theme(panel.grid = element_blank()) +
  scale_x_continuous(labels = function(x) str_c(x, '%')) +
  scale_y_continuous(labels = function(x) str_replace(x, '\\.', ',')) +
  labs(
    x = 'Proporção de jovens na população',
    y = 'Média IDEB 2017 Anos Iniciais',
    title = 'Relação entre desempenho educacional e proporção de jovens',
    subtitle = 'Municípios paulistas',
    caption = 'Notas:
    i) Elaboração própria a partir de dados do INEP e Finbra;
    ii) A área sombreada representa o intervalo de confiança de 95%.'
  )

# ggsave('plots/outros/scatterplot_prop_jovens_municipios.png',
#        height = 6.5, width = 6)

# Scatterplot Razão Jovens/Idosos - Regiões -----------------------------------
df_reg %>% 
  ggplot(aes(x = razao_jovens_idosos, y = media_ideb)) +
  geom_point(aes(size = total_pop), alpha = 0.5, shape = 1) +
  geom_smooth(method = 'lm', fill = 'lightgray', col = 'darkred') +
  cagedExplorer::custom_theme() +
  guides(size = FALSE) +
  theme(panel.grid = element_blank()) +
  scale_x_continuous(labels = function(x) str_replace(x, '\\.', ',')) +
  scale_y_continuous(labels = function(x) str_replace(x, '\\.', ',')) +
  labs(
    x = 'Razão Jovens/Idosos',
    y = 'Média IDEB 2017 Anos Iniciais',
    title = 'Relação entre desempenho educacional e razão jovens/idosos',
    subtitle = 'Médias das Regiões de Governo de SP',
    caption = 'Notas:
    i) Elaboração própria a partir de dados do INEP e Finbra;
    ii) Médias regionais do IDEB ponderadas por matrículas dos municípios (anos iniciais e rede pública);
    iii) A área sombreada representa o intervalo de confiança de 95%;
    iv) A área dos círculos é proporcional à população de cada região.'
  )

# ggsave('plots/outros/scatterplot_razao_jovens_idosos_regioes.png',
#        height = 6.5, width = 6)

# Scatterplot Razão Jovens/Idosos - Municípios --------------------------------
df_iniciais %>% 
  ggplot(aes(x = razao_jovens_idosos, y = ideb)) +
  geom_point(alpha = 0.3, shape = 1) +
  geom_smooth(method = 'lm', col = 'darkred', fill = 'lightgray') +
  cagedExplorer::custom_theme() +
  theme(panel.grid = element_blank()) +
  scale_x_continuous(labels = function(x) str_replace(x, '\\.', ',')) +
  scale_y_continuous(labels = function(x) str_replace(x, '\\.', ',')) +
  labs(
    x = 'Razão Jovens/Idosos',
    y = 'Média IDEB 2017 Anos Iniciais',
    title = 'Relação entre desempenho educacional e razão jovens/idosos',
    subtitle = 'Municípios paulistas',
    caption = 'Notas:
    i) Elaboração própria a partir de dados do INEP e Finbra;
    ii) A área sombreada representa o intervalo de confiança de 95%.'
  )

# ggsave('plots/outros/scatterplot_razao_jovens_idosos_municipios.png',
#        height = 6.5, width = 6)
