library(cagedExplorer)

# Estimativa de populacao em 2017
pop2017 <- readxl::read_excel('estimativa_populacao_municipios_2017.xlsx', sheet = 1,
                              range = 'A1:C5571', col_names = TRUE) %>% 
  filter(str_detect(Codmun7, '35\\d{5}')) %>% 
  select(-municipio) %>% 
  mutate(Codmun7 = as.integer(Codmun7))

# Percentual de docentes com curso superior
dsu2017 <- readxl::read_excel('DSU_2017.xlsx', sheet = 1, range = 'A1:E646', col_names = TRUE) %>% 
  select(-municipio)

# Lendo dados IDEB Anos Iniciais
ideb_iniciais <- readRDS('df_ideb_iniciais.rds')

# Lendo dados IDEB Anos Iniciais
ideb_medio <- readRDS('df_ideb_medio.rds')

# Joining data
df_municipios <- municipios_sp %>% 
  inner_join(pop2017, by = 'Codmun7') %>% 
  inner_join(dsu2017, by = 'Codmun7') %>% 
  inner_join(ideb_iniciais %>% 
              filter(rede == 'Pública') %>% 
              select(Codmun7, taxa_aprov_iniciais_2017, ind_rend_iniciais_2017,
                     saeb_iniciais_2017, ideb_iniciais_2017),
            by = 'Codmun7') %>%
  left_join(ideb_medio %>% 
              filter(rede == 'Pública') %>% 
              select(-rede),
            by = 'Codmun7') %>% 
  select(-regiao_metropolitana, -aglomeracao_urbana, -pop)

# Agregando por regioes
df_medias <- df_municipios %>% 
  group_by(regiao_governo) %>% 
  summarise(pop = sum(pop2017),
            perc_doc_sup = mean(DSU_F14),
            ideb_2017 = mean(ideb_iniciais_2017, na.rm = TRUE),
            saeb_2017 = mean(saeb_iniciais_2017, na.rm = TRUE),
            rend_2017 = mean(ind_rend_iniciais_2017, na.rm = TRUE),
            aprov_2017 = mean(taxa_aprov_iniciais_2017, na.rm = TRUE),
            perc_doc_sup_pond = weighted.mean(DSU_F14, w = pop2017, na.rm = TRUE),
            ideb_2017_pond = weighted.mean(ideb_iniciais_2017, w = pop2017, na.rm = TRUE),
            saeb_2017_pond = weighted.mean(saeb_iniciais_2017, w = pop2017, na.rm = TRUE),
            rend_2017_pond = weighted.mean(ind_rend_iniciais_2017, w = pop2017, na.rm = TRUE),
            aprov_2017_pond = weighted.mean(taxa_aprov_iniciais_2017, w = pop2017, na.rm = TRUE),
            perc_doc_sup_med = mean(DSU_MED, na.rm = TRUE),
            ideb_2017_med = mean(ideb_medio_2017, na.rm = TRUE),
            perc_doc_sup_med_pond = weighted.mean(DSU_MED, w = pop2017, na.rm = TRUE),
            ideb_2017_med_pond = weighted.mean(ideb_medio_2017, w = pop2017, na.rm = TRUE))

# Scatterplot Ano Iniciais
ggplot(df_medias,
       aes(x = perc_doc_sup_pond, y = ideb_2017_pond)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = 'loess', se = FALSE, color = 'red') +
  custom_theme() +
  scale_y_continuous(labels = function(x) formatC(x, digits = 2, big.mark = '.', decimal.mark = ',')) +
  scale_x_continuous(labels = function(x) paste(x, '%')) +
  labs(x = 'Percentual de docentes com curso superior',
       y = 'Média IDEB 2017 - Anos Iniciais',
       title = 'Nota no IDEB vs. Docentes com curso superior',
       subtitle = 'Médias das Regiões de Governo de SP, ponderadas pela população dos municípios',
       caption = 'Fonte: Elaboração própria a partir de dados do INEP e do IBGE.')
# ggsave('ideb_vs_dsu_anos_iniciais_2017_pond.jpg', height = 7, width = 7)

# Scatterplot Ensino Médio
ggplot(df_medias,
       aes(x = perc_doc_sup_med_pond, y = ideb_2017_med_pond)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = 'loess', se = FALSE, color = 'red') +
  custom_theme() +
  scale_y_continuous(labels = function(x) formatC(x, digits = 2, big.mark = '.', decimal.mark = ',')) +
  scale_x_continuous(labels = function(x) paste(x, '%')) +
  labs(x = 'Percentual de docentes com curso superior',
       y = 'Média IDEB 2017 - Anos Iniciais',
       title = 'Nota no IDEB vs. Docentes com curso superior',
       subtitle = 'Médias das Regiões de Governo de SP, ponderadas pela população dos municípios',
       caption = 'Fonte: Elaboração própria a partir de dados do INEP e do IBGE.')
