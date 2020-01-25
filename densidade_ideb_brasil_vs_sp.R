library(tidyverse)

# LENDO DADOS IDEB ------------------------------------------------------
# Nao usamos a base salva em .rds porque precisamos de dados de todo o Brasil

# Anos Iniciais
ideb_iniciais <- 
  readxl::read_excel(
    "data/divulgacao_anos_iniciais_municipios2017-atualizado-Jun_2019.xlsx",
    sheet = 1, range = 'A8:CK14444', col_names = TRUE
    ) %>% 
  select(uf = SG_UF, Codmun7 = COD_MUN, rede = REDE,
         taxa_aprov_iniciais_2005 = TAP05_F14,
         taxa_aprov_iniciais_2007 = TAP07_F14,
         taxa_aprov_iniciais_2009 = TAP09_F14,
         taxa_aprov_iniciais_2011 = TAP11_F14,
         taxa_aprov_iniciais_2013 = TAP13_F14,
         taxa_aprov_iniciais_2015 = TAP15_F14,
         taxa_aprov_iniciais_2017 = tap_f14,
         ind_rend_iniciais_2005 = P14_05,
         ind_rend_iniciais_2007 = P14_07,
         ind_rend_iniciais_2009 = P14_09,
         ind_rend_iniciais_2011 = P14_11,
         ind_rend_iniciais_2013 = P14_13,
         ind_rend_iniciais_2015 = P14_15,
         ind_rend_iniciais_2017 = P4,
         saeb_iniciais_2005 = PAD14_05,
         saeb_iniciais_2007 = PAD14_07,
         saeb_iniciais_2009 = PAD14_09,
         saeb_iniciais_2011 = PAD14_11,
         saeb_iniciais_2013 = PAD14_13,
         saeb_iniciais_2015 = PAD14_15,
         saeb_iniciais_2017 = PAD14_17,
         ideb_iniciais_2005 = IDEB14_05,
         ideb_iniciais_2007 = IDEB14_07,
         ideb_iniciais_2009 = IDEB14_09,
         ideb_iniciais_2011 = IDEB14_11,
         ideb_iniciais_2013 = IDEB14_13,
         ideb_iniciais_2015 = IDEB14_15,
         ideb_iniciais_2017 = IDEB14_17) %>% 
  mutate_at(vars(contains('_iniciais_')),.funs = as.numeric)

# Ano Finais
ideb_finais <- 
  readxl::read_excel(
    "data/divulgacao_anos_finais_municipios2017-atualizado-Jun_2019.xlsx",
    sheet = 1, range = 'A8:CD14365', col_names = TRUE) %>% 
  select(uf = SG_UF, Codmun7 = COD_MUN, rede = REDE,
         taxa_aprov_finais_2005 = TAP05_F58,
         taxa_aprov_finais_2007 = TAP07_F58,
         taxa_aprov_finais_2009 = TAP09_F58,
         taxa_aprov_finais_2011 = TAP11_F58,
         taxa_aprov_finais_2013 = TAP13_F58,
         taxa_aprov_finais_2015 = TAP15_F58,
         taxa_aprov_finais_2017 = tap_f58,
         ind_rend_finais_2005 = P58_05,
         ind_rend_finais_2007 = P58_07,
         ind_rend_finais_2009 = P58_09,
         ind_rend_finais_2011 = P58_11,
         ind_rend_finais_2013 = P58_13,
         ind_rend_finais_2015 = P58_15,
         ind_rend_finais_2017 = P8,
         saeb_finais_2005 = PAD58_05,
         saeb_finais_2007 = PAD58_07,
         saeb_finais_2009 = PAD58_09,
         saeb_finais_2011 = PAD58_11,
         saeb_finais_2013 = PAD58_13,
         saeb_finais_2015 = PAD58_15,
         saeb_finais_2017 = PAD58_17,
         ideb_finais_2005 = IDEB58_05,
         ideb_finais_2007 = IDEB58_07,
         ideb_finais_2009 = IDEB58_09,
         ideb_finais_2011 = IDEB58_11,
         ideb_finais_2013 = IDEB58_13,
         ideb_finais_2015 = IDEB58_15,
         ideb_finais_2017 = IDEB58_17) %>% 
  mutate_at(vars(starts_with('ideb_finais_')), .funs = as.numeric) %>% 
  mutate_at(vars(starts_with('saeb_finais_')), .funs = as.numeric)

# Ensino Medio
ideb_medio <- 
  readxl::read_excel(
    "data/divulgacao_ensino_medio_municipios2017-atualizado-Jun_2019.xlsx",
    sheet = 1, range = 'A7:O11269') %>% 
  select(uf = SG_UF, Codmun7 = COD_MUN, rede = REDE, ideb_medio_2017 = IDEB12_17,
         taxa_aprov_medio_2017 = TAP_MED, ind_rend_medio_2017 = P12,
         saeb_medio_2017 = PAD12_17) %>%
  mutate_at(vars(contains("_medio_2017")), as.numeric)

# FILTRANDO REDE PUBLICA E SELECIONANDO VARIAVEIS -----------------------------
ideb_iniciais <- ideb_iniciais %>% 
  filter(rede == 'Pública') %>%
  select(uf, Codmun7, rede, nota = ideb_iniciais_2017) %>% 
  mutate(nivel = 'Fundamental - Anos Iniciais') 

ideb_finais <- ideb_finais %>% 
  filter(rede == 'Pública') %>%
  select(uf, Codmun7, rede, nota = ideb_finais_2017) %>% 
  mutate(nivel = 'Fundamental - Anos Finais')

ideb_medio <- ideb_medio %>%
  filter(rede == 'Pública') %>%
  select(uf, Codmun7, rede, nota = ideb_medio_2017) %>% 
  mutate(nivel = 'Médio')

# JUNTANDO BASES --------------------------------------------------------------
# Dados Brasil
df_ideb_brasil <- ideb_iniciais %>% 
  bind_rows(ideb_finais) %>% 
  bind_rows(ideb_medio) %>% 
  mutate(abrangencia = 'Municípios Brasileiros',
         nivel = fct_relevel(nivel, "Médio", "Fundamental - Anos Finais",
                             "Fundamental - Anos Iniciais"))

# Filtrando municipios paulistas
df_ideb_sp <- df_ideb_brasil %>% 
  filter(uf == 'SP') %>% 
  mutate(abrangencia = 'Municípios Paulistas')

# Dados completos
df_ideb_full <- df_ideb_brasil %>% 
  bind_rows(df_ideb_sp)

# CALCULANDO MEDIAS -----------------------------------------------------------
df_medias <- df_ideb_full %>% 
  group_by(abrangencia, nivel) %>% 
  summarise(media = mean(nota, na.rm = TRUE))

# DENSITY PLOT ----------------------------------------------------------------
df_ideb_full %>% 
  ggplot() +
  geom_density(aes(x = nota, fill = nivel),
               alpha = 0.5, position = 'identity') +
  cagedExplorer::custom_theme() +
  theme(panel.grid = element_blank()) +
  # scale_fill_discrete(name = 'Nível de Ensino') +
  scale_y_continuous(labels = function(x) formatC(x, width = 3, big.mark = '.', decimal.mark = ','),
                     limits = c(0, 1.25), breaks = seq(0, 1, by = 0.5)) +
  scale_x_continuous(labels = function(x) formatC(x, width = 3, big.mark = '.', decimal.mark = ',')) +
  labs(x = 'Nota IDEB',
       y = 'Densidade',
       title = 'Distribuição das médias municipais no IDEB 2017, por nível de ensino',
       subtitle = 'Comparação dos resultados do Brasil e do Estado de São Paulo',
       caption = "Fonte: Elaboração própria a partir de dados do INEP.\nNota: Médias indicadas pelas linhas verticais e caixas de texto.") +
  facet_wrap(~ abrangencia, nrow = 2) +
  geom_vline(data = df_medias,
             aes(xintercept = media, col = nivel),
             alpha = 0.6, linetype = 'dotted') +
  geom_label(data = df_medias,
            aes(x = media, y = 1.2, col = nivel,
                label = round(media, digits = 2) %>% 
                  str_replace("\\.", ",")),
            size = 3, family = 'serif') +
  scale_fill_brewer(name = 'Nível de Ensino', palette = 'Dark2') +
  scale_color_brewer(name = 'Nível de Ensino', palette = 'Dark2') +
  guides(col = FALSE)

ggsave('plots/densidade_ideb_brasil_vs_sp.png', width = 7, height = 7)