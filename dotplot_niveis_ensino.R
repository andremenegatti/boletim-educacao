library(tidyverse)

municipios_sp <- readRDS('data/df_dados_completos_anos_iniciais.rds')  %>% 
  select(Codmun7, municipio, municipio_clean, regiao_governo, pop_ibge)

# Importando dados matrículas -------------------------------------------
# Consideramos apenas rede pública

# Ensino Fundamental
matriculas_fundamental <- 
  readxl::read_excel('data/matriculas_sp.xlsx', sheet = 1,
                     range = 'A1:M646', col_names = TRUE) %>% 
  mutate(
    
    matriculas_iniciais = 
      anos_iniciais_federal + anos_iniciais_estadual + anos_iniciais_municipal,
    
    municipalizacao_iniciais = 
      anos_iniciais_municipal / matriculas_iniciais,
    
    matriculas_finais =
      anos_finais_federal + anos_finais_estadual + anos_finais_municipal,
    
    municipalizacao_finais =
      anos_finais_municipal / matriculas_finais
    
    ) %>% 
  select(Codmun7, matriculas_iniciais, matriculas_finais,
         municipalizacao_iniciais, municipalizacao_finais)

# Ensino Médio
matriculas_medio <- 
  readxl::read_excel('data/matriculas_sp.xlsx', sheet = 2,
                     range = 'A1:R646', col_names = TRUE) %>% 
  mutate(
    matriculas = 
      ano1_federal + ano1_estadual + ano1_municipal +
      ano2_federal + ano2_estadual + ano2_municipal +
      ano3_federal + ano3_estadual + ano3_municipal,
    
    municipalizacao = 
      (ano1_municipal + ano2_municipal + ano3_municipal) / matriculas,
    
    nivel = 'medio',
    
    ) %>% 
  select(Codmun7, nivel, matriculas, municipalizacao)

# LENDO DADOS IDEB ------------------------------------------------------
# Anos Iniciais
ideb_iniciais <- 
  readxl::read_excel(
    "data/divulgacao_anos_iniciais_municipios2017-atualizado-Jun_2019.xlsx",
    sheet = 1, range = 'A8:CK14444', col_names = TRUE
  ) %>% 
  filter(REDE == 'Pública', SG_UF == 'SP') %>% 
  select(Codmun7 = COD_MUN, ideb = IDEB14_17) %>% 
  mutate(ideb = as.numeric(ideb)) %>% 
  mutate(nivel = 'iniciais')

# Ano Finais
ideb_finais <- 
  readxl::read_excel(
    "data/divulgacao_anos_finais_municipios2017-atualizado-Jun_2019.xlsx",
    sheet = 1, range = 'A8:CD14365', col_names = TRUE) %>% 
  filter(REDE == 'Pública', SG_UF == 'SP') %>%
  select(Codmun7 = COD_MUN, ideb = IDEB58_17) %>%
  mutate(ideb = as.numeric(ideb)) %>% 
  mutate(nivel = 'finais')

# Ensino Medio
ideb_medio <- 
  readxl::read_excel(
    "data/divulgacao_ensino_medio_municipios2017-atualizado-Jun_2019.xlsx",
    sheet = 1, range = 'A7:O11269') %>% 
  filter(REDE == 'Pública', SG_UF == 'SP') %>% 
  select(Codmun7 = COD_MUN, ideb = IDEB12_17) %>%
  mutate(ideb = as.numeric(ideb)) %>% 
  mutate(nivel = 'medio')

# JUNTANDO BASES --------------------------------------------------------------
matriculas_long <- matriculas_fundamental %>% 
  pivot_longer(cols = c(matriculas_iniciais, matriculas_finais),
               names_to = 'nivel', values_to = 'matriculas',
               names_prefix = 'matriculas_') %>% 
  mutate(municipalizacao = ifelse(nivel == 'iniciais',
                                  municipalizacao_iniciais,
                                  municipalizacao_finais)) %>% 
  select(Codmun7, nivel, matriculas, municipalizacao) %>% 
  bind_rows(matriculas_medio)

ideb_long <- ideb_iniciais %>% 
  bind_rows(ideb_finais) %>% 
  bind_rows(ideb_medio)

df_mun <- matriculas_long %>% 
  left_join(ideb_long, by = c('Codmun7', 'nivel')) %>% 
  left_join(municipios_sp, by = 'Codmun7') %>% 
  select(Codmun7, municipio, regiao_governo, nivel, ideb, matriculas,
         municipalizacao, municipio_clean)

# CHECANDO MISSING DATA NO IDEB ----------------------------------------------
# Lista com DFs contendo municipios sem dados de IDEB em cada nivel
na_list <- map2(
  .x = list(ideb_iniciais, ideb_finais, ideb_medio),
  .y = c('iniciais', 'finais', 'medio'),
  .f = ~ municipios_sp %>% 
    left_join(.x, by = 'Codmun7') %>% 
    left_join(matriculas_long %>% filter(nivel == .y), by = 'Codmun7') %>% 
    filter(is.na(ideb)) %>% 
    select(municipio, regiao_governo, pop_ibge, matriculas)
) %>% set_names('iniciais', 'finais', 'medio') ; na_list

# Lista contendo numero de municipios sem dados, quanto esse numero representa
# do total de municipios da regiao, populacao desses municipios sem dados e
# quanto essa populacao representa do total da regiao
map(
  .x = na_list,
  .f = ~ municipios_sp %>%
    group_by(regiao_governo) %>% 
    summarise(muns_regiao = n(), pop_regiao = sum(pop_ibge)) %>% 
    left_join(.x %>% group_by(regiao_governo) %>% 
                summarise(muns_na = n(), pop_na = sum(pop_ibge)),
              by = 'regiao_governo') %>% 
    mutate(muns_na = ifelse(is.na(muns_na), 0, muns_na),
           pop_na = ifelse(is.na(pop_na), 0, pop_na),
           na_ratio = muns_na / muns_regiao,
           na_pop_ratio = pop_na / pop_regiao) %>% 
    arrange(desc(na_ratio))
)

# Conclusão: missing data não parece ser um problema

# CALCULANDO MÉDIAS REGIONAIS -------------------------------------------------
df_reg <- df_mun %>% 
  group_by(regiao_governo, nivel) %>% 
  summarise(media_ideb = weighted.mean(ideb, w = matriculas, na.rm = TRUE),
            total_matriculas = sum(matriculas, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(regiao_governo = fct_reorder(regiao_governo, media_ideb, median),
         nivel = case_when(nivel == 'iniciais' ~ 'Anos Iniciais',
                           nivel == 'finais' ~ 'Anos Finais',
                           nivel == 'medio' ~ 'Médio') %>% 
           fct_relevel('Anos Iniciais', 'Anos Finais', 'Médio'))


dotplot_niveis <- ggplot(df_reg) +
  geom_point(aes(x = media_ideb, y = regiao_governo,
                 shape = nivel, col = nivel), size = 3) +
  scale_color_brewer(palette = 'Accent', name = 'Nível de Ensino') +
  scale_shape_discrete(name = 'Nível de Ensino') +
  cagedExplorer::custom_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = 'dotted')) +
  labs(
    x = 'Média IDEB',
    y = 'Região de Governo',
    title = 'Desempenho das Regiões de Governo de SP por nível educacional',
    subtitle = 
      'Médias do IDEB 2017 ponderadas por matrículas na rede pública em cada nível',
    caption = 'Fonte: Elaboração própria a partir de dados do INEP.'
  ) ; dotplot_niveis

ggsave(plot = dotplot_niveis,
       filename = 'plots/dotplot_ideb_niveis_ensino.png',
       height = 8, width = 7.5)

plotlyplot <- plotly::ggplotly(dotplot_niveis) 

htmlwidgets::saveWidget(widget = plotlyplot,
                        file = 'dotplot_ideb_niveis_ensino.html')
