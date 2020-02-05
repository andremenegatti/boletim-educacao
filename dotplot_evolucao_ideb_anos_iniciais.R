library(tidyverse)
library(cagedExplorer)

# Carregando dados de educacao ------------------------------------------------
ideb_iniciais <- readRDS('data/df_ideb_iniciais.rds') %>%
  filter(rede == 'Pública') %>% 
  select(Codmun7, ideb_iniciais_2007, ideb_iniciais_2017)

matriculas_2017 <- readxl::read_excel('data/matriculas_sp.xlsx', sheet = 1,
                                      range = 'A1:H646', col_names = TRUE) %>% 
  mutate(matriculas_rede_publica_2017 = anos_iniciais_federal + 
           anos_iniciais_estadual + anos_iniciais_municipal) %>% 
  select(Codmun7, matriculas_rede_publica_2017)

matriculas_2007 <- readxl::read_excel('data/matriculas_sp.xlsx', sheet = 3,
                                      range = 'A1:H646', col_names = TRUE) %>% 
  mutate(matriculas_rede_publica_2007 = anos_iniciais_federal + 
           anos_iniciais_estadual + anos_iniciais_municipal) %>% 
  select(Codmun7, matriculas_rede_publica_2007)

# População 2017 - Estimativa IBGE --------------------------------------------
pop2017 <- 
  readxl::read_excel('data/estimativa_populacao_municipios_2017.xlsx',
                     sheet = 1, range = 'A1:C5571', col_names = TRUE) %>% 
  filter(str_detect(Codmun7, '35\\d{5}')) %>% 
  select(Codmun7, pop2017) %>% 
  mutate(Codmun7 = as.integer(Codmun7))

# Carregando municipios_sp e juntando dados de educacao -----------------------
df_mun <- readRDS('data/municipios_sp.rds') %>% 
  select(Codmun7, municipio, municipio_clean,
         regiao_administrativa, regiao_governo) %>% 
  left_join(pop2017, by = 'Codmun7') %>% 
  left_join(ideb_iniciais, by = 'Codmun7') %>% 
  left_join(matriculas_2007, by = 'Codmun7') %>% 
  left_join(matriculas_2017, by = 'Codmun7') %>% 
  mutate(evolucao_bruta = ideb_iniciais_2017 - ideb_iniciais_2007,
         evolucao_relativa = evolucao_bruta * 100 / ideb_iniciais_2007)

df_mun_long <- df_mun %>% 
  pivot_longer(cols = c(ideb_iniciais_2007, ideb_iniciais_2017),
               names_to = 'ano', names_prefix = 'ideb_iniciais_',
               values_to = 'ideb_iniciais') %>%
  mutate(ano = as.integer(ano),
         matriculas_rede_publica = ifelse(ano == 2007,
                                          matriculas_rede_publica_2007,
                                          matriculas_rede_publica_2017)) %>% 
  select(-matriculas_rede_publica_2007, matriculas_rede_publica_2017)

df_reg_long <- df_mun_long %>% 
  group_by(regiao_governo, ano) %>% 
  summarise(
    pop2017 = mean(pop2017),
    matriculas = sum(matriculas_rede_publica),
    ideb = mean(ideb_iniciais, na.rm = TRUE),
    ideb_pond = weighted.mean(ideb_iniciais,
                              w = matriculas_rede_publica,
                              na.rm = TRUE)
    ) %>% ungroup() %>% 
  arrange(ano) %>% 
  group_by(regiao_governo) %>% 
  mutate(evolucao_bruta = ifelse(ano == 2007,
                                 lead(ideb_pond) - ideb_pond,
                                 ideb_pond - lag(ideb_pond)),
         evolucao_relativa = evolucao_bruta / first(ideb_pond)) %>% 
  ungroup() %>% 
  mutate(regiao_governo = fct_reorder(regiao_governo,
                                      ideb_pond,
                                      last))

# Dotplot evolucao bruta, com setas -------------------------------------------
dotplot_evolucao_bruta <- 
  ggplot(df_reg_long) +
  geom_path(aes(x = ideb_pond, y = regiao_governo),
            arrow = arrow(length = unit(1.5, 'mm'), type = 'closed')) +
  geom_text(aes(x = ideb_pond,
                y = regiao_governo,
                label = formatC(ideb_pond, 3, big.mark = '.', decimal.mark = ','),
                hjust = ifelse(ano == '2007', 1.4, -0.4)),
            size = 3, family = 'Serif', color = 'gray25') +
  cagedExplorer::custom_theme() +
  scale_x_continuous(limits = c(4.3, 7.4)) +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.y = element_line(linetype = 'dotted')) +
  labs(
    x = 'Média IDEB',
     y = 'Região de Governo',
     title = 'Evolução do IDEB Anos Iniciais',
     subtitle = 'Médias das Regiões de Governo de São Paulo em 2007 e 2017',
     caption = 'Notas:
     i) Elaboração própria a partir de dados do INEP;
     ii) Ponderação pelo número de matrículas em cada município (rede pública, anos iniciais)'
    ) ; dotplot_evolucao_bruta

# ggsave(plot = dotplot_evolucao_bruta,
#        filename = 'plots/dotplot_ideb_anos_iniciais_evolucao_bruta.png',
#        height = 8, width = 6.5)

# Dotplot evolucao relativa ---------------------------------------------------
# Filtrando apenas 2017 para calcular breakpoints e definir cores
df_reg_2017 <- df_reg_long %>% 
  filter(ano == 2017) 

# Definindo breakpoints e palette
media_ideb_sp <- weighted.mean(df_mun$ideb_iniciais_2017, w = df_mun$matriculas_rede_publica_2017)
breaks <- c(5.83, 6, 6.312623, media_ideb_sp, 6.579427, 6.661894, 6.767315, 6.874487, 7.072346, 7.1855)
sort(df_reg_2017$ideb_pond)
table(.bincode(df_reg_2017$ideb_pond, breaks = breaks))
palette <- c(reds_full[2:4], blues_full[3:8])

# Variável de grupo para as cores e reordenando regiao_governo
df_reg_2017 <- df_reg_2017 %>% 
  mutate(grupo_saldo = .bincode(ideb_pond, breaks = breaks) %>%
           as.factor() %>% fct_reorder(ideb_pond), 
         regiao_governo = fct_reorder(regiao_governo, evolucao_relativa))

# Plotting
dotplot_evolucao_relativa <- ggplot(df_reg_2017) +
  geom_point(aes(x = evolucao_relativa, y = regiao_governo,
                 size = pop2017, fill = grupo_saldo, col = grupo_saldo),
             shape = 21, alpha = 0.8) +
  geom_point(aes(x = evolucao_relativa, y = regiao_governo,
                 size = pop2017, col = grupo_saldo),
             shape = 1) +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) +
  custom_theme() +
  scale_x_continuous(labels = function(x) str_c(x * 100, '%')) +
  guides(size = FALSE, fill = FALSE, color = FALSE) +
  labs(
    x = 'Evolução Relativa da Média do IDEB entre 2007 e 2017',
    y = 'Região de Governo',
    title = 'Evolução do IDEB Anos Iniciais',
    subtitle = 'Evolução relativa das Regiões de Governo de SP entre 2007 2017',
    caption = 'Notas:
     i) Elaboração própria a partir de dados do INEP;
     ii) Foram utilizadas médias regionais ponderadas pelo número de matrículas em cada município;
    iii) O tamanho dos círculos é proporcional à população de cada região de governo;
    iv) As cores refletem o desempenho de cada região comparado à média estadual, em 2017'
  ) ; dotplot_evolucao_relativa

# ggsave(plot = dotplot_evolucao_relativa,
#        filename = 'plots/dotplot_ideb_anos_iniciais_evolucao_relativa.png',
#        height = 8, width = 6.5)

dotplotly <- plotly::ggplotly(dotplot_evolucao_relativa)
htmlwidgets::saveWidget(dotplotly, 'dotplot_ideb_evolucao_relativa.html')
