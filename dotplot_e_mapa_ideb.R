library(cagedExplorer)

# IMPORTANDO DADOS ------------------------------------------------------------
# População 2017 - Estimativa IBGE
pop2017 <- readxl::read_excel('data/estimativa_populacao_municipios_2017.xlsx', sheet = 1,
                              range = 'A1:C5571', col_names = TRUE) %>% 
  filter(str_detect(Codmun7, '35\\d{5}')) %>% 
  select(Codmun7, pop2017) %>% 
  mutate(Codmun7 = as.integer(Codmun7))

# Notas IDEB Anos Iniciais, rede pública
ideb_iniciais <- readRDS('data/df_ideb_iniciais.rds') %>% 
  filter(rede == 'Pública') %>% 
  select(Codmun7, taxa_aprov_iniciais_2017,
         ind_rend_iniciais_2017, saeb_iniciais_2017,
         ideb_iniciais_2017)

# Matriculas anos iniciais, rede pública
matriculas_iniciais <- readRDS('data/matriculas_iniciais_long.rds') %>% 
  filter(rede == 'publica') %>% 
  rename(matriculas_rede_publica = matriculas) %>% 
  select(-municipio, -rede)

# Municipios SP
df_municipios <- readRDS('data/municipios_sp.rds')

# Juntando dados --------------------------------------------------------------
# Municipios SP (do pacote cagedExplorer)
df_iniciais <- df_municipios %>%  
  inner_join(pop2017, by = 'Codmun7') %>% 
  inner_join(ideb_iniciais, by = 'Codmun7') %>% 
  inner_join(matriculas_iniciais, by = 'Codmun7') %>% 
  select(Codmun7, municipio, municipio_clean, pop2017,
         regiao_administrativa, regiao_governo,
         taxa_aprov_iniciais_2017:matriculas_rede_publica)

# Agregando por regioes -------------------------------------------------------
df_medias <- df_iniciais %>% 
  group_by(regiao_governo) %>% 
  summarise(pop = sum(pop2017),
            matriculas = sum(matriculas_rede_publica),
            ideb_2017 = mean(ideb_iniciais_2017, na.rm = TRUE),
            saeb_2017 = mean(saeb_iniciais_2017, na.rm = TRUE),
            rend_2017 = mean(ind_rend_iniciais_2017, na.rm = TRUE),
            aprov_2017 = mean(taxa_aprov_iniciais_2017, na.rm = TRUE),
            ideb_2017_pond = weighted.mean(ideb_iniciais_2017, w = matriculas_rede_publica, na.rm = TRUE),
            saeb_2017_pond = weighted.mean(saeb_iniciais_2017, w = matriculas_rede_publica, na.rm = TRUE),
            rend_2017_pond = weighted.mean(ind_rend_iniciais_2017, w = matriculas_rede_publica, na.rm = TRUE),
            aprov_2017_pond = weighted.mean(taxa_aprov_iniciais_2017, w = matriculas_rede_publica, na.rm = TRUE),
            ideb_2017_pond_pop = weighted.mean(ideb_iniciais_2017, w = pop2017, na.rm = TRUE),
            saeb_2017_pond_pop = weighted.mean(saeb_iniciais_2017, w = pop2017, na.rm = TRUE),
            rend_2017_pond_pop = weighted.mean(ind_rend_iniciais_2017, w = pop2017, na.rm = TRUE),
            aprov_2017_pond_pop = weighted.mean(taxa_aprov_iniciais_2017, w = pop2017, na.rm = TRUE))

# Definindo breaks e palette --------------------------------------------------
# Calculando media ponderada pelo numero de matriculas
# Regiões abaixo da média serão pintadas de tons vermelhos
media_ideb_sp <- weighted.mean(df_iniciais$ideb_iniciais_2017, w = df_iniciais$matriculas_rede_publica)

breaks <- c(5.83, 6, 6.312623, media_ideb_sp, 6.579427, 6.661894, 6.767315, 6.874487, 7.072346, 7.1855)
sort(df_medias$ideb_2017_pond)
table(.bincode(df_medias$ideb_2017_pond, breaks = breaks))

palette <- c(reds_full[2:4], blues_full[3:8])

# DOTPLOT ---------------------------------------------------------------------
dotplot1 <- df_medias %>%
  mutate(regiao_governo = fct_reorder(regiao_governo, ideb_2017_pond)) %>%
  mutate(grupo_saldo = .bincode(ideb_2017_pond, breaks = breaks) %>%
           as.factor() %>% fct_reorder(ideb_2017_pond)) %>% 
  ggplot() +
  geom_point(aes(x = ideb_2017_pond, y = regiao_governo, fill = grupo_saldo),
             size = 5, shape = 21, col = 'gray') +
  geom_vline(xintercept = media_ideb_sp, col = 'darkred', alpha = 0.5) +
  scale_fill_manual(values = palette) +
  scale_x_continuous(labels = function(x) formatC(x, digits = 2, big.mark = '.', decimal.mark = ',')) +
  custom_theme() +
  theme(legend.position = 'none') +
  labs(x = 'Nota IDEB 2017 - Anos Iniciais',
       y = 'Região de Governo',
       title = 'Desempenho das Regiões de Governo no IDEB 2017',
       subtitle = 'Anos iniciais - Rede pública - Médias ponderadas pelo número de matrículas',
       caption = 'Notas:
       i) Elaboração própria a partir de dados do INEP;
       ii) Ponderação pelo número de matrículas em cada município (rede pública, anos iniciais);
       iii) A linha vertical indica a média (ponderada) dos municípios paulistas')

# Grafico interativo para inspecao
# plotly::ggplotly(dotplot1)

ggsave(plot = dotplot1, filename = 'plots/dotplot_ideb2017_anos_iniciais_ponderada_matriculas_rede_publica.png', height = 8, width = 6.5)

# MAPA ------------------------------------------------------------------------
map_title <-  'Regiões de Governo de São Paulo - IDEB 2017 Anos Iniciais - Rede Pública'

mapa_ideb <- df_medias %>% 
  add_geometry_regioes_gov() %>% 
  mutate(`Média IDEB` = ideb_2017_pond) %>% 
  tmap::tm_shape() +
  tmap::tm_style("beaver",
                 legend.format = list(fun = function(x) formatC(x, digits = 2, big.mark = '.',
                                                                decimal.mark = ',', format = 'f'),
                                      text.separator = " a ")) +
  tmap::tm_fill("Média IDEB",
                palette = palette,
                style = 'fixed',
                breaks = breaks,
                alpha = 1,
                id = "regiao_governo") +
  tmap::tm_layout(main.title.size = 1.2,
                  fontfamily = 'serif',
                  main.title.fontface = 'bold',
                  scale = 1.1,
                  bg.color = "white",
                  inner.margins = c(.1, .1, .1, .1),
                  main.title = map_title) +
  tmap::tm_compass(north = 0,
                   type = "8star",
                   size = 2,
                   position = c("right", "bottom")) +
  tmap::tm_scale_bar(text.size = 0.6,
                     text.color = NA,
                     lwd = 1,
                     color.dark = "black",
                     color.light = "white") +
  tmap::tm_legend(legend.position = c(0.01, 0.08)) +
  tmap::tm_borders(col = "black", lwd = 0.3)

tmap::tmap_save(mapa_ideb, filename = 'plots/mapa_ideb2017_anos_iniciais_ponderada_matriculas_rede_publica.png', width = 7, height = 5.5)