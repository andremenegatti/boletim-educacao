library(cagedExplorer)

pop2017 <- readxl::read_excel('estimativa_populacao_municipios_2017.xlsx', sheet = 1,
                              range = 'A1:C5571', col_names = TRUE) %>% 
  filter(str_detect(Codmun7, '35\\d{5}')) %>% 
  select(-municipio) %>% 
  mutate(Codmun7 = as.integer(Codmun7))

df_municipios <- municipios_sp %>% 
  inner_join(pop2017, by = 'Codmun7') %>% 
  select(-regiao_metropolitana, -aglomeracao_urbana, -pop)


# Lendo dados IDEB Anos Iniciais
ideb_iniciais <- readRDS('df_ideb_iniciais.rds')

# Filtrando municipios de SP e selecionando variaveis
df_iniciais <- df_municipios %>% 
  select(Codmun7, municipio, municipio_clean, pop2017, regiao_administrativa, regiao_governo) %>% 
  left_join(ideb_iniciais %>% 
              filter(rede == 'Pública') %>% 
              select(Codmun7, taxa_aprov_iniciais_2017,
                     ind_rend_iniciais_2017, saeb_iniciais_2017, ideb_iniciais_2017),
            by = 'Codmun7')

# Agregando por regioes
df_medias <- df_iniciais %>% 
  group_by(regiao_governo) %>% 
  summarise(pop = sum(pop2017),
            ideb_2017 = mean(ideb_iniciais_2017, na.rm = TRUE),
            saeb_2017 = mean(saeb_iniciais_2017, na.rm = TRUE),
            rend_2017 = mean(ind_rend_iniciais_2017, na.rm = TRUE),
            aprov_2017 = mean(taxa_aprov_iniciais_2017, na.rm = TRUE),
            ideb_2017_pond = weighted.mean(ideb_iniciais_2017, w = pop2017, na.rm = TRUE),
            saeb_2017_pond = weighted.mean(saeb_iniciais_2017, w = pop2017, na.rm = TRUE),
            rend_2017_pond = weighted.mean(ind_rend_iniciais_2017, w = pop2017, na.rm = TRUE),
            aprov_2017_pond = weighted.mean(taxa_aprov_iniciais_2017, w = pop2017, na.rm = TRUE))

# Definindo breaks e palette --------------------------------------------------
media_ideb_sp <- weighted.mean(df_iniciais$ideb_iniciais_2017, w = df_iniciais$pop2017)
breaks <- c(5.91, 6, 6.30709, media_ideb_sp, 6.589727, 6.64985, 6.72893, 6.874487, 7.06548, 7.1855)
# sort(df_medias$ideb_2017_pond)
# table(.bincode(df_medias$ideb_2017_pond, breaks = breaks))
palette <- c(reds_full[2:4], blues_full[3:8])

# DOTPLOT ---------------------------------------------------------------------
df_medias %>%
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
       title = 'Desempenho no IDEB 2017 - Anos Iniciais',
       subtitle = 'Comparação das médias por região de governo, ponderadas pela população dos municípios',
       caption = 'Nota: A linha vertical indica a média dos municípios paulistas, ponderada pela população.\nFonte: Elaboração própria a partir de dados do INEP.')

ggsave('dotplot_ideb2017_anos_iniciais_ponderada.png', height = 8, width = 6)

# MAPA ------------------------------------------------------------------------
map_title <-  'Regiões de Governo de São Paulo - IDEB 2017 - Anos Iniciais'

mapa_ideb <- df_medias %>% 
  add_geometry_regioes_gov() %>% 
  mutate(`Média Ponderada IDEB` = ideb_2017_pond) %>% 
  tmap::tm_shape() +
  tmap::tm_style("beaver",
                 legend.format = list(fun = function(x) formatC(x, digits = 2, big.mark = '.',
                                                                decimal.mark = ',', format = 'f'),
                                      text.separator = " a ")) +
  tmap::tm_fill("Média Ponderada IDEB",
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

tmap::tmap_save(mapa_ideb, filename = 'mapa_ideb2017_anos_iniciais_ponderada.png', width = 7, height = 5.5)
