library(cagedExplorer)

# Lendo dados IDEB Anos Iniciais
ideb_iniciais <- readRDS('df_ideb_iniciais.rds')

# Filtrando municipios de SP e selecionando variaveis
df_iniciais <- municipios_sp %>% 
  select(Codmun7, municipio, municipio_clean, pop, regiao_administrativa, regiao_governo) %>% 
  left_join(ideb_iniciais %>% 
              filter(rede == 'Pública') %>% 
              select(Codmun7, taxa_aprov_iniciais_2017,
                     ind_rend_iniciais_2017, saeb_iniciais_2017, ideb_iniciais_2017),
            by = 'Codmun7')

# Agregando por regioes
df_medias <- df_iniciais %>% 
  group_by(regiao_governo) %>% 
  summarise(ideb_2017 = mean(ideb_iniciais_2017, na.rm = TRUE),
            saeb_2017 = mean(saeb_iniciais_2017, na.rm = TRUE),
            rend_2017 = mean(ind_rend_iniciais_2017, na.rm = TRUE),
            aprov_2017 = mean(taxa_aprov_iniciais_2017, na.rm = TRUE))

# Definindo breaks e palette --------------------------------------------------
media_ideb_sp <- mean(df_iniciais$ideb_iniciais_2017)
breaks <- c(5.48, 6, 6.23077, 6.440625, media_ideb_sp, 6.566667, 6.68, 6.9, 6.981250)
table(.bincode(df_medias$ideb_2017, breaks = breaks))
palette <- c(reds_full[1], reds_full[3:5], blues_full[c(3, 4, 5, 7)])

# DOTPLOT ---------------------------------------------------------------------
df_medias %>%
  mutate(regiao_governo = fct_reorder(regiao_governo, ideb_2017)) %>%
  mutate(grupo_saldo = .bincode(ideb_2017, breaks = breaks) %>%
           as.factor() %>% fct_reorder(ideb_2017)) %>% 
  ggplot() +
  geom_point(aes(x = ideb_2017, y = regiao_governo, fill = grupo_saldo),
             size = 5, shape = 21, col = 'gray') +
  geom_vline(xintercept = media_ideb_sp, col = 'darkred', alpha = 0.5) +
  scale_fill_manual(values = palette) +
  scale_x_continuous(labels = function(x) formatC(x, digits = 2, big.mark = '.', decimal.mark = ',')) +
  custom_theme() +
  theme(legend.position = 'none') +
  labs(x = 'Nota IDEB 2017 - Anos Iniciais',
       y = 'Região de Governo',
       title = 'Desempenho no IDEB 2017 - Anos Iniciais',
       subtitle = 'Comparação das médias por região de governo',
       caption = 'Nota: A linha vertical indica a média dos municípios paulistas (6,542).\nFonte: Elaboração própria a partir de dados do INEP.')

# ggsave('dotplot_ideb2017_anos_iniciais.png', height = 8, width = 6)

# MAPA ------------------------------------------------------------------------
map_title <-  'Regiões de Governo de São Paulo - IDEB 2017 - Anos Iniciais'

mapa_ideb <- df_medias %>% 
  add_geometry_regioes_gov() %>% 
  mutate(`Média IDEB` = ideb_2017) %>% 
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

tmap::tmap_save(mapa_ideb, filename = 'mapa_ideb2017_anos_iniciais.png', width = 7, height = 5.5)