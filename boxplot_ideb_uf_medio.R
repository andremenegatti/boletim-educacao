library(tidyverse)

ideb_medio <- readxl::read_excel("~/backup_windows/Desktop/USP_Municipios/Dados/IDEB/divulgacao_ensino_medio_municipios2017-atualizado-Jun_2019.xlsx",
                                 sheet = 1, range = 'A7:O11269') %>% 
  select(uf = SG_UF, Codmun7 = COD_MUN, rede = REDE, ideb_medio_2017 = IDEB12_17,
         taxa_aprov_medio_2017 = TAP_MED, ind_rend_medio_2017 = P12,
         saeb_medio_2017 = PAD12_17) %>%
  mutate_at(vars(contains("_medio_2017")), as.numeric)

medio_brasil <- ideb_medio %>% 
  select(uf, Codmun7, rede, ideb_medio_2017) %>% 
  mutate(regiao = case_when(uf %in% c('PR', 'RS', 'SC') ~ 'Sul',
                            uf %in% c('MS', 'MT', 'GO', 'DF') ~ 'Centro-Oeste',
                            uf %in% c('SP', 'MG', 'ES', 'RJ') ~ 'Sudeste',
                            uf %in% c('AM', 'RO', 'PA', 'AC', 'RR', 'TO', 'AP') ~ 'Norte',
                            TRUE ~ 'Nordeste')) %>% 
  filter(rede == "Pública") %>% 
  mutate(uf = fct_reorder(uf, ideb_medio_2017, function(x) median(x, na.rm = TRUE)))


medio_brasil %>% 
  ggplot(aes(x = uf, y = ideb_medio_2017, fill = regiao)) +
  geom_boxplot(outlier.shape = 1, outlier.alpha = 0.2) + 
  cagedExplorer::custom_theme() + 
  labs(x = 'Unidade Federativa',
       y = 'Nota IDEB 2017 - Ensino Médio',
       title = 'Desempenho das unidades federativas no IDEB 2017 - Ensino Médio',
       subtitle = 'Comparação de distribuições de médias municipais',
       caption = 'Fonte: Elaboração própria a partir de dados do INEP.') +
  scale_fill_brewer(palette = 'Accent', name = 'Região') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

# ggsave('plots/boxplot_ideb_uf_medio.png', width = 8, height = 6)
