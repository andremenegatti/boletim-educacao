library(tidyverse)

ideb_finais <- 
  readxl::read_excel("~/backup_windows/Desktop/USP_Municipios/Dados/IDEB/divulgacao_anos_finais_municipios2017-atualizado-Jun_2019.xlsx",
                     sheet = 1, range = 'A8:CD14365', col_names = TRUE) %>% 
  select(uf = SG_UF, Codmun7 = COD_MUN, rede = REDE,
         ideb_finais_2005 = IDEB58_05,
         ideb_finais_2007 = IDEB58_07,
         ideb_finais_2009 = IDEB58_09,
         ideb_finais_2011 = IDEB58_11,
         ideb_finais_2013 = IDEB58_13,
         ideb_finais_2015 = IDEB58_15,
         ideb_finais_2017 = IDEB58_17) %>% 
  mutate_at(vars(starts_with('ideb_finais_')), .funs = as.numeric) %>% 
  mutate_at(vars(starts_with('saeb_finais_')), .funs = as.numeric)

finais_brasil <- ideb_finais %>% 
  select(uf, Codmun7, rede, ideb_finais_2017) %>% 
  mutate(regiao = case_when(uf %in% c('PR', 'RS', 'SC') ~ 'Sul',
                            uf %in% c('MS', 'MT', 'GO', 'DF') ~ 'Centro-Oeste',
                            uf %in% c('SP', 'MG', 'ES', 'RJ') ~ 'Sudeste',
                            uf %in% c('AM', 'RO', 'PA', 'AC', 'RR', 'TO', 'AP') ~ 'Norte',
                            TRUE ~ 'Nordeste')) %>% 
  filter(rede == "Pública") %>% 
  mutate(uf = fct_reorder(uf, ideb_finais_2017, function(x) median(x, na.rm = TRUE)))


finais_brasil %>% 
  ggplot(aes(x = uf, y = ideb_finais_2017, fill = regiao)) +
  geom_boxplot(outlier.shape = 1, outlier.alpha = 0.2) + 
  cagedExplorer::custom_theme() + 
  labs(x = 'Unidade Federativa',
       y = 'Nota IDEB 2017 - Anos Finais',
       title = 'Desempenho das unidades federativas no IDEB 2017 - Anos Finais',
       subtitle = 'Comparação de distribuições de médias municipais',
       caption = 'Fonte: Elaboração própria a partir de dados do INEP.') +
  scale_fill_brewer(palette = 'Accent', name = 'Região') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

# ggsave('plots/boxplot_ideb_finais_uf.png', width = 8, height = 6)
