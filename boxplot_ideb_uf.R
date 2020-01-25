library(tidyverse)

ideb_iniciais <- readxl::read_excel("data/divulgacao_anos_iniciais_municipios2017-atualizado-Jun_2019.xlsx",
                                    sheet = 1, range = 'A8:CK14444', col_names = TRUE) %>% 
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
  mutate_at(vars(contains('_iniciais_')), .funs = as.numeric)


iniciais_brasil <- ideb_iniciais %>% 
  select(uf, Codmun7, rede, taxa_aprov_iniciais_2017,
         saeb_iniciais_2017, ideb_iniciais_2017) %>% 
  mutate(regiao = case_when(uf %in% c('PR', 'RS', 'SC') ~ 'Sul',
                            uf %in% c('MS', 'MT', 'GO', 'DF') ~ 'Centro-Oeste',
                            uf %in% c('SP', 'MG', 'ES', 'RJ') ~ 'Sudeste',
                            uf %in% c('AM', 'RO', 'PA', 'AC', 'RR', 'TO', 'AP') ~ 'Norte',
                            TRUE ~ 'Nordeste')) %>% 
  mutate(uf = fct_reorder(uf, ideb_iniciais_2017, function(x) median(x, na.rm = TRUE))) %>% 
  filter(rede == "Pública")



iniciais_brasil %>% 
  ggplot(aes(x = uf, y = ideb_iniciais_2017, fill = regiao)) +
  geom_boxplot(outlier.shape = 1, outlier.alpha = 0.2) + 
  cagedExplorer::custom_theme() + 
  labs(x = 'Unidade Federativa',
       y = 'Nota IDEB 2017 - Anos Iniciais',
       title = 'Desempenho das unidades federativas no IDEB 2017 - Anos Iniciais',
       subtitle = 'Comparação de distribuições de médias municipais',
       caption = 'Fonte: Elaboração própria a partir de dados do INEP.') +
  scale_fill_brewer(palette = 'Accent', name = 'Região') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

# ggsave('plots/boxplot_ideb_iniciais_uf.png', width = 8, height = 6)
