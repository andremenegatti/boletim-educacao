library(tidyverse)

ideb_medio <- readxl::read_excel("~/backup_windows/Desktop/USP_Municipios/Dados/IDEB/divulgacao_ensino_medio_municipios2017-atualizado-Jun_2019.xlsx",
                                 sheet = 1, range = 'A7:O11269') %>% 
  select(uf = SG_UF, Codmun7 = COD_MUN, rede = REDE, ideb_medio_2017 = IDEB12_17,
         taxa_aprov_medio_2017 = TAP_MED, ind_rend_medio_2017 = P12,
         saeb_medio_2017 = PAD12_17) %>%
  mutate_at(vars(contains("_medio_2017")), as.numeric) %>% 
  filter(uf == "SP") %>% 
  select(-uf)

glimpse(ideb_medio)

ideb_finais <- readxl::read_excel("~/backup_windows/Desktop/USP_Municipios/Dados/IDEB/divulgacao_anos_finais_municipios2017-atualizado-Jun_2019.xlsx",
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
  mutate_at(vars(starts_with('saeb_finais_')), .funs = as.numeric) %>% 
  filter(uf == "SP") %>% 
  select(-uf)

glimpse(ideb_medio)

ideb_iniciais <- readxl::read_excel("~/backup_windows/Desktop/USP_Municipios/Dados/IDEB/divulgacao_anos_iniciais_municipios2017-atualizado-Jun_2019.xlsx",
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
  mutate_at(vars(contains('_iniciais_')), .funs = as.numeric) %>% 
  filter(uf == "SP") %>% 
  select(-uf)

glimpse(ideb_iniciais)
# saveRDS(ideb_iniciais, 'df_ideb_iniciais.rds')

glimpse(ideb_medio)
# saveRDS(ideb_medio, 'df_ideb_medio.rds')
