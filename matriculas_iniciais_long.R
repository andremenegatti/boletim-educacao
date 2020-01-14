library(tidyverse)

matriculas_sp <- readxl::read_excel('data/matriculas_sp.xlsx', sheet = 1,
                                    range = 'A1:H646', col_names = TRUE)

matriculas_iniciais <- matriculas_sp %>% 
  select(-ensino_fundamental_total, -anos_iniciais_total) %>% 
  mutate(anos_iniciais_publica = 
           anos_iniciais_federal + anos_iniciais_estadual + anos_iniciais_municipal) %>% 
  mutate(municipalizacao = anos_iniciais_municipal / anos_iniciais_publica)

glimpse(matriculas_iniciais)

matriculas_iniciais_long <- matriculas_iniciais %>% 
  pivot_longer(cols = contains('anos_'), names_prefix = 'anos_iniciais_',
               names_to = 'rede', values_to = 'matriculas') 

glimpse(matriculas_iniciais_long)

saveRDS(matriculas_iniciais_long, 'data/matriculas_iniciais_long.rds')