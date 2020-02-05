library(tidyverse)

df_iniciais <- readRDS('data/df_dados_completos_anos_iniciais.rds')

df_sedes <- df_iniciais %>% 
  filter(municipio_clean == cagedExplorer::clean_text(regiao_governo)) %>% 
  select(regiao_governo, ideb_sede = ideb, saeb_sede = saeb,
         matriculas_sede = matriculas)

df_outros <- df_iniciais %>% 
  filter(municipio_clean != cagedExplorer::clean_text(regiao_governo)) %>% 
  group_by(regiao_governo) %>% 
  summarise(ideb_outros = weighted.mean(ideb, w = matriculas, na.rm = TRUE),
            saeb_outros = weighted.mean(saeb, w = matriculas, na.rm = TRUE),
            matriculas_outros = sum(matriculas, na.rm = TRUE))

df_diff <- df_sedes %>% 
  left_join(df_outros, by = 'regiao_governo') %>% 
  mutate(diferenca_ideb = ideb_sede - ideb_outros,
         diferenca_saeb = saeb_sede - saeb_outros) %>% 
  arrange(desc(diferenca_ideb))

df_diff2 <- df_diff %>% 
  select(regiao_governo, ideb_sede, ideb_outros, diferenca_ideb) %>% 
  pivot_longer(cols = c(ideb_sede, ideb_outros), names_to = 'sede',
               values_to = 'ideb', names_prefix = 'ideb_') %>% 
  mutate(diff = ifelse(sede == 'sede', 0, diferenca_ideb),
         sinal = ifelse(diferenca_ideb > 0, 'positivo', 'negativo') %>% 
           fct_relevel('positivo', 'negativo'),
         regiao_governo = fct_reorder(regiao_governo, diferenca_ideb, mean)) %>% 
  arrange(regiao_governo, desc(sede)) %>% 
  mutate(ideb_contra = ifelse(sede == 'sede', lead(ideb), lag(ideb)))

dotplot_sedes <- ggplot(df_diff2) +
  geom_path(aes(y = regiao_governo, x = diff, col = sinal, group = regiao_governo),
            arrow = arrow(length = unit(1.5, 'mm'), end = 'last', type = 'closed'),
            alpha = 0.7) +
  geom_text(aes(x = diff,
                y = regiao_governo, col = 'gray25',
                label = formatC(ideb_contra, digits = 3, big.mark = '.', decimal.mark = ','),
                hjust =
                  case_when(
                    sinal == 'positivo' & sede == 'sede' ~ 1.4,
                    sinal == 'positivo' & sede == 'outros' ~ -0.4,
                    sinal == 'negativo' & sede == 'sede' ~ -0.4,
                    sinal == 'negativo' & sede == 'outros' ~ 1.4
                    )
                ),
            size = 3, family = 'Serif', color = 'gray25') +
  scale_color_manual(values = c('dodgerblue4', 'firebrick'), name = '',
                     labels = c('Sede com nota maior', 'Sede com nota menor')) +
  scale_x_continuous(limits = c(-0.65, 1.15),
                     labels = function(x) formatC(x, big.mark = '.',
                                                  decimal.mark = ',')) +
  custom_theme() +
  theme(panel.grid.major.x = element_blank(), legend.position = 'bottom') +
  labs(
    x = 'Diferença de nota entre sede e média dos outros municípios',
    y = 'Região de Governo',
    title = 'Comparação entre o desempenho das sedes e outros municípios',
    subtitle = 'Regiões de Governo de São Paulo, IDEB 2017 Anos Iniciais',
    caption = 'Notas:
    i) Elaboração própria a partir de dados do INEP;
    iii) A ponta da seta representa a nota da sede e a base, a média dos outros municípios;
    iii) Para o cálculo das médias dos municípios (exceto sede), ponderou-se a nota pelo 
          número de matrículas de cada município.'
  ) ; dotplot_sedes

# ggsave(plot = dotplot_sedes,
#        filename = 'plots/dotplot_sedes.png',
#        height = 8, width = 6.5)