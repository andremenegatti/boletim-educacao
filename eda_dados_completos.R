library(cagedExplorer)
source("dotplot_regions.R")

df_iniciais <- readRDS('data/df_dados_completos_anos_iniciais.rds')

# EDA -------------------------------------------------------------------------
# Agregando por regioes
df_regioes <- df_iniciais %>% 
  group_by(regiao_governo) %>% 
  summarise(ideb = weighted.mean(ideb, w = pop_ibge, na.rm = TRUE),
            saeb = weighted.mean(saeb, w = pop_ibge, na.rm = TRUE),
            perc_adequado = weighted.mean(perc_adequado_avg,
                                          w = matriculados_5ano,
                                          na.rm = TRUE),
            matriculas_5ano = sum(matriculados_5ano, na.rm = TRUE),
            pop = sum(pop_ibge),
            perc_despesas_educacao = sum(despesas_educacao) / sum(despesas_totais),
            perc_desp_educ_pib = (sum(despesas_educacao) / 1e+3) / sum(pib),
            despesas_educacao_pc = sum(despesas_educacao) / sum(pop_ibge),
            ird = mean(nota_ird),
            doc_sup = mean(doc_sup),
            pib_pc = sum(pib) / sum(pop_ibge))

media_ideb <- weighted.mean(df_iniciais$ideb, w = df_iniciais$pop_ibge)
dotplot_regions(df_regioes, var_plot = ideb, palette = 'steelblue', vline = media_ideb)

dotplot_regions(df_regioes, var_plot = perc_adequado, vline = mean(df_regioes$perc_adequado))
dotplot_regions(df_regioes, var_plot = perc_despesas_educacao, vline = mean(df_regioes$perc_despesas_educacao))
dotplot_regions(df_regioes, var_plot = perc_desp_educ_pib, vline = mean(df_regioes$perc_desp_educ_pib))
dotplot_regions(df_regioes, var_plot = doc_sup, vline = mean(df_regioes$doc_sup))

p1 <- ggplot(df_regioes,
             aes(x = ird, y = perc_adequado,
                 fill = regiao_governo, size = pop)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  custom_theme() +
  theme(legend.position = 'none')

plotly::ggplotly(p1)


ggplot(df_regioes,
       aes(x = doc_sup, y = perc_adequado)) +
  geom_point() +
  geom_smooth(method = 'lm', col = 'red', se = FALSE) +
  custom_theme() +
  scale_x_continuous(labels = function(x) paste0(x, '%')) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  labs(x = 'Docentes com curso superior',
       y = 'Alunos com desempenho adequado',
       title = 'Relação entre formação dos docentes e desempenho dos alunos',
       subtitle = 'Regiões de Governo de São Paulo',
       caption = 'Fonte: Elaboração própria a partir de dados do INEP e do QEdu.')

ggsave("dsu_vs_desempenho_adequado.png", height = 6, width = 6)

ggplot(df_regioes,
       aes(x = perc_desp_educ_pib, y = ideb)) +
  geom_point() +
  geom_smooth(method = 'lm')

codigos_sedes <- municipios_sp %>% 
  filter(municipio %in% regiao_governo) %>% 
  select(Codmun7) %>% unlist()

df_sedes <- df_iniciais %>%
  filter(Codmun7 %in% codigos_sedes) %>% 
  mutate(perc_desp_educ_pib = (despesas_educacao / 1e+3) / pib)

media_ideb <- mean(df_sedes$ideb)
dotplot_regions(df_sedes, var_plot = ideb, palette = 'steelblue', vline = media_ideb)
dotplot_regions(df_sedes, var_plot = perc_despesas_educacao, vline = mean(df_sedes$perc_despesas_educacao))
dotplot_regions(df_sedes, var_plot = perc_desp_educ_pib, vline = mean(df_sedes$perc_desp_educ_pib))



p1 <- ggplot(df_iniciais,
             aes(x = perc_despesas_educacao, y = saeb,
                 fill = regiao_governo, size = pop_ibge)) +
  geom_point() +
  custom_theme() +
  theme(legend.position = 'none')

plotly::ggplotly(p1)