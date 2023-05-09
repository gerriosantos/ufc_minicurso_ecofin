

rm(list = ls())
gc()


library(tidyverse)
library(extrafont)

geo <- geobr::read_municipality(year = 2020) |>
  select(code_muni, mun = name_muni, cod_uf = code_state,
         geom) |>
  mutate(code_muni = as.integer(substr(code_muni, 1, 6)))


f_case <- function(x, v1, v2, v3, v4, cat_1, cat_2,
                   cat_3, cat_4){
  x <- case_when(
    x > v1 & x <= v2 ~ cat_1,
    x > v2 & x <= v3 ~ cat_2,
    x > v3 & x <= v4 ~ cat_3,
    x > v4 ~ cat_4,
  )

  x <- fct_relevel(x, cat_1, cat_2, cat_3, cat_4)

}

d <- haven::read_dta('data-raw/excesso_mortes_full.dta') |>
  filter(data <= '2020-06-01') |>
  group_by(code_muni = CODMUNRES) |>
  summarise(pop19 = first(pop19),
            across(
              morte2015:morte2020,
              ~ sum(.x, na.rm = T)
            )) |> ungroup()



df_mapa_exc_morte <- d |>
  mutate(media_mortes = rowMeans(d[,3:7]),
         excesso_mortes = round(morte2020 - media_mortes,2),
         p_score = round(((morte2020 - media_mortes)/media_mortes)*100, 2),
         excesso = f_case(excesso_mortes, -900, 0, 50, 100,
                          cat_1 = 'Abaixo de 0',
                          cat_2 = 'De 0 a 50',
                          cat_3 = 'De 50 a 100',
                          cat_4 = 'Acima de 100'),
         p_score1 =  f_case(p_score, -92, 0, 15, 30,
                            cat_1 = 'Abaixo de 0%',
                            cat_2 = 'De 0% a 15%',
                            cat_3 = 'De 15% a 30%',
                            cat_4 = 'Acima de 30%'
         )
  )|>
  left_join(geo, by = 'code_muni') |>
  sf::st_as_sf()



f_mapa <- function(data, filtra_uf,
                   uf = c(11:17, 21:29, 31:35, 41:43, 50:53),
                   preencher,
                   titulo = '',
                   limite_barra,
                   breacks_limite,
                   plot.margin){


  data |>
    filter({{filtra_uf}} %in% uf) |>

    ggplot(aes(fill = {{preencher}}))+
    geom_sf(color = NA)+
    # geom_sf(data = reg_plan_ce,
    #         fill = "transparent",
    #         colour = "white", size = 0.5)+
    theme_minimal()+
    #scale_fill_gradient(limits = c(-100, 100))+
    scale_fill_viridis_d(alpha = 0.9, begin = 0.8,
                         # guide = guide_legend(
                         #   nrow = 1,
                         #   label.position = 'bottom',
                         #   direction = "horizontal",
                         #   label.hjust = 0),
                         end = 0.3,
                         # limits = limite_barra,
                         # breaks = breacks_limite,
                         na.value = 'grey')+
    theme(legend.position = 'bottom',
          text = element_text(family="Times New Roman",
                              color="black",
                              size=12, face="bold"),
          legend.direction = "horizontal",
          legend.text = element_text(size = 14),
          title = element_text(size = 18),
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          plot.margin = plot.margin) +
    labs(title = titulo,
         fill = '')
}


library(patchwork)

g_e1 <- f_mapa(data = df_mapa_exc_morte,
               filtra_uf = cod_uf, uf = 23,
               preencher = excesso,
               titulo = 'Excesso de Mortes',
               plot.margin = margin(0,2,0,0, 'cm'))

ggsave(filename = 'excesso_morteCE.png',
       path = 'figures/',
       plot = g_e1,
       width = 1280, height = 830,
       units = 'px', scale = 3)


g_p1 <- f_mapa(data = df_mapa_exc_morte,
               filtra_uf = cod_uf, uf = 23,
               preencher = p_score1,
               titulo = 'P-Score',
               plot.margin = margin(0,0,0,2, 'cm'))

ggsave(filename = 'p_scoreCE.png',
       path = 'figures/',
       plot = g_p1,
       width = 1280, height = 830,
       units = 'px', scale = 3)




g_e2 <- f_mapa(data = df_mapa_exc_morte,
               filtra_uf = cod_uf,
               preencher = excesso,
               # limite_barra = c(0,5565),
               # breacks_limite = c(100, 3000, 5500),
               titulo = 'Excesso de Mortes',
               plot.margin = margin(0,2,0,0, 'cm'))

ggsave(filename = 'excesso_morteBR.png',
       path = 'figures/',
       plot = g_e2,
       width = 1280, height = 830,
       units = 'px', scale = 3)

g_p2 <- f_mapa(data = df_mapa_exc_morte,
               filtra_uf = cod_uf,
               preencher = p_score1,
               # limite_barra = c(0,200),
               # breacks_limite = c(-47, -20, 0, 20, 47),
               titulo = 'P-Score',
               plot.margin = margin(0,0,0,2, 'cm'))

ggsave(filename = 'p_scoreBR.png',
       path = 'figures/',
       plot = g_p2,
       width = 1280, height = 830,
       units = 'px', scale = 3)
