

rm(list = ls())
gc()


library(basedosdados)
library(tidyverse)

basedosdados::set_billing_id(
  billing_project_id = Sys.getenv('SENHA_GOOGLE_CLOUND'))


# Finbra UF ----

query <- bdplyr("br_me_siconfi.uf_despesas_funcao")

df <- bd_collect(query)

write_rds(df, 'data-raw/finbra_uf.rds')


# Finbra MUN ----

query <- bdplyr("br_me_siconfi.municipio_despesas_funcao") |>
  filter(ano %in% c(2005:2020), sigla_uf == 'CE')

df <- bd_collect(query)

write_rds(df, 'data-raw/finbra_mun.rds')



# Ideb ----

query <- bdplyr("br_inep_ideb.municipio") |>
  filter(anos_escolares == 'iniciais (1-5)')

df <- bd_collect(query)

write_rds(df, 'data-raw/ideb.rds')


# Populacao ----

query <- bdplyr("br_ibge_populacao.municipio") |>
  filter(sigla_uf == 'CE' & ano %in% c(2007:2020))


df <- bd_collect(query)

write_rds(df, 'data-raw/pop.rds')















