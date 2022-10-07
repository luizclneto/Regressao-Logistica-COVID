library(dplyr)
library(srvyr) 
library(readr)


setwd("C:\\Users\\luiz.neto\\Documents\\Monografia\\Correção - Defesa\\Calculos R")

pnad_covid = read_csv("Base de Dados - Analise Descritiva.csv", col_types = cols(
  chave_domicilio = col_double(),
  chave_individuo = col_double(),
  UPA = col_double(),
  Estrato = col_double(),
  V1032 = col_double(),
  Covid = col_double(),
  Classe_Social = col_character(),
  Faixa_Etaria = col_character(),
  Regiao = col_character(),
  Sexo = col_character(),
  Raca = col_character(),
  Escolaridade = col_character(),
  Grupo_Risco = col_character()
))

pnad_com_pesos = pnad_covid %>% as_survey_design(ids = UPA, strata = Estrato, weights = V1032, nest = TRUE)












analise_classe = pnad_com_pesos %>%
  group_by(Classe_Social, Covid) %>%
  survey_tally(name="Observacoes")
write.csv(analise_classe, 'C:\\Users\\luiz.neto\\Documents\\Monografia\\Correção - Defesa\\Calculos R\\Analise - Classe Social.csv', row.names=FALSE)

analise_faixa = pnad_com_pesos %>%
  group_by(Faixa_Etaria, Covid) %>%
  survey_tally(name="Observacoes")
write.csv(analise_faixa, 'C:\\Users\\luiz.neto\\Documents\\Monografia\\Correção - Defesa\\Calculos R\\Analise - Faixa Etaria.csv', row.names=FALSE)

analise_regiao = pnad_com_pesos %>%
  group_by(Regiao, Covid) %>%
  survey_tally(name="Observacoes")
write.csv(analise_regiao, 'C:\\Users\\luiz.neto\\Documents\\Monografia\\Correção - Defesa\\Calculos R\\Analise - Regiao.csv', row.names=FALSE)

analise_sexo = pnad_com_pesos %>%
  group_by(Sexo, Covid) %>%
  survey_tally(name="Observacoes")
write.csv(analise_sexo, 'C:\\Users\\luiz.neto\\Documents\\Monografia\\Correção - Defesa\\Calculos R\\Analise - Genero.csv', row.names=FALSE)

analise_raca = pnad_com_pesos %>%
  group_by(Raca, Covid) %>%
  survey_tally(name="Observacoes")
write.csv(analise_raca, 'C:\\Users\\luiz.neto\\Documents\\Monografia\\Correção - Defesa\\Calculos R\\Analise - Raca.csv', row.names=FALSE)

analise_escolaridade = pnad_com_pesos %>%
  group_by(Escolaridade, Covid) %>%
  survey_tally(name="Observacoes")
write.csv(analise_escolaridade, 'C:\\Users\\luiz.neto\\Documents\\Monografia\\Correção - Defesa\\Calculos R\\Analise - Escolaridade.csv', row.names=FALSE)

analise_grupo = pnad_com_pesos %>%
  group_by(Grupo_Risco, Covid) %>%
  survey_tally(name="Observacoes")
write.csv(analise_grupo, 'C:\\Users\\luiz.neto\\Documents\\Monografia\\Correção - Defesa\\Calculos R\\Analise - Grupo de Risco.csv', row.names=FALSE)