library(dplyr)
library(srvyr) 
library(readr)
library(survey)
library(jtools)

setwd("C:\\Users\\luiz.neto\\Documents\\Monografia\\Correção - Defesa\\Calculos R")

pnad_covid = read_csv("Base de Dados - Regressao.csv", col_types = cols(
  chave_domicilio = col_double(),
  chave_individuo = col_double(),
  UPA = col_double(),
  Estrato = col_double(),
  V1032 = col_double(),
  Covid = col_double(),
  Nordeste = col_double(),
  Feminino = col_double(),
  Pardo = col_double(),
  Escolaridade = col_double(),
  Grupo_Risco = col_double(),
  Idoso = col_double(),
  Pobre = col_double()
))

pnad_com_pesos = pnad_covid %>% as_survey_design(ids = UPA, strata = Estrato, weights = V1032, nest = TRUE)

reg = svyglm(Covid ~ Nordeste + Feminino + Pardo + Escolaridade + Grupo_Risco + Idoso + Pobre,
             family = binomial, design = pnad_com_pesos)

summary(reg, exp=TRUE)

coef(reg)

confint(reg)

exp(coef(reg))

exp(confint(reg))

summ(reg)