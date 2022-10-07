library(dplyr)
library(srvyr) 
library(readr)


setwd("C:\\Users\\luiz.neto\\Documents\\Monografia\\Calculos R")

pnad_covid = read_csv("PNAD_COVID_112020.csv", col_types = cols(.default = "d"))

#pnad_com_pesos = pnad_covid %>% as_survey_design(ids = UPA, strata = Estrato, weights = V1032, nest = TRUE)

# Criando Chaves de Domicílio e Indivíduo
pnad_covid = pnad_covid %>% mutate(
  chave_domicilio = paste(as.character(pnad_covid$UPA),as.character(pnad_covid$V1008),sep=""),
  chave_individuo = paste(as.character(UPA),as.character(V1008),as.character(A001),sep=""))

 
# renda_total = rowSums(pnad_covid[ , c('C01012','D0013','D0023','D0033','D0043','D0053','D0063','D0073')], na.rm=TRUE)

rendas_extras = pnad_covid[c('chave_domicilio', 'chave_individuo','D0013','D0023','D0033','D0043','D0053','D0063','D0073')]
rendas_extras$rendas_extras = rowSums(rendas_extras[ , c('D0013','D0023','D0033','D0043','D0053','D0063','D0073')], na.rm=TRUE)


renda_extra_domiciliar = aggregate(rendas_extras$rendas_extras, by=list(Category=rendas_extras$chave_domicilio), FUN=sum)
colnames(renda_extra_domiciliar) = c('Domicilio', 'Renda Extra')

pessoas_por_domicilio = count(rendas_extras, chave_domicilio)
colnames(pessoas_por_domicilio) = c('Domicilio', 'Numero Pessoas')

info_domicilio = merge(renda_extra_domiciliar, pessoas_por_domicilio)
colnames(info_domicilio) = c('chave_domicilio', 'RendaExtraTotal', 'PessoasDomicilio')

info_domicilio = info_domicilio %>% mutate(
  renda_domiciliar_per_capita = RendaExtraTotal / PessoasDomicilio / PessoasDomicilio
)

renda_extra_domiciliar_per_capita = info_domicilio[c('chave_domicilio', 'renda_domiciliar_per_capita')]
colnames(renda_extra_domiciliar_per_capita) = c('chave_domicilio', 'renda_extra_domiciliar_per_capita')


pnad_covid = merge(pnad_covid, renda_extra_domiciliar_per_capita)


renda_individual = pnad_covid[c('chave_domicilio', 'C01012','renda_extra_domiciliar_per_capita')]
renda_individual$renda_total = rowSums(renda_individual[ , c('C01012', 'renda_extra_domiciliar_per_capita')], na.rm=TRUE)


renda_domiciliar = aggregate(renda_individual$renda_total, by=list(Category=renda_individual$chave_domicilio), FUN=sum)
colnames(renda_domiciliar) = c('Domicilio', 'Renda Domiciliar')


info_domicilio = merge(renda_domiciliar, pessoas_por_domicilio)
colnames(info_domicilio) = c('chave_domicilio', 'RendaTotal', 'PessoasDomicilio')

info_domicilio = info_domicilio %>% mutate(
  renda_domiciliar_per_capita = RendaTotal / PessoasDomicilio
)

renda_dpc = info_domicilio[c('chave_domicilio', 'renda_domiciliar_per_capita')]
colnames(renda_dpc) = c('chave_domicilio', 'renda_domiciliar_per_capita')

pnad_covid = merge(pnad_covid, renda_dpc)


pnad_covid = pnad_covid %>% mutate(
  Covid = if_else(B009B == 1 | B009D == 1 | B009F == 1, 1, 0, 0),
  Nordeste = if_else(UF %in% 21:29, 1, 0, 0),
  Feminino = if_else(A003 == 2, 1, 0, 0),
  Pardo = if_else(A004 == 4, 1, 0, 0),
  Escolaridade = if_else(A005 < 7, 1, 0, 0),
  Grupo_Risco = if_else(B0101 == 1 | B0102 == 1 | B0103 == 1 | B0104 == 1 | B0105 == 1 | B0106 == 1, 1, 0, 0),
  Idoso = if_else(A002 >= 60, 1, 0, 0),
  Pobre = if_else(renda_domiciliar_per_capita < 404.25, 1, 0, 0)
)

bd_regressao = pnad_covid[c('chave_domicilio', 'chave_individuo', 'UPA', 'Estrato', 'V1032', 'Covid', 
                            'Nordeste', 'Feminino', 'Pardo', 'Escolaridade', 'Grupo_Risco', 'Idoso', 'Pobre')]

write.csv(bd_regressao, 'C:\\Users\\luiz.neto\\Documents\\Monografia\\Correção - Defesa\\Calculos R\\Base de Dados - Regressao.csv', row.names=FALSE)