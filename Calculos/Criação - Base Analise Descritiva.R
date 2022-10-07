library(dplyr)
library(srvyr) 
library(readr)


setwd("/Users/luiz.leao/Documents/Estudo/UFPE/Monografia/Correção - Defesa/Calculos R")

pnad_covid = read_csv("PNAD_COVID_112020.csv", col_types = cols(.default = "d"))


# Criando Chaves de Domic?lio e Indiv?duo
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
colnames(renda_domiciliar) = c('chave_domicilio', 'renda_domiciliar')


pnad_covid = merge(pnad_covid, renda_domiciliar)

pnad_covid$renda_domiciliar = as.numeric(as.character(pnad_covid$renda_domiciliar))

pnad_covid = pnad_covid %>% mutate(
  Covid = if_else(B009B == 1 | B009D == 1 | B009F == 1, 1, 0, 0),
  
  Classe_Social = factor(case_when(
    renda_domiciliar <= 2090 ~ "E",
    renda_domiciliar %in% c(2091:5225) ~ "D",
    renda_domiciliar %in% c(5226:10450) ~ "C",
    renda_domiciliar %in% c(10451:20900) ~ "B",
    renda_domiciliar >= 20901 ~ "A"),
    levels = c("E", "D", "C", "B","A")),
  
  Faixa_Etaria = case_when(
    A002 <= 19 ~ "Ate 19 anos",
    A002 %in% 20:59 ~ "20 a 59 anos", 
    A002 >= 60 ~ "60 anos ou mais"),
  
  Regiao = case_when(
    UF %in% 11:17 ~ "Norte",
    UF %in% 21:29 ~ "Nordeste",
    UF %in% 31:35 ~ "Sudeste",
    UF %in% 41:43 ~ "Sul",
    UF %in% 50:53 ~ "Centro-Oeste"),
  
  Sexo = ifelse(A003 == 1, "Homem", "Mulher"),
  
  Raca = case_when(
    A004 == 1 ~ "Branca",
    A004 == 2 ~ "Preta",
    A004 == 3 ~ "Amarela",
    A004 == 4 ~ "Parda",
    A004 == 5 ~ "Indigena",
    A004 == 9 ~ "Ignorado"),
  
  Escolaridade = factor(case_when( 
    A005 == 1 ~ "Sem Instrucao",
    A005 == 2 ~ "Fundamental Incompleto",
    A005 == 3 ~ "Fundamental completo",
    A005 == 4 ~ "Medio Incompleto",
    A005 == 5 ~ "Medio completo",
    A005 == 6 ~ "Superior Incompleto",
    A005 == 7 ~ "Superior completo", 
    A005 == 8 ~ "Pos-graduacao"), 
    levels = c( "Sem Instrucao", 
                "Fundamental Incompleto",
                "Fundamental completo",
                "Medio Incompleto", 
                "Medio completo",
                "Superior Incompleto",
                "Superior completo",
                "Pos-graduacao")),
  
  Grupo_Risco = ifelse(B0101 == 1 | B0102 == 1 | B0103 == 1 | B0104 == 1 | B0105 == 1 | B0106 == 1, "Sim", "Nao"))



bd_descritiva = pnad_covid[c('chave_domicilio', 'chave_individuo', 'UPA', 'Estrato', 'V1032', 'Covid', 
                            'Classe_Social', 'Faixa_Etaria', 'Regiao', 'Sexo', 'Raca', 'Escolaridade', 'Grupo_Risco')]


write.csv(bd_descritiva, '/Users/luiz.leao/Documents/Estudo/UFPE/Monografia/Correção - Defesa/Calculos R/Base de Dados - Analise Descritiva.csv', row.names = FALSE)

teste = pnad_covid



