#comando pra limpar a memoria
rm(list=ls(all=TRUE)) 

#bibliotecas
require(graphics)
library(forecast)
library(TTR)
library(glmnet) 
library(dplyr)   
library(psych)   
library(mvtnorm)
library(excelR)
library(elmNNRcpp)
library(readxl)
library(stringr)

##define diretorio correto: (modificar para a pasta que estao os arquivos)
#arquivos .R devem estar na mesma pasta que todos os arquivos da base
setwd("~/Facul/10º Período/TCC 2")

# Carregando dados de sessão anterior 
# (comentar caso seja a primeira vez rodando o script e alterar caminho do arquivo se necessário)
load("~/Facul/10º Período/TCC 2/.RData")

#suprime warning mensages
options(warn=-1)

#lendo fun??es auxiliares

#verifica se bibliotecas est?o instaladas
requiredPackages<-c("tcltk","graphics","forecast","TTR","graphics","ggplot2",
                    "dplyr","psych","metaheuristicOpt","pso","psoptim","tidyr",
                    "mvtnorm","excelR","elmNNRcpp","stringr","DEoptim","kernlab","nnfor")

#carrega os pacotes
for (p in requiredPackages){
  if(!require(p,character.only = TRUE)) 
    install.packages(p)
  library(p,character.only = TRUE)
}


Dataset_PRF_2007 <- read_excel("~/Facul/10º Período/TCC 2/Dataset_raw/Dataset PRF 2007-2020.xlsx", sheet = "2007")
Dataset_PRF_2008 <- read_excel("~/Facul/10º Período/TCC 2/Dataset_raw/Dataset PRF 2007-2020.xlsx", sheet = "2008")
Dataset_PRF_2009 <- read_excel("~/Facul/10º Período/TCC 2/Dataset_raw/Dataset PRF 2007-2020.xlsx", sheet = "2009")
Dataset_PRF_2010 <- read_excel("~/Facul/10º Período/TCC 2/Dataset_raw/Dataset PRF 2007-2020.xlsx", sheet = "2010")
Dataset_PRF_2011 <- read_excel("~/Facul/10º Período/TCC 2/Dataset_raw/Dataset PRF 2007-2020.xlsx", sheet = "2011")
Dataset_PRF_2012 <- read_excel("~/Facul/10º Período/TCC 2/Dataset_raw/Dataset PRF 2007-2020.xlsx", sheet = "2012")
Dataset_PRF_2013 <- read_excel("~/Facul/10º Período/TCC 2/Dataset_raw/Dataset PRF 2007-2020.xlsx", sheet = "2013")
Dataset_PRF_2014 <- read_excel("~/Facul/10º Período/TCC 2/Dataset_raw/Dataset PRF 2007-2020.xlsx", sheet = "2014")
Dataset_PRF_2015 <- read_excel("~/Facul/10º Período/TCC 2/Dataset_raw/Dataset PRF 2007-2020.xlsx", sheet = "2015")
Dataset_PRF_2016 <- read_excel("~/Facul/10º Período/TCC 2/Dataset_raw/Dataset PRF 2007-2020.xlsx", sheet = "2016")
Dataset_PRF_2017 <- read_excel("~/Facul/10º Período/TCC 2/Dataset_raw/Dataset PRF 2007-2020.xlsx", sheet = "2017")
Dataset_PRF_2018 <- read_excel("~/Facul/10º Período/TCC 2/Dataset_raw/Dataset PRF 2007-2020.xlsx", sheet = "2018")
Dataset_PRF_2019 <- read_excel("~/Facul/10º Período/TCC 2/Dataset_raw/Dataset PRF 2007-2020.xlsx", sheet = "2019")


View(Dataset_PRF_2007)
View(Dataset_PRF_2008)
View(Dataset_PRF_2009)
View(Dataset_PRF_2010)
View(Dataset_PRF_2011)
View(Dataset_PRF_2012)
View(Dataset_PRF_2013)
View(Dataset_PRF_2014)
View(Dataset_PRF_2015)
View(Dataset_PRF_2016)
View(Dataset_PRF_2017)
View(Dataset_PRF_2018)
View(Dataset_PRF_2019)

Dataset_PRF_2007$ano <- 2007
Dataset_PRF_2008$ano <- 2008
Dataset_PRF_2009$ano <- 2009
Dataset_PRF_2010$ano <- 2010
Dataset_PRF_2011$ano <- 2011
Dataset_PRF_2012$ano <- 2012
Dataset_PRF_2013$ano <- 2013
Dataset_PRF_2014$ano <- 2014
Dataset_PRF_2015$ano <- 2015
Dataset_PRF_2016$ano <- 2016
Dataset_PRF_2017$ano <- 2017
Dataset_PRF_2018$ano <- 2018
Dataset_PRF_2019$ano <- 2019

Dataset_PRF_all <- rbind(
  Dataset_PRF_2007, Dataset_PRF_2008, Dataset_PRF_2009, Dataset_PRF_2010,
  Dataset_PRF_2011, Dataset_PRF_2012, Dataset_PRF_2013, Dataset_PRF_2014,
  Dataset_PRF_2015, Dataset_PRF_2016, Dataset_PRF_2017, Dataset_PRF_2018, 
  Dataset_PRF_2019)

Dataset_PRF_all <- Dataset_PRF_all %>% select(-id, -horario, -data_inversa) %>% as.matrix()

View(Dataset_PRF_all)

##########################################################################################################################################

##Normalização das colunas da base de dados
# tirar acentuação e caracteres com problemas de encode para não dar problema com a decodificação do csv
# OBS: salvar esse script com encode UTF-8

for(i in 1:nrow(Dataset_PRF_all)) {
  #horario
  #Dataset_PRF_all[i,'horario'] = str_sub(Dataset_PRF_all[i,'horario'], start = 12)
  
  #dia_semana
  if(Dataset_PRF_all[i,'dia_semana'] == 'Domingo'){
    Dataset_PRF_all[i,'dia_semana'] = 'domingo'
  }
  if(Dataset_PRF_all[i,'dia_semana'] == 'Segunda'){
    Dataset_PRF_all[i,'dia_semana'] = 'segunda-feira'
  }
  if((Dataset_PRF_all[i,'dia_semana'] == 'TerÁa') || (Dataset_PRF_all[i,'dia_semana'] == 'terÁa-feira')){
    Dataset_PRF_all[i,'dia_semana'] = 'terca-feira'
  }
  if(Dataset_PRF_all[i,'dia_semana'] == 'Quarta'){
    Dataset_PRF_all[i,'dia_semana'] = 'quarta-feira'
  }
  if(Dataset_PRF_all[i,'dia_semana'] == 'Quinta'){
    Dataset_PRF_all[i,'dia_semana'] = 'quinta-feira'
  }
  if(Dataset_PRF_all[i,'dia_semana'] == 'Sexta'){
    Dataset_PRF_all[i,'dia_semana'] = 'sexta-feira'
  }
  if((Dataset_PRF_all[i,'dia_semana'] == 's·bado') || (Dataset_PRF_all[i,'dia_semana'] == 'S·bado')){
    Dataset_PRF_all[i,'dia_semana'] = 'sabado'
  }
  
  #fase_dia
  if(!is.na(Dataset_PRF_all[i,'fase_dia']) && !is.null(Dataset_PRF_all[i,'fase_dia'])){
    if(Dataset_PRF_all[i,'fase_dia'] == 'Plena Noite'){
      Dataset_PRF_all[i,'fase_dia'] = 'Plena noite'
    }
  } else {
    Dataset_PRF_all[i,'fase_dia'] = '(null)'
  }
  
  #causa_acidente
  if(!is.na(Dataset_PRF_all[i,'causa_acidente'])){
    if(Dataset_PRF_all[i,'causa_acidente'] == 'Agress„o Externa'){
      Dataset_PRF_all[i,'causa_acidente'] = 'Agressao Externa'
    }
    if((Dataset_PRF_all[i,'causa_acidente'] == 'Defeito mec‚nico em veÌculo') || (Dataset_PRF_all[i,'causa_acidente'] == 'Defeito Mec‚nico no VeÌculo')){
      Dataset_PRF_all[i,'causa_acidente'] = 'Defeito Mecanico em Veiculo'
    }
    if(Dataset_PRF_all[i,'causa_acidente'] == 'Defeito na via'){
      Dataset_PRF_all[i,'causa_acidente'] = 'Defeito na Via'
    }
    if(Dataset_PRF_all[i,'causa_acidente'] == 'DeficiÍncia ou n„o Acionamento do Sistema de IluminaÁ„o/SinalizaÁ„o do VeÌculo'){
      Dataset_PRF_all[i,'causa_acidente'] = 'Deficiencia ou nao Acionamento do Sistema de Iluminacao/Sinalizacao do Veiculo'
    }
    if(Dataset_PRF_all[i,'causa_acidente'] == 'DesobediÍncia ‡ sinalizaÁ„o'){
      Dataset_PRF_all[i,'causa_acidente'] = 'Desobediencia da sinalizacao'
    }
    if(Dataset_PRF_all[i,'causa_acidente'] == 'DesobediÍncia ‡s normas de tr‚nsito pelo condutor'){
      Dataset_PRF_all[i,'causa_acidente'] = 'Desobediencia das normas de transito pelo condutor'
    }
    if(Dataset_PRF_all[i,'causa_acidente'] == 'DesobediÍncia ‡s normas de tr‚nsito pelo pedestre'){
      Dataset_PRF_all[i,'causa_acidente'] = 'Desobediencia das normas de transito pelo pedestre'
    }
    if(Dataset_PRF_all[i,'causa_acidente'] == 'Dormindo'){
      Dataset_PRF_all[i,'causa_acidente'] = 'Condutor Dormindo'
    }
    if(Dataset_PRF_all[i,'causa_acidente'] == 'Falta de atenÁ„o'){
      Dataset_PRF_all[i,'causa_acidente'] = 'Falta de Atencao'
    }
    if(Dataset_PRF_all[i,'causa_acidente'] == 'Falta de AtenÁ„o ‡ ConduÁ„o'){
      Dataset_PRF_all[i,'causa_acidente'] = 'Falta de Atencao do Condutor'
    }
    if(Dataset_PRF_all[i,'causa_acidente'] == 'Falta de AtenÁ„o do Pedestre'){
      Dataset_PRF_all[i,'causa_acidente'] = 'Falta de Atencao do Pedestre'
    }
    if(Dataset_PRF_all[i,'causa_acidente'] == 'FenÙmenos da Natureza'){
      Dataset_PRF_all[i,'causa_acidente'] = 'Fenomenos da Natureza'
    }
    if((Dataset_PRF_all[i,'causa_acidente'] == 'Ingest„o de ¡lcool') || (Dataset_PRF_all[i,'causa_acidente'] == 'Ingest„o de ·lcool')){
      Dataset_PRF_all[i,'causa_acidente'] = 'Ingestao de Alcool'
    }
    if(Dataset_PRF_all[i,'causa_acidente'] == 'Ingest„o de ·lcool e/ou subst‚ncias psicoativas pelo pedestre'){
      Dataset_PRF_all[i,'causa_acidente'] = 'Ingestao de Alcool e/ou Substancias Psicoativas pelo Pedestre'
    }
    if(Dataset_PRF_all[i,'causa_acidente'] == 'Ingest„o de Subst‚ncias Psicoativas'){
      Dataset_PRF_all[i,'causa_acidente'] = 'Ingestao de Substancias Psicoativas'
    }
    if(Dataset_PRF_all[i,'causa_acidente'] == 'Mal S˙bito'){
      Dataset_PRF_all[i,'causa_acidente'] = 'Mal Subito'
    }
    if(Dataset_PRF_all[i,'causa_acidente'] == 'N„o guardar dist‚ncia de seguranÁa'){
      Dataset_PRF_all[i,'causa_acidente'] = '	Nao guardar distancia de seguranca'
    }
    if(Dataset_PRF_all[i,'causa_acidente'] == 'Objeto est·tico sobre o leito carroÁ·vel'){
      Dataset_PRF_all[i,'causa_acidente'] = 'Objeto estatico sobre o leito'
    }
    if(Dataset_PRF_all[i,'causa_acidente'] == 'RestriÁ„o de Visibilidade'){
      Dataset_PRF_all[i,'causa_acidente'] = 'Baixa Visibilidade'
    }
    if(Dataset_PRF_all[i,'causa_acidente'] == 'SinalizaÁ„o da via insuficiente ou inadequada'){
      Dataset_PRF_all[i,'causa_acidente'] = 'Sinalizacao da Via Insuficiente ou Inadequada'
    }
    if(Dataset_PRF_all[i,'causa_acidente'] == 'Ultrapassagem indevida'){
      Dataset_PRF_all[i,'causa_acidente'] = 'Ultrapassagem Indevida'
    }
    if((Dataset_PRF_all[i,'causa_acidente'] == 'Velocidade incompatÌvel') || (Dataset_PRF_all[i,'causa_acidente'] == 'Velocidade IncompatÌvel')){
      Dataset_PRF_all[i,'causa_acidente'] = 'Velocidade Incompativel'
    }
  } else {
    Dataset_PRF_all[i,'causa_acidente'] = '(null)'
  }
  
  #tipo_acidente
  if(!is.na(Dataset_PRF_all[i,'tipo_acidente'])){
    if(Dataset_PRF_all[i,'tipo_acidente'] == 'Atropelamento de animal'){
      Dataset_PRF_all[i,'tipo_acidente'] = 'Atropelamento de Animal'
    }
    if(Dataset_PRF_all[i,'tipo_acidente'] == 'Atropelamento de pessoa'){
      Dataset_PRF_all[i,'tipo_acidente'] = 'Atropelamento de Pedestre'
    }
    if(Dataset_PRF_all[i,'tipo_acidente'] == 'Colis„o com bicicleta'){
      Dataset_PRF_all[i,'tipo_acidente'] = 'Colisao com bicicleta'
    }
    if(Dataset_PRF_all[i,'tipo_acidente'] == 'Colis„o com objeto em movimento'){
      Dataset_PRF_all[i,'tipo_acidente'] = 'Colisao com objeto em movimento'
    }
    if(Dataset_PRF_all[i,'tipo_acidente'] == 'Colis„o com objeto est·tico'){
      Dataset_PRF_all[i,'tipo_acidente'] = 'Colisao com objeto estatico'
    }
    if(Dataset_PRF_all[i,'tipo_acidente'] == 'Colis„o com objeto fixo'){
      Dataset_PRF_all[i,'tipo_acidente'] = 'Colisao com objeto fixo'
    }
    if(Dataset_PRF_all[i,'tipo_acidente'] == 'Colis„o com objeto mÛvel'){
      Dataset_PRF_all[i,'tipo_acidente'] = 'Colisao com objeto movel'
    }
    if(Dataset_PRF_all[i,'tipo_acidente'] == 'Colis„o frontal'){
      Dataset_PRF_all[i,'tipo_acidente'] = 'Colisao frontal'
    }
    if(Dataset_PRF_all[i,'tipo_acidente'] == 'Colis„o lateral'){
      Dataset_PRF_all[i,'tipo_acidente'] = 'Colisao lateral'
    }
    if((Dataset_PRF_all[i,'tipo_acidente'] == 'Colis„o transversal') || (Dataset_PRF_all[i,'tipo_acidente'] == 'Colis„o Transversal')){
      Dataset_PRF_all[i,'tipo_acidente'] = 'Colisao transversal'
    }
    if(Dataset_PRF_all[i,'tipo_acidente'] == 'Colis„o traseira'){
      Dataset_PRF_all[i,'tipo_acidente'] = 'Colisao traseira'
    }
    if(Dataset_PRF_all[i,'tipo_acidente'] == 'Danos Eventuais'){
      Dataset_PRF_all[i,'tipo_acidente'] = 'Danos eventuais'
    }
    if(Dataset_PRF_all[i,'tipo_acidente'] == 'Derramamento de Carga'){
      Dataset_PRF_all[i,'tipo_acidente'] = 'Derramamento de carga'
    }
    if(Dataset_PRF_all[i,'tipo_acidente'] == 'IncÍndio'){
      Dataset_PRF_all[i,'tipo_acidente'] = 'Incendio'
    }
    if(Dataset_PRF_all[i,'tipo_acidente'] == 'Queda de motocicleta / bicicleta / veÌculo'){
      Dataset_PRF_all[i,'tipo_acidente'] = 'Queda de motocicleta / bicicleta / veiculo'
    }
    if(Dataset_PRF_all[i,'tipo_acidente'] == 'Queda de ocupante de veÌculo'){
      Dataset_PRF_all[i,'tipo_acidente'] = 'Queda de ocupante de veiculo'
    }
    if(Dataset_PRF_all[i,'tipo_acidente'] == 'SaÌda de leito carroÁ·vel'){
      Dataset_PRF_all[i,'tipo_acidente'] = 'Saida de leito'
    }
    if(Dataset_PRF_all[i,'tipo_acidente'] == 'SaÌda de Pista'){
      Dataset_PRF_all[i,'tipo_acidente'] = 'Saida de Pista'
    }
  } else {
    Dataset_PRF_all[i,'tipo_acidente'] = '(null)'
  }
  
  #classificacao_acidentes
  if(strtoi(Dataset_PRF_all[i,'mortos']) > 0){
    Dataset_PRF_all[i,'classificacao_acidente'] = "mortos"
  } else {
    if(strtoi(Dataset_PRF_all[i,'feridos_graves']) > 0){
      Dataset_PRF_all[i,'classificacao_acidente'] = "feridos_graves"
    } else {
      if(strtoi(Dataset_PRF_all[i,'feridos_leves']) > 0){
        Dataset_PRF_all[i,'classificacao_acidente'] = "feridos_leves"
      } else{
        Dataset_PRF_all[i,'classificacao_acidente'] = "ilesos"
      }
    }
  }
  
  #sentido_via
  if((is.na(Dataset_PRF_all[i,'sentido_via'])) || (Dataset_PRF_all[i,'sentido_via'] == 'N„o Informado')){
    Dataset_PRF_all[i,'sentido_via'] = 'Nao Informado'
  }
  
  #condicao_metereologica
  if((!is.na(Dataset_PRF_all[i,'condicao_metereologica'])) && (!is.null(Dataset_PRF_all[i,'condicao_metereologica']))){
    if(Dataset_PRF_all[i,'condicao_metereologica'] == 'Nevoeiro/neblina'){
      Dataset_PRF_all[i,'condicao_metereologica'] = 'Nevoeiro/Neblina'
    }
    if(Dataset_PRF_all[i,'condicao_metereologica'] == 'CÈu Claro'){
      Dataset_PRF_all[i,'condicao_metereologica'] = 'Ceu Claro'
    }
    if((Dataset_PRF_all[i,'condicao_metereologica'] == 'Ignorado') || (Dataset_PRF_all[i,'condicao_metereologica'] == '(null)')){
      Dataset_PRF_all[i,'condicao_metereologica'] = 'Ignorada'
    }
  } else {
    Dataset_PRF_all[i,'condicao_metereologica'] = 'Ignorada'
  }
  
  #tipo_pista
  if((!is.na(Dataset_PRF_all[i,'tipo_pista'])) && (!is.null(Dataset_PRF_all[i,'tipo_pista']))){
    if(Dataset_PRF_all[i,'tipo_pista'] == 'M˙ltipla'){
      Dataset_PRF_all[i,'tipo_pista'] = 'Multipla'
    }
  } else {
    Dataset_PRF_all[i,'tipo_pista'] = '(null)'
  }
  
  #tracado_via
  if((!is.na(Dataset_PRF_all[i,'tracado_via'])) && (!is.null(Dataset_PRF_all[i,'tracado_via']))){
    if(Dataset_PRF_all[i,'tracado_via'] == 'Desvio Tempor·rio'){
      Dataset_PRF_all[i,'tracado_via'] = 'Desvio Temporario'
    }
    if(Dataset_PRF_all[i,'tracado_via'] == 'InterseÁ„o de vias'){
      Dataset_PRF_all[i,'tracado_via'] = 'Intersecao de vias'
    }
    if(Dataset_PRF_all[i,'tracado_via'] == 'Desvio Tempor·rio'){
      Dataset_PRF_all[i,'tracado_via'] = 'Desvio Temporario'
    }
    if(Dataset_PRF_all[i,'tracado_via'] == 'RotatÛria'){
      Dataset_PRF_all[i,'tracado_via'] = 'Rotatoria'
    }
    if(Dataset_PRF_all[i,'tracado_via'] == 'T˙nel'){
      Dataset_PRF_all[i,'tracado_via'] = 'Tunel'
    }
    if(Dataset_PRF_all[i,'tracado_via'] == 'N„o Informado' || Dataset_PRF_all[i,'tracado_via'] == '(null)'){
      Dataset_PRF_all[i,'tracado_via'] = 'Nao Informado'
    }
  } else {
    Dataset_PRF_all[i,'tracado_via'] = 'Nao Informado'
  }
  
  #uso_solo
  if((!is.na(Dataset_PRF_all[i,'uso_solo'])) && (!is.null(Dataset_PRF_all[i,'uso_solo']))){
    if(Dataset_PRF_all[i,'uso_solo'] == 'N„o'){
      Dataset_PRF_all[i,'uso_solo'] = 'Nao'
    }
  } else {
    Dataset_PRF_all[i,'uso_solo'] = '(null)'
  }
  
  #km
  Dataset_PRF_all[i,'km'] = as.numeric(Dataset_PRF_all[i,'km'])
  
  #br
  Dataset_PRF_all[i,'br'] = as.integer(Dataset_PRF_all[i,'br'])
  
}

View(Dataset_PRF_all)

X_all_strings = subset(Dataset_PRF_all, select = -c(classificacao_acidente))
y_all_strings <- Dataset_PRF_all[,'classificacao_acidente'];

write.table(Dataset_PRF_all, file = 'Dataset_PRF_all_strings.csv', row.names = FALSE, col.names = FALSE, sep = ",", dec = "." )
write.table(X_all_strings, file = 'X_all_strings.csv', row.names = FALSE, col.names = FALSE, sep = ",", dec = "." )
write.table(y_all_strings, file = 'y_all_strings.csv', row.names = FALSE, col.names = FALSE, sep = ",", dec = "." )

write.csv(Dataset_PRF_all, file = "Dataset_PRF_2014_csv_.csv", row.names = FALSE)


##########################################################################################################################################




### Normalizar strings em valores inteiros para rodar ML em python
#
for(i in 1:nrow(Dataset_PRF_all)) {
  
  #dia_semana
  if(Dataset_PRF_all[i,'dia_semana'] == 'domingo'){
    Dataset_PRF_all[i,'dia_semana'] = 1
  }
  if(Dataset_PRF_all[i,'dia_semana'] == 'segunda-feira'){
    Dataset_PRF_all[i,'dia_semana'] = 2
  }
  if(Dataset_PRF_all[i,'dia_semana'] == 'terca-feira'){
    Dataset_PRF_all[i,'dia_semana'] = 3
  }
  if(Dataset_PRF_all[i,'dia_semana'] == 'quarta-feira'){
    Dataset_PRF_all[i,'dia_semana'] = 4
  }
  if(Dataset_PRF_all[i,'dia_semana'] == 'quinta-feira'){
    Dataset_PRF_all[i,'dia_semana'] = 5
  }
  if(Dataset_PRF_all[i,'dia_semana'] == 'sexta-feira'){
    Dataset_PRF_all[i,'dia_semana'] = 5
  }
  if(Dataset_PRF_all[i,'dia_semana'] == 'sabado'){
    Dataset_PRF_all[i,'dia_semana'] = 7
  }
  
  #km
  Dataset_PRF_all[i,'km'] = as.integer(Dataset_PRF_all[i,'km'])
  
  #br
  Dataset_PRF_all[i,'br'] = as.integer(Dataset_PRF_all[i,'br'])
  
}


View(Dataset_PRF_all)

# Reescreve a base em novos arquivos .csv
write.csv(Dataset_PRF_all, file = "Dataset_PRF_all_csv_.csv", row.names = FALSE)
write.table(Dataset_PRF_all, file = 'Dataset_PRF_all_strings.csv', row.names = FALSE, col.names = TRUE, sep = ",", dec = "." )


### Normalização dos valores com string para números (para utilizar as técnicas de ML em python depois)




