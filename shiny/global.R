## Load packages
library("tidyr")
library("httr")
library("lubridate")
library("knitr")
library("shiny")
library("shinyjs")
library("shinyBS")
library("plotly")
library("shinythemes")
library("shinycssloaders")
library("markdown")
library("flexdashboard")
library("tidyverse")
library("ggthemes")
library("treemap")
library("treemapify")
library("leaflet")
library("readxl")
library("readODS")
library("htmlTable")
library("stringr")
library("esquisse")
library("gridExtra")
#library("ggpubr")
library("RColorBrewer")
library("data.table")
options(DT.options = list(scrollY="300px",scrollX="300px", 
                          pageLength = 100, 
                          columnDefs = list(list(className = 'dt-center', targets = "_all"))))
library("shinydashboard")
library("shinyWidgets") # nicer inputs
##############################################################################
#names(aggSetor) <- c("university", "impact_P", "P_top10", "escala")

## Define font to be used later
f1 = list(family = "Arial", size = 10, color = "rgb(30,30,30)")

## Function to format the dates for better plotting
printDate = function(date){
  # paste0(day(date),"/",month(date, lab=T, locale="us"))
  monthsEn=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  paste0(day(date),"/",monthsEn[month(date)])
}

## colors for observed data
blu = 'rgb(100,140,240)'
dblu = 'rgb(0,0,102)'
red = 'rgb(200,30,30)'
dred = 'rgb(100,30,30)'

##############################################################################
## DATA SOURCES
dados <- readRDS("LeidenRanking.Rds")
dados <- as.data.frame(dados)
#dados <- readRDS("LeidenRanking.Rds")
data2 <- read.csv("LatLongBrazilianUniversities2.csv")

brazil <- dados %>% dplyr::filter(Country=="BRAZIL", Frac_counting == 0, Field == "ALL SCIENCES",Per_End == 2019 | Per_End == 2018 | Per_End == 2017 | Per_End == 2016, 
                           University == "UNIVERSITY OF SAO PAULO" |University == "UNIVERSIDADE ESTADUAL PAULISTA"|University == "UNIVERSITY OF CAMPINAS"| University == "UNIVERSIDADE FEDERAL DO RIO DE JANEIRO" 
                           | University == "FEDERAL UNIVERSITY OF RIO GRANDE DO SUL"| University == "UNIVERSIDADE FEDERAL DE MINAS GERAIS")
brazil <- brazil[order(brazil$University),]
brazil <- brazil %>% select(University, Per_End,impact_P,P_collab,P_top10,PP_top10,P_industry_collab)

Nruniversidades <- dados %>% group_by(Country, latitude, longitude) %>% summarise(NrUniv=n_distinct(University))
df <-  data.frame(Cor = topo.colors(56, alpha = NULL), stringsAsFactors = FALSE)
#str(Nruniversidades)
Nruniversidades$Country <- as.factor(Nruniversidades$Country)
Nruniversidades$latitude <- as.numeric(Nruniversidades$latitude)
Nruniversidades$longitude <- as.numeric(Nruniversidades$longitude)
Nruniversidades$NrUniv <- as.numeric(Nruniversidades$NrUniv)


brazil2 <- dados %>% dplyr::filter(Country=="BRAZIL")

brazil2 <- dados %>%dplyr::filter(Country=="BRAZIL")


InstName <- (Instituicoes=c("UFLA"   ="FEDERAL UNIVERSITY OF LAVRAS"             ,
                  "UFMT"   ="FEDERAL UNIVERSITY OF MATO GROSSO"        ,
                  "UFPA"   ="FEDERAL UNIVERSITY OF PARA"               ,
                  "UFPB"   ="FEDERAL UNIVERSITY OF PARAIBA"            ,
                  "UFPel"  ="FEDERAL UNIVERSITY OF PELOTAS"            ,
                  "UFPE"   ="FEDERAL UNIVERSITY OF PERNAMBUCO"         ,
                  "UFRN"   ="FEDERAL UNIVERSITY OF RIO GRANDE DO NORTE",
                  "UFRGS"  ="FEDERAL UNIVERSITY OF RIO GRANDE DO SUL"  ,
                  "UFSM"   ="FEDERAL UNIVERSITY OF SANTA MARIA"        ,
                  "UFSCAR" ="FEDERAL UNIVERSITY OF SAO CARLOS"         ,
                  "UFU"    ="FEDERAL UNIVERSITY OF UBERLANDIA"         ,
                  "UEL"    ="LONDRINA STATE UNIVERSITY"                ,
                  "UERJ"   ="RIO DE JANEIRO STATE UNIVERSITY"          ,
                  "UEM"    ="STATE UNIVERSITY OF MARINGA"              ,
                  "UNB"    ="UNIVERSIDADE DE BRASILIA"                 ,
                  "UNESP"  ="UNIVERSIDADE ESTADUAL PAULISTA"           ,
                  "UFBA"   ="UNIVERSIDADE FEDERAL DA BAHIA"            ,
                  "UFG"    ="UNIVERSIDADE FEDERAL DE GOIAS"            ,
                  "UFJF"   ="UNIVERSIDADE FEDERAL DE JUIZ DE FORA"     ,
                  "UFMG"   ="UNIVERSIDADE FEDERAL DE MINAS GERAIS"     ,
                  "UFSC"   ="UNIVERSIDADE FEDERAL DE SANTA CATARINA"   ,
                  "USP"    ="UNIVERSIDADE FEDERAL DE SAO PAULO"        ,
                  "UFV"    ="UNIVERSIDADE FEDERAL DE VICOSA"           ,
                  "UFABC"  ="UNIVERSIDADE FEDERAL DO ABC"              ,
                  "UFC"    ="UNIVERSIDADE FEDERAL DO CEARA"            ,
                  "UFES"   ="UNIVERSIDADE FEDERAL DO ESPIRITO SANTO"   ,
                  "UFPR"   ="UNIVERSIDADE FEDERAL DO PARANA"           ,
                  "UFRJ"   ="UNIVERSIDADE FEDERAL DO RIO DE JANEIRO"   ,
                  "UFF"    ="UNIVERSIDADE FEDERAL FLUMINENSE"          ,
                  "UNICAMP"="UNIVERSITY OF CAMPINAS"                   ,
                  "USP"    ="UNIVERSITY OF SAO PAULO" ))
InstName2 <- (Instituicoes=c("Não Selecionada","UFLA"   ="FEDERAL UNIVERSITY OF LAVRAS"             ,
                            "UFMT"   ="FEDERAL UNIVERSITY OF MATO GROSSO"        ,
                            "UFPA"   ="FEDERAL UNIVERSITY OF PARA"               ,
                            "UFPB"   ="FEDERAL UNIVERSITY OF PARAIBA"            ,
                            "UFPel"  ="FEDERAL UNIVERSITY OF PELOTAS"            ,
                            "UFPE"   ="FEDERAL UNIVERSITY OF PERNAMBUCO"         ,
                            "UFRN"   ="FEDERAL UNIVERSITY OF RIO GRANDE DO NORTE",
                            "UFRGS"  ="FEDERAL UNIVERSITY OF RIO GRANDE DO SUL"  ,
                            "UFSM"   ="FEDERAL UNIVERSITY OF SANTA MARIA"        ,
                            "UFSCAR" ="FEDERAL UNIVERSITY OF SAO CARLOS"         ,
                            "UFU"    ="FEDERAL UNIVERSITY OF UBERLANDIA"         ,
                            "UEL"    ="LONDRINA STATE UNIVERSITY"                ,
                            "UERJ"   ="RIO DE JANEIRO STATE UNIVERSITY"          ,
                            "UEM"    ="STATE UNIVERSITY OF MARINGA"              ,
                            "UNB"    ="UNIVERSIDADE DE BRASILIA"                 ,
                            "UNESP"  ="UNIVERSIDADE ESTADUAL PAULISTA"           ,
                            "UFBA"   ="UNIVERSIDADE FEDERAL DA BAHIA"            ,
                            "UFG"    ="UNIVERSIDADE FEDERAL DE GOIAS"            ,
                            "UFJF"   ="UNIVERSIDADE FEDERAL DE JUIZ DE FORA"     ,
                            "UFMG"   ="UNIVERSIDADE FEDERAL DE MINAS GERAIS"     ,
                            "UFSC"   ="UNIVERSIDADE FEDERAL DE SANTA CATARINA"   ,
                            "USP"    ="UNIVERSIDADE FEDERAL DE SAO PAULO"        ,
                            "UFV"    ="UNIVERSIDADE FEDERAL DE VICOSA"           ,
                            "UFC"    ="UNIVERSIDADE FEDERAL DO CEARA"            ,
                            "UFES"   ="UNIVERSIDADE FEDERAL DO ESPIRITO SANTO"   ,
                            "UFPR"   ="UNIVERSIDADE FEDERAL DO PARANA"           ,
                            "UFRJ"   ="UNIVERSIDADE FEDERAL DO RIO DE JANEIRO"   ,
                            "UFF"    ="UNIVERSIDADE FEDERAL FLUMINENSE"          ,
                            "UNICAMP"="UNIVERSITY OF CAMPINAS"                   ,
                            "USP"    ="UNIVERSITY OF SAO PAULO" ))

#clean_inst = brazil2 %>% filter(!is.na(ticker$Name))
#stock_names_ticker = setNames(brazil2$University, clean_tickers$Name)

#pop <- data.table::fread("Populacao.csv")
#names(pop) <- c("UF_EXERCICIO", "POPULACAO", "REGIAO")


