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

dados <- readRDS("LeidenRanking2020.Rds")

#dados <- readRDS("LeidenRanking.Rds")
data2 <- read.csv("LatLongBrazilianUniversities2.csv")

brazil <- dados %>% filter(Country=="BRAZIL", Frac_counting == 0, Field == "ALL SCIENCES",Per_End == 2019 | Per_End == 2018 | Per_End == 2017 | Per_End == 2016, 
                           University == "UNIVERSITY OF SAO PAULO" |University == "UNIVERSIDADE ESTADUAL PAULISTA"|University == "UNIVERSITY OF CAMPINAS"| University == "UNIVERSIDADE FEDERAL DO RIO DE JANEIRO" 
                           | University == "FEDERAL UNIVERSITY OF RIO GRANDE DO SUL"| University == "UNIVERSIDADE FEDERAL DE MINAS GERAIS")
brazil <- brazil[order(brazil$University),]
brazil <- brazil %>% select(University, Per_End,impact_P,collab_P,P_top10,PP_top10,P_industry_collab)

Nruniversidades <- data %>% group_by(Country, latitude, longitude) %>% summarise(NrUniv=n_distinct(University))
df <-  data.frame(Cor = topo.colors(56, alpha = NULL), stringsAsFactors = FALSE)
str(Nruniversidades)
Nruniversidades$Country <- as.factor(Nruniversidades$Country)
Nruniversidades$latitude <- as.numeric(Nruniversidades$latitude) 
Nruniversidades$longitude <- as.numeric(Nruniversidades$longitude)
Nruniversidades$NrUniv <- as.numeric(Nruniversidades$NrUniv)

brazil2 <- data %>% filter(Country=="BRAZIL")


#pop <- data.table::fread("Populacao.csv")
#names(pop) <- c("UF_EXERCICIO", "POPULACAO", "REGIAO")
