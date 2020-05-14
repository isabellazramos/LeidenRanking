rm(list = ls())
cat("\014")
library("data.table")
library("stringr")
library("tidyverse")
library("leaflet")
library(readr)
#library(readxl)
#data <- read_excel("LeidenRanking2019.xlsx", sheet = 2)
#write.csv(data, "LeidenRanking2019.csv", row.names = FALSE)
#rm(data)
data <- fread("LeidenRanking2019.csv", encoding = "Latin-1")
names(data)
data$Per_Init <- str_sub(data$Period,1,4)
data$Per_End <- str_sub(data$Period,6,9)
#list_of_files <- list.files(path = ".", recursive = TRUE,
#                            pattern = "\\.txt$", 
#                            full.names = TRUE)
LatLong <- fread("LatLongCountry.txt", header = FALSE, sep = "\t", dec = ".")
names(LatLong) <- c("Siglas", "latitude", "longitude", "Country")
data <- left_join(data, LatLong, by="Country")
data <- data %>% select(c(1:2,89:91, 4, 87:88, 3, 5:86))
names(data)
#Funcao para remover acentos
data$University <- abjutils::rm_accent(data$University)
data$Country    <- abjutils::rm_accent(data$Country   )
data$Field      <- abjutils::rm_accent(data$Field     )
#Apagar Espacos antes e apos o texto
data$University <- str_trim(data$University, side = c("both", "left", "right"))
data$University <- str_squish(data$University)
data$Country <- str_trim(data$Country, side = c("both", "left", "right"))
data$Country <- str_squish(data$Country)
data$Field   <- str_trim(data$Field, side = c("both", "left", "right"))
data$Field   <- str_squish(data$Field)
#Transformar tudo em letras maiusculas
data$University <- str_to_upper(data$University)
data$Country  <- str_to_upper(data$Country)
data$Field   <- str_to_upper(data$Field)

saveRDS(data, file="LeidenRanking.Rds")
dados <- readRDS("LeidenRanking.Rds")

brazil <- data %>% filter(Country=="BRAZIL")
unique(data$Country)

Nruniversidades <- data %>% group_by(Country, latitude, longitude) %>% summarise(NrUniv=n_distinct(University))
str(Nruniversidades)
Nruniversidades$Country <- as.factor(Nruniversidades$Country)
Nruniversidades$latitude <- as.numeric(Nruniversidades$latitude) 
Nruniversidades$longitude <- as.numeric(Nruniversidades$longitude)
Nruniversidades$NrUniv <- as.numeric(Nruniversidades$NrUniv)

Nruniversidades %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(lng = ~longitude, lat = ~latitude, popup = ~NrUniv,
             clusterOptions = markerClusterOptions())

library(leaflet)
m = leaflet() %>% addTiles()
m  # a map with the default OSM tile layer

m = m %>% setView(-93.65, 42.0285, zoom = 17)
m

m %>% addPopups(-93.65, 42.0285, 'Here is the <b>Department of Statistics</b>, ISU')


data(quakes)

# Show first 20 rows from the `quakes` dataset
leaflet(data = quakes[1:20,]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(mag), label = ~as.character(mag))
