rm(list = ls())
cat("\014")
library("data.table")
library("stringr")
library("tidyverse")
library("leaflet")
library("readr")
library("plotly")
library("htmlTable")
# library(readxl)
# data <- read_excel("LeidenRanking2020.xlsx", sheet = 2)
# write.csv(data, "LeidenRanking2020.csv", row.names = FALSE)
# rm(data)
# data <- fread("LeidenRanking2020.csv", encoding = "Latin-1")
# names(data)
# data$Per_Init <- str_sub(data$Period,1,4)
# data$Per_End <- str_sub(data$Period,6,9)
# #list_of_files <- list.files(path = ".", recursive = TRUE,
# #                            pattern = "\\.txt$",
# #                            full.names = TRUE)
# LatLong <- fread("LatLongCountry.txt", header = FALSE, sep = "\t", dec = ".")
# names(LatLong) <- c("Siglas", "latitude", "longitude", "Country")
# data <- left_join(data, LatLong, by="Country")
# data <- data %>% select(c(1:2,89:91, 4, 87:88, 3, 5:86))
# names(data)
# #Funcao para remover acentos
# data$University <- abjutils::rm_accent(data$University)
# data$Country    <- abjutils::rm_accent(data$Country   )
# data$Field      <- abjutils::rm_accent(data$Field     )
# #Apagar Espacos antes e apos o texto
# data$University <- str_trim(data$University, side = c("both", "left", "right"))
# data$University <- str_squish(data$University)
# data$Country <- str_trim(data$Country, side = c("both", "left", "right"))
# data$Country <- str_squish(data$Country)
# data$Field   <- str_trim(data$Field, side = c("both", "left", "right"))
# data$Field   <- str_squish(data$Field)
# #Transformar tudo em letras maiusculas
# data$University <- str_to_upper(data$University)
# data$Country  <- str_to_upper(data$Country)
# data$Field   <- str_to_upper(data$Field)
<<<<<<< HEAD
# data$impact_P <- if_else(data$Frac_counting=="0",round(data$impact_P),data$impact_P) 
# data$P_top1 <- if_else(data$Frac_counting=="0",round(data$P_top1),data$P_top1) 
# data$P_top5 <- if_else(data$Frac_counting=="0",round(data$P_top5),data$P_top5) 
# data$P_top10 <- if_else(data$Frac_counting=="0",round(data$P_top10),data$P_top10) 
# data$P_top50 <- if_else(data$Frac_counting=="0",round(data$P_top50),data$P_top50)
# data$P_collab <- if_else(data$Frac_counting=="0",round(data$P_top50),data$P_top50)
# data$P_int_collab        <- if_else(data$Frac_counting=="0",round(data$P_int_collab        ),data$P_int_collab        )
# data$P_industry_collab   <- if_else(data$Frac_counting=="0",round(data$P_industry_collab   ),data$P_industry_collab   )
# data$P_short_dist_collab <- if_else(data$Frac_counting=="0",round(data$P_short_dist_collab ),data$P_short_dist_collab )
# data$P_long_dist_collab  <- if_else(data$Frac_counting=="0",round(data$P_long_dist_collab  ),data$P_long_dist_collab  )
# data$P_OA                <- if_else(data$Frac_counting=="0",round(data$P_OA                ),data$P_OA                )
# data$P_gold_OA           <- if_else(data$Frac_counting=="0",round(data$P_gold_OA           ),data$P_gold_OA           )
# data$P_hybrid_OA         <- if_else(data$Frac_counting=="0",round(data$P_hybrid_OA         ),data$P_hybrid_OA         )
# data$P_bronze_OA         <- if_else(data$Frac_counting=="0",round(data$P_bronze_OA         ),data$P_bronze_OA         )
# data$P_green_OA          <- if_else(data$Frac_counting=="0",round(data$P_green_OA          ),data$P_green_OA          )
# data$P_OA_unknown        <- if_else(data$Frac_counting=="0",round(data$P_OA_unknown        ),data$P_OA_unknown        )
# data <- modify_if(data, ~is.numeric(.), ~round(., 2))
# saveRDS(data, file="LeidenRanking2020.Rds")
data <- readRDS("LeidenRanking2020.Rds")
=======
# 
#saveRDS(data, file="LeidenRanking.Rds")
data <- readRDS("LeidenRanking.Rds")
>>>>>>> a58fb2b7afef1b397e61feb7d36c737bca29faeb
data2 <- read.csv("LatLongBrazilianUniversities.csv")
#data <- left_join(data,data2,by = "University")
brazil <- data %>% filter(Country=="BRAZIL")
unique(brazil$University)

Nruniversidades <- data %>% group_by(Country, latitude, longitude) %>% summarise(NrUniv=n_distinct(University))
df <-  data.frame(Cor = topo.colors(56, alpha = NULL), stringsAsFactors = FALSE)
str(Nruniversidades)
Nruniversidades$Country <- as.factor(Nruniversidades$Country)
Nruniversidades$latitude <- as.numeric(Nruniversidades$latitude) 
Nruniversidades$longitude <- as.numeric(Nruniversidades$longitude)
Nruniversidades$NrUniv <- as.numeric(Nruniversidades$NrUniv)

data %>% group_by(Country, latitude, longitude) %>% summarise(NrUniv=n_distinct(University)) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(lng = ~longitude, lat = ~latitude, popup = ~NrUniv,
             clusterOptions = markerClusterOptions(maxClusterRadius = 15))

#MAPA DO BRASIL
data2 %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(lng = data2$Longitude, lat = data2$Latitude, popup = data2$University,
             clusterOptions = markerClusterOptions(maxClusterRadius = 15))

Nruniversidades %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, label = ~as.character(NrUniv),
             labelOptions = labelOptions(noHide = T, direction = 'center', textOnly = T), 
             color = ~as.factor(NrUniv))

Nruniversidades %>% 
  leaflet() %>% 
  addTiles() %>% 
  addLabelOnlyMarkers(lng = ~longitude, lat = ~latitude, label = ~as.character(NrUniv), 
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = F))

Nruniversidades %>% 
  leaflet() %>% addTiles() %>%
  addPopups(~longitude, ~latitude, ~as.character(NrUniv), 
            options = popupOptions(minWidth = 20, closeOnClick = FALSE, closeButton = FALSE))

Nruniversidades %>% 
  leaflet() %>% addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, 
             weight = 10, color = "#03F", 
             opacity = 0.1,
             stroke = FALSE,
             popup = ~ NrUniv)



# library(leaflet)
# m = leaflet() %>% addTiles()
# m  # a map with the default OSM tile layer
# 
# m = m %>% setView(-93.65, 42.0285, zoom = 17)
# m
# 
# m %>% addPopups(-93.65, 42.0285, 'Here is the <b>Department of Statistics</b>, ISU')


science <- data %>% group_by(Field) %>% summarise(n())


ImpactP <- data %>% group_by(Country, University) %>% 
  summarise(Impacto=sum(impact_P)) %>% arrange(desc(Impacto))

#PIECHART PUBLICAÇÔES
ImpactPBrazil <- ImpactP %>% filter(Country=="BRAZIL")
fig <- plot_ly(type='pie', labels=labels,values= ImpactPBrazil$Impacto, 
               textinfo='label+percent',
               insidetextorientation='radial')
fig <- fig %>% layout(title = "Publicações das universidades brasileiras durante o período de 2006-2017")
fig

#BOXPLOT PUBLICAÇÔES
ImpactP_AllSciences <- brazil %>% filter(Field == "ALL SCIENCES")
boxplot <- plot_ly(ImpactP_AllSciences,
              y = ~impact_P,
              color = ~University,
              type = "box") %>% 
  layout(title = "Publicações das universidades brasileiras 2006-2017",
         xaxis = list(title = "Universidades",
                      zeroline = FALSE),
         yaxis = list(title = "Publicações",
                      zeroline = FALSE))
boxplot

Nr <- data %>% group_by(Country, University, Per_Init, Per_End) %>% 
  summarise(Impacto=sum(impact_P)) %>% arrange(desc(Impacto)) 
head(Nr)

plot1 <- brazil %>% 
  filter(University=="UNIVERSIDADE FEDERAL DE VICOSA",
         Period=="2014–2017", 
         Frac_counting=="0") %>% 
  ggplot(aes(Field, impact_P, fill=Field, label= round(impact_P, digits = 2), 
             text=paste("Produção:",impact_P, "<br>", 
                        "Período:", Period))) +
  geom_col(aes(Field, impact_P), show.legend = FALSE) + 
  xlab("Área Ciêntífica (2014-2017)") + ylab("Número de Publicações com Impacto") + 
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()
ggplotly(plot1, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")


plot2 <- brazil %>% 
  filter(University=="UNIVERSIDADE FEDERAL DE VICOSA",
         Period=="2014–2017", 
         Frac_counting=="1") %>% 
  ggplot(aes(Field, impact_P, fill=Field, label= round(impact_P, digits = 2), 
             text=paste("Produção:",impact_P, "<br>", 
                        "Período:", Period))) +
  geom_col(aes(Field, impact_P), show.legend = FALSE) + 
  xlab("Área Ciêntífica (2014-2017)") + ylab("Número de Publicações com Impacto") + 
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()
ggplotly(plot2, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")

plot6 <- brazil %>% 
  filter(University=="FEDERAL UNIVERSITY OF PARAIBA",
         Period=="2014–2017", 
         Frac_counting=="1") %>% 
  ggplot(aes(Field, P_top1, fill=Field, label= round(P_top1, digits = 2), 
             text=paste("P Top 1% :",P_top1, "<br>", 
                        "Período:", Period))) +
  geom_col(aes(Field, P_top1), show.legend = FALSE) + 
  xlab("Área Ciêntífica (2014-2017)") + ylab("O número e a proporção de publicações de uma universidade que pertencem ao 1% mais citado com mais frequência.") + 
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()
ggplotly(plot2, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")


library(ggthemes)
plot3 <- brazil %>% 
  filter(University=="UNIVERSIDADE FEDERAL DE VICOSA",
         Field=="MATHEMATICS AND COMPUTER SCIENCE", 
         Frac_counting=="0") %>% group_by(Period) %>% 
  ggplot(aes(Period, impact_P, fill=Period, label= round(impact_P, digits = 2), 
             text=paste("Produção:",impact_P, "<br>", 
                        "Período:", Period))) +
  geom_col(aes(Period, impact_P), show.legend = FALSE) + 
  xlab("Período de Avaliação") + ylab("Número de Publicações com Impacto") + 
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()
ggplotly(plot3, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")


plot4 <- brazil %>% 
  filter(University=="UNIVERSIDADE FEDERAL DE VICOSA",
         Field=="MATHEMATICS AND COMPUTER SCIENCE", 
         Frac_counting=="1") %>% group_by(Period) %>% 
  ggplot(aes(Period, impact_P, fill=Period, label= round(impact_P, digits = 2), 
             text=paste("Produção:",impact_P, "<br>", 
                        "Período:", Period))) +
  geom_col(aes(Period, impact_P), show.legend = FALSE) + 
  xlab("Período de Avaliação") + ylab("Número de Publicações com Impacto") + 
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()
ggplotly(plot4, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")


teste <- brazil %>% 
  filter(University=="UNIVERSIDADE FEDERAL DE VICOSA",
         Field=="MATHEMATICS AND COMPUTER SCIENCE", 
         Frac_counting=="1") %>% group_by(Per_Init) 


