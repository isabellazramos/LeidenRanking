library(tidyverse)
link = "https://raw.githack.com/fsbmat/StackOverflow/master/Mean_2013.txt"
Mean_2013 <- read.table(link, header = TRUE)

#transformar os dados para o formato tidy
Mean_2013 %>% gather(variavel, valor, -Mes, -Mes_id) -> mean2013_tidy

#plotando o gráfico
mean2013_tidy %>%
  ggplot(aes(x =reorder(Mes, Mes_id),y = valor, fill = variavel)) +
  geom_col(position = "dodge") +
  labs(x = "Mês", y = "Valor")
##################################################################
library(tidyverse)
dat01 <-dados %>% filter(Country=="BRAZIL") %>% 
  filter(University %in% c("FEDERAL UNIVERSITY OF PARAIBA","FEDERAL UNIVERSITY OF PERNAMBUCO"),
         Period=="2015–2018", 
         Frac_counting=="0") %>% select(1,9,11)
#link = "https://raw.githack.com/fsbmat/StackOverflow/master/Mean_2013.txt"
#Mean_2013 <- read.table(dat01, header = TRUE)

#transformar os dados para o formato tidy
#dat01 %>% gather(variavel, valor, -Mes, -Mes_id) -> mean2013_tidy

#plotando o gráfico
plot01 <- dat01 %>% 
  ggplot(aes(x =reorder(Field, impact_P),y = impact_P, fill = University, label= round(impact_P, digits = 2), 
         text=paste("impact_P% :",impact_P, "<br>", 
                    "Período:", "2015"))) +
  geom_col(position = "dodge", show.legend = FALSE) +
xlab("Área Científica (2015–2018)") + ylab("impact_P")+ ggtitle("O número de publicações de uma universidade") + 
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat01$impact_P)+500))
#theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
ggplotly(plot01, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
