#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    ##################################################################
    #########################Dados do Participante####################
    ##################################################################
    output$table1 <- DT::renderDataTable({
        DT::datatable(dados,  
                      class = 'cell-border stripe',
                      extensions = 'Buttons', options = list(
                          dom = 'Bfrtip',
                          buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print')
                      ))
    })
    output$table2 <- DT::renderDataTable({
       DT::datatable(brazil2,  
                     class = 'cell-border stripe',
                     extensions = 'Buttons', options = list(
                        dom = 'Bfrtip',
                        buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print')
                     ))
    })
    
    output$plot37 <- renderPlot({
       data2 %>% 
          leaflet() %>% 
          addTiles() %>% 
          addMarkers(lng = data2$Longitude, lat = data2$Latitude, popup = data2$University,
                     clusterOptions = markerClusterOptions(maxClusterRadius = 15))
       
    })
    
    output$InstPais <- renderPlotly({
       dados <- dados %>% dplyr::filter(year==input$AnoInst)
       df <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
          dplyr::filter(University %in% input$InstPais1,
                        Period=="2015–2018",
                        Field ==input$FieldAvaliado,
                        Frac_counting=="0")
       
   plotInst <- ggplot(df, aes(x =reorder(stringr::str_wrap(University,width = 10), impact_P), y = impact_P, fill = University, label= round(impact_P, digits = 2), 
                                   text=paste("Número de Publicações:",impact_P, "<br>", 
                                              "Período:", Period))) +
          geom_col(show.legend = FALSE) + 
          xlab("Área Científica") + ylab("Número de Publicações")+ ggtitle("O número de publicações de uma universidade") + 
          geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(df$impact_P)+500)) + 
          theme(legend.position = "bottom")
       #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
       ggplotly(plotInst, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
    })
    
    output$treemap <- renderPlot({
       aggSetor <-brazil2%>% dplyr::filter(Per_End=="2019", Frac_counting=="1", Field==input$Field1) %>% 
          select(University, impact_P, P_top10)
       aggSetor$escala <- scale(aggSetor$P_top10)
       aggSetor$University <- recode(aggSetor$University, 
                                     "FEDERAL UNIVERSITY OF LAVRAS"             ="UFLA"   ,
                                     "FEDERAL UNIVERSITY OF MATO GROSSO"        ="UFMT"   ,
                                     "FEDERAL UNIVERSITY OF PARA"               ="UFPA"   ,
                                     "FEDERAL UNIVERSITY OF PARAIBA"            ="UFPB"   ,
                                     "FEDERAL UNIVERSITY OF PELOTAS"            ="UFPel"  ,
                                     "FEDERAL UNIVERSITY OF PERNAMBUCO"         ="UFPE"   ,
                                     "FEDERAL UNIVERSITY OF RIO GRANDE DO NORTE"="UFRN"   ,
                                     "FEDERAL UNIVERSITY OF RIO GRANDE DO SUL"  ="UFRGS"  ,
                                     "FEDERAL UNIVERSITY OF SANTA MARIA"        ="UFSM"   ,
                                     "FEDERAL UNIVERSITY OF SAO CARLOS"         ="UFSCAR" ,
                                     "FEDERAL UNIVERSITY OF UBERLANDIA"         ="UFU"    ,
                                     "LONDRINA STATE UNIVERSITY"                ="UEL"    ,
                                     "RIO DE JANEIRO STATE UNIVERSITY"          ="UERJ"   ,
                                     "STATE UNIVERSITY OF MARINGA"              ="UEM"    ,
                                     "UNIVERSIDADE DE BRASILIA"                 ="UNB"    ,
                                     "UNIVERSIDADE ESTADUAL PAULISTA"           ="UNESP"  ,
                                     "UNIVERSIDADE FEDERAL DA BAHIA"            ="UFBA"   ,
                                     "UNIVERSIDADE FEDERAL DE GOIAS"            ="UFG"    ,
                                     "UNIVERSIDADE FEDERAL DE JUIZ DE FORA"     ="UFJF"   ,
                                     "UNIVERSIDADE FEDERAL DE MINAS GERAIS"     ="UFMG"   ,
                                     "UNIVERSIDADE FEDERAL DE SANTA CATARINA"   ="UFSC"   ,
                                     "UNIVERSIDADE FEDERAL DE SAO PAULO"        ="USP"    ,
                                     "UNIVERSIDADE FEDERAL DE VICOSA"           ="UFV"    ,
                                     "UNIVERSIDADE FEDERAL DO ABC"              ="UFABC"  ,
                                     "UNIVERSIDADE FEDERAL DO CEARA"            ="UFC"    ,
                                     "UNIVERSIDADE FEDERAL DO ESPIRITO SANTO"   ="UFES"   ,
                                     "UNIVERSIDADE FEDERAL DO PARANA"           ="UFPR"   ,
                                     "UNIVERSIDADE FEDERAL DO RIO DE JANEIRO"   ="UFRJ"   ,
                                     "UNIVERSIDADE FEDERAL FLUMINENSE"          ="UFF"    ,
                                     "UNIVERSITY OF CAMPINAS"                   ="UNICAMP",
                                     "UNIVERSITY OF SAO PAULO"                  ="USP"     )
       
       ggplot(aggSetor, aes(area = impact_P, fill = P_top10, label = University)) +
          geom_treemap()+scale_fill_gradient2(low = "#F8766D",
                                              high = "red",
                                              space = "Lab")+
          #scale_fill_gradient(low = "yellow", high = "red", na.value = NA)+
          geom_treemap_text(place = "centre",grow = T, reflow = T, alpha = .8, colour = "black") +
          theme(legend.position = "bottom") +
          labs(
             title = "Universidades Brasileiras no Leiden Ranking (2019)",
             caption = "Cor relacionado a número de artigos entre os 10% mais citados - Número de Artigos citados",
             fill = "Número de Artigos Top 10" )
    })
    
    
    
    output$plot01 <- renderPlotly({
       dados <- dados %>% dplyr::filter(year==input$AnoImpact_P)
       if(input$optionsescolha01 == "Null")
       {
          if(input$frac01 == "0")
          {
             dat01 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                dplyr::filter(University==input$options01,
                       Period==ifelse(input$AnoImpact_P=="2020","2015–2018","2016–2019"), 
                       Frac_counting=="0")
             
             plot01 <- ggplot(dat01, aes(stringr::str_wrap(Field,width = 10), impact_P, fill=Field, label= round(impact_P, digits = 2), 
                                         text=paste("Número de Publicações:",impact_P, "<br>", 
                                                    "Período:", Period))) +
                geom_col( show.legend = FALSE) + 
                xlab(paste0("Área Científica", "(",ifelse(input$AnoImpact_P=="2020","2015–2018","2016–2019"),")")) + ylab("Número de Publicações")+ ggtitle("O número de publicações de uma universidade") + 
                geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat01$impact_P)+500)) + 
                theme(legend.position = "bottom")
             #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
             ggplotly(plot01, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
          }else
          {
             dat01 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                dplyr::filter(University==input$options01,
                       Period==ifelse(input$AnoImpact_P=="2020","2015–2018","2016–2019"),  
                       Frac_counting=="1")
             
             plot01 <- ggplot(dat01, aes(stringr::str_wrap(Field,width = 10), impact_P, fill=Field, label= round(impact_P, digits = 2), 
                                         text=paste("Número de Publicações:",impact_P, "<br>", 
                                                    "Período:", Period))) +
                geom_col( show.legend = FALSE) + 
                xlab(paste0("Área Científica", "(",ifelse(input$AnoImpact_P=="2020","2015–2018","2016–2019"),")")) + ylab("Número de Publicações")+ ggtitle("O número de publicações de uma universidade") + 
                geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat01$impact_P)+500)) + 
                theme(legend.position = "bottom")
             #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
             ggplotly(plot01, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
          }
       }
       else
       {
          if(input$frac01 == "0")
          {
             dat01 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                dplyr::filter(University %in% c(input$options01,input$optionsescolha01),
                       Period=="2015–2018", 
                       Frac_counting=="0") %>% select(1,9,11)
             plot01 <- dat01 %>% 
                ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), impact_P),y = impact_P, fill = University, label= round(impact_P, digits = 2), 
                           text=paste(University,"<br>","Número de Publicações:",impact_P, "<br>", 
                                      "Período:", "2015-2018"))) +
                geom_col(position = "dodge", show.legend = FALSE) +
                xlab(paste0("Área Científica", "(",ifelse(input$AnoImpact_P=="2020","2015–2018","2016–2019"),")")) + ylab("Número de Publicações")+ ggtitle("Comparação: O número de publicações de uma universidade") + 
                geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat01$impact_P)+500)) + 
                theme(legend.position = "bottom")
             #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
             ggplotly(plot01, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
             
          }else
          {
             dat01 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                dplyr::filter(University %in% c(input$options01,input$optionsescolha01),
                       Period=="2015–2018", 
                       Frac_counting=="1") %>% select(1,9,11)
             plot01 <- dat01 %>% 
                ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), impact_P),y = impact_P, fill = University, label= round(impact_P, digits = 2), 
                           text=paste(University,"<br>","Número de Publicações:",impact_P, "<br>", 
                                      "Período:", "2015-2018"))) +
                geom_col(position = "dodge", show.legend = FALSE) +
                xlab(paste0("Área Científica", "(",ifelse(input$AnoImpact_P=="2020","2015–2018","2016–2019"),")")) + ylab("Número de Publicações")+ ggtitle("Comparação: O número de publicações de uma universidade") + 
                geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat01$impact_P)+500)) + 
                theme(legend.position = "bottom")
             #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
             ggplotly(plot01, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
          }
       }
       
       
    })
    
    output$textImp_p <- renderText({
       if(input$frac01 == "0"){
          HTML(paste("A contagem não fracionada leva em conta o número absoluto de publicações de uma universidade."))
          
       }else{
          "A contagem fracionada leva em conta a proporção de cada universidade em um determinado trabalho em conjunto com outras universidades."
       }
    })
    
    output$textImp_p2 <- renderText({
       if(input$frac7 == "0"){
          HTML(paste("A contagem não fracionada leva em conta o número absoluto de publicações de uma universidade."))
          
       }else{
          "A contagem fracionada leva em conta a proporção de cada universidade em um determinado trabalho em conjunto com outras universidades."
       }
    })
    
    output$plot13 <- renderPlotly(
       {
          dados <- dados %>% dplyr::filter(year==input$AnoImpact_P)
          if(input$optionsescolha03 == "Null")
            {
             if(input$frac7 == "0")
             {
                dat13 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University==input$options7,
                          Period=="2015–2018", 
                          Frac_counting=="0")
                plot13 <- ggplot(dat13, aes(x =reorder(stringr::str_wrap(Field,width = 10), PP_top1), PP_top1, fill=Field, label= round(PP_top1, digits = 2), 
                                            text=paste("PP Top 1% :",PP_top1, "<br>", 
                                                       "Período:", Period))) +
                   geom_col( show.legend = FALSE) + 
                   xlab(paste0("Área Científica", "(",ifelse(input$AnoImpact_P=="2020","2015–2018","2016–2019"),")")) + ylab("PP_top1")+ ggtitle("A proporção de publicações de uma universidade que pertencem ao 1% mais citado") + 
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat13$PP_top1)+0.001))
                #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
                ggplotly(plot13, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
                
             }else
             {
                dat13 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University==input$options7,
                          Period=="2015–2018", 
                          Frac_counting=="1")
                plot13 <- ggplot(dat13, aes(stringr::str_wrap(Field,width = 10), PP_top1, fill=Field, label= round(PP_top1, digits = 2), 
                                            text=paste("PP Top 1% :",PP_top1, "<br>", 
                                                       "Período:", Period))) +
                   geom_col( show.legend = FALSE) + 
                   xlab("Área Científica (2015–2018)") + ylab("PP_top1")+ ggtitle("A proporção de publicações de uma universidade que pertencem ao 1% mais citado") + 
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat13$PP_top1)+0.001))
                #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
                ggplotly(plot13, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
             }
             
            }else
            {
               if(input$frac7 == "0")
               {
                  dat13 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                     dplyr::filter(University %in% c(input$options7,input$optionsescolha03),
                            Period=="2015–2018", 
                            Frac_counting=="0") %>% select(1,9,42)
                  plot13 <- dat13 %>% 
                     ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), PP_top1),y = PP_top1, fill = University, label= round(PP_top1, digits = 2), 
                                text=paste(University,"<br>","PP_top1% :",PP_top1, "<br>", 
                                           "Período:", "2015-2018"))) +
                     geom_col(position = "dodge", show.legend = FALSE) +
                     xlab("Área Científica (2015–2018)") + ylab("PP_top1")+ ggtitle("Comparação: A proporção de publicações de uma universidade que pertencem ao 1% mais citado") + 
                     geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat13$PP_top1)+500))
                  ggplotly(plot13, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
                  
               }else
               {
                  dat13 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                     dplyr::filter(University==input$options7,
                            Period=="2015–2018", 
                            Frac_counting=="1")%>% select(1,9,42)
                  plot13 <- dat13 %>% 
                     ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), PP_top1),y = PP_top1, fill = University, label= round(PP_top1, digits = 2), 
                                text=paste(University,"<br>","PP_top1% :",PP_top1, "<br>", 
                                           "Período:", "2015-2018"))) +
                     geom_col(position = "dodge", show.legend = FALSE) +
                     xlab("Área Científica (2015–2018)") + ylab("PP_top1")+ ggtitle("Comparação: A proporção de publicações de uma universidade que pertencem ao 1% mais citado") + 
                     geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat13$PP_top1)+500))
                  ggplotly(plot13, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
                  
                  
               }
             
            }
       }
    )
    
    
    output$plot2 <- renderLeaflet({
        dados %>% group_by(Country, latitude, longitude) %>% summarise(NrUniv=n_distinct(University)) %>% 
            leaflet() %>% 
            addTiles() %>% 
            addMarkers(lng = ~longitude, lat = ~latitude, popup = ~NrUniv,
                       clusterOptions = markerClusterOptions(maxClusterRadius = 15))
    })
    
   output$plot3 <- renderPlotly({
      dados <- dados %>% dplyr::filter(year==input$AnoImpact_P)
       plot2 <- dados %>% dplyr::filter(Country=="BRAZIL") %>% 
           dplyr::filter(University=="UNIVERSIDADE FEDERAL DE VICOSA",
                  Period=="2015–2018", 
                  Frac_counting=="1") %>% 
           ggplot(aes(stringr::str_wrap(Field,width = 10), impact_P, fill=Field, label= round(impact_P, digits = 2), 
                      text=paste("Produção:",impact_P, "<br>", 
                                 "Período:", Period))) +
           geom_col(aes(stringr::str_wrap(Field,width = 10), impact_P), show.legend = FALSE) + 
          xlab("Área Científica (2015–2018)") + ylab("Número de Publicações com Impacto") + 
           geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw() +
           theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
       ggplotly(plot2, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
       
   }) 
   
   output$plot11 <- renderPlotly({
      dados <- dados %>% dplyr::filter(year==input$AnoImpact_P)
       plot11 <- dados %>% dplyr::filter(Country=="BRAZIL") %>% 
           dplyr::filter(University==input$options,
                  Period=="2015–2018", 
                  Frac_counting=="1") %>% 
           ggplot(aes(stringr::str_wrap(Field,width = 10), impact_P, fill=Field, label= round(impact_P, digits = 2), 
                      text=paste("Produção:",impact_P, "<br>", 
                                 "Período:", Period))) +
           geom_col(aes(stringr::str_wrap(Field,width = 10), impact_P), show.legend = FALSE) + 
          xlab("Área Científica (2015–2018)") + ylab("Número de Publicações com Impacto") + 
           geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw() +
           theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
       ggplotly(plot11, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
       
   })
   
   output$textImp_p3 <- renderText({
      if(input$frac == "0"){
         HTML(paste("A contagem não fracionada leva em conta o número absoluto de publicações de uma universidade."))
         
      }else{
         "A contagem fracionada leva em conta a proporção de cada universidade em um determinado trabalho em conjunto com outras universidades."
      }
   })
   output$plot6 <- renderPlotly(
       {
          dados <- dados %>% dplyr::filter(year==input$AnoP_Top1)
         if(input$optionsescolha02 == "Null")
            {
               if(input$frac == "0")
               {
                  dat6 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                     dplyr::filter(University==input$options1,
                                   Period==ifelse(input$AnoP_Top1=="2020","2015–2018","2016–2019"), 
                                   Frac_counting=="0")
                  
                  plot6 <- ggplot(dat6, aes(stringr::str_wrap(Field,width = 10), P_top1, fill=Field, label= round(P_top1, digits = 2), 
                                              text=paste("Número de Publicações:",P_top1, "<br>", 
                                                         "Período:", Period))) +
                     geom_col( show.legend = FALSE) + 
                     xlab(paste0("Área Científica", "(",ifelse(input$P_top1=="2020","2015–2018","2016–2019"),")")) +  ylab("P_top1") + ggtitle("O número de publicações de uma universidade que pertencem ao 1% mais citado") + 
                     geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat6$P_top1)+10)) + 
                     theme(legend.position = "bottom")
                  ggplotly(plot6, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
                  
               }else
               {
                  dat6 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                     dplyr::filter(University==input$options1,
                                   Period==ifelse(input$AnoP_Top1=="2020","2015–2018","2016–2019"), 
                                   Frac_counting=="1")
                  plot6 <- ggplot(dat6, aes(stringr::str_wrap(Field,width = 10), P_top1, fill=Field, label= round(P_top1, digits = 2), 
                                            text=paste("P Top 1% :",P_top1, "<br>", 
                                                       "Período:", Period))) +
                     geom_col(show.legend = FALSE) + 
                     xlab("Área Científica (2015–2018)") + ylab("P_top1") + ggtitle("O número de publicações de uma universidade que pertencem ao 1% mais citado")+ 
                     geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat6$P_top1)+0.5))+
                     theme(legend.position = "bottom")
                  ggplotly(plot6, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
               }
               
            }else{
               if(input$frac == "0")
               {
                  dat6 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                     dplyr::filter(University %in% c(input$options1,input$optionsescolha02),
                            Period=="2015–2018", 
                            Frac_counting=="0") %>% select(1,9,18)
                  plot6 <- dat6 %>% 
                     ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), P_top1),y = P_top1, fill = University, label= round(P_top1, digits = 2), 
                                text=paste(University,"<br>","P_top1% :",P_top1, "<br>", 
                                           "Período:", "2015-2018"))) +
                     geom_col(position = "dodge", show.legend = FALSE) +
                     xlab("Área Científica (2015–2018)") + ylab("P_top1")+ ggtitle("Comparação: O número de publicações de uma universidade que pertencem ao 1% mais citado") + 
                     geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat6$P_top1)+10))+
                     theme(legend.position = "bottom")
                  ggplotly(plot6, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
                  
               }else
               {
                  dat6 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                     dplyr::filter(University %in% c(input$options1,input$optionsescolha02),
                            Period=="2015–2018", 
                            Frac_counting=="1") %>% select(1,9,18)
                  plot6 <- dat6 %>% 
                     ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), P_top1),y = P_top1, fill = University, label= round(P_top1, digits = 2), 
                                text=paste(University,"<br>","P_top1% :",P_top1, "<br>", 
                                           "Período:", "2015-2018"))) +
                     geom_col(position = "dodge", show.legend = FALSE) +
                     xlab("Área Científica (2015–2018)") + ylab("P_top1")+ ggtitle("Comparação: O número de publicações de uma universidade que pertencem ao 1% mais citado") + 
                     geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat6$P_top1)+10))+
                     theme(legend.position = "bottom")
                  ggplotly(plot6, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
               }
         }
       }
   )
   
   output$textImp_p4 <- renderText({
      if(input$frac2 == "0"){
         HTML(paste("A contagem não fracionada leva em conta o número absoluto de publicações de uma universidade."))
         
      }else{
         "A contagem fracionada leva em conta a proporção de cada universidade em um determinado trabalho em conjunto com outras universidades."
      }
   })
   output$plot7 <- renderPlotly(
       {
          dados <- dados %>% dplyr::filter(year==input$AnoP_Top5)
          if(input$optionsescolha04 == "Null")
             {
                
                if(input$frac2 == "0")
                {
                   dat7 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                      dplyr::filter(University==input$options2,
                                    Period==ifelse(input$AnoP_Top5=="2020","2015–2018","2016–2019"), 
                                    Frac_counting=="0")
                   
                   plot7 <- ggplot(dat7, aes(stringr::str_wrap(Field,width = 10), P_top5, fill=Field, label= round(P_top5, digits = 2), 
                                               text=paste("Número de Publicações:",P_top5, "<br>", 
                                                          "Período:", Period))) +
                      geom_col( show.legend = FALSE) + 
                      xlab(paste0("Área Científica", "(",ifelse(input$AnoP_Top5=="2020","2015–2018","2016–2019"),")")) +  ylab("P_top5")+ ggtitle("O número de publicações de uma universidade que pertencem ao 5% mais citado") + 
                      geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat7$P_top5)+0.5)) + 
                      theme(legend.position = "bottom")
                   ggplotly(plot7, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
                   
                }else
                {
                   dat7 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                      dplyr::filter(University==input$options2,
                                    Period==ifelse(input$AnoP_Top5=="2020","2015–2018","2016–2019"), 
                                    Frac_counting=="1")
                   
                   plot7 <- ggplot(dat7, aes(stringr::str_wrap(Field,width = 10), P_top5, fill=Field, label= round(P_top5, digits = 2), 
                                             text=paste("Número de Publicações:",P_top5, "<br>", 
                                                        "Período:", Period))) +
                      geom_col( show.legend = FALSE) + 
                      xlab(paste0("Área Científica", "(",ifelse(input$AnoP_Top5=="2020","2015–2018","2016–2019"),")")) +  ylab("P_top5")+ ggtitle("O número de publicações de uma universidade que pertencem ao 5% mais citado") + 
                      geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat7$P_top5)+0.5)) + 
                      theme(legend.position = "bottom")
                   ggplotly(plot7, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
                }
             
            }else
            {
               if(input$frac2 == "0")
               {
                  dat7 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                     dplyr::filter(University %in% c(input$options2,input$optionsescolha04),
                            Period=="2015–2018", 
                            Frac_counting=="0") %>% select(1,9,19)
                  plot7 <- dat7 %>%  
                     ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), P_top5),y = P_top5, fill = University, label= round(P_top5, digits = 2), 
                                text=paste(University,"<br>","P_top5% :",P_top5, "<br>", 
                                           "Período:", "2015-2018"))) +
                     geom_col(position = "dodge", show.legend = FALSE) +
                     xlab(paste0("Área Científica","(",ifelse(input$AnoP_Top5=="2020","2015–2018","2016–2019"),")")) + ylab("P_top5")+ ggtitle("Comparação: O número de publicações de uma universidade que pertencem ao 5% mais citado") + 
                     geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat7$P_top5)+0.5))+ 
                     theme(legend.position = "bottom")
                  ggplotly(plot7, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
                  
               }else
               {
                  dat7 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                     dplyr::filter(University %in% c(input$options2,input$optionsescolha04),
                            Period=="2015–2018", 
                            Frac_counting=="1") %>% select(1,9,19)
                  plot7 <- dat7 %>%  
                     ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), P_top5),y = P_top5, fill = University, label= round(P_top5, digits = 2), 
                                text=paste(University,"<br>","P_top5% :",P_top5, "<br>", 
                                           "Período:", "2015-2018"))) +
                     geom_col(position = "dodge", show.legend = FALSE) +
                     xlab(paste0("Área Científica","(",ifelse(input$AnoP_Top5=="2020","2015–2018","2016–2019"),")")) + ylab("P_top5")+ ggtitle("Comparação: O número de publicações de uma universidade que pertencem ao 5% mais citado") + 
                     geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat7$P_top5)+0.5))+ 
                     theme(legend.position = "bottom")
                  ggplotly(plot7, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")

               }
            }
       }
   )
   
   output$textImp_p5 <- renderText({
      if(input$frac8 == "0"){
         HTML(paste("A contagem não fracionada leva em conta o número absoluto de publicações de uma universidade."))
         
      }else{
         "A contagem fracionada leva em conta a proporção de cada universidade em um determinado trabalho em conjunto com outras universidades."
      }
   })
   output$plot14 <- renderPlotly(
       {
          dados <- dados %>% dplyr::filter(year==input$AnoImpact_P)
          if(input$optionsescolha05 == "Null")
          {
             if(input$frac8 == "0")
             {
                dat14 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University==input$options8,
                          Period=="2015–2018", 
                          Frac_counting=="0") 
                plot14 <- ggplot(dat14, aes(stringr::str_wrap(Field,width = 10), PP_top5, fill=Field, label= round(PP_top5, digits = 2), 
                                            text=paste("PP Top 5% :",PP_top5, "<br>", 
                                                       "Período:", Period))) +
                   geom_col(show.legend = FALSE) + 
                   xlab("Área Científica (2015–2018)") + ylab("PP_top5")+ ggtitle("A proporção de publicações de uma universidade que pertencem ao 5% mais citado") + 
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat14$PP_top5)+0.005))
                #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
                ggplotly(plot14, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
             }else
             {
                dat14 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University==input$options8,
                          Period=="2015–2018", 
                          Frac_counting=="1") 
                plot14 <- ggplot(dat14, aes(stringr::str_wrap(Field,width = 10), PP_top5, fill=Field, label= round(PP_top5, digits = 2), 
                                            text=paste("PP Top 5% :",PP_top5, "<br>", 
                                                       "Período:", Period))) +
                   geom_col(show.legend = FALSE) + 
                   xlab("Área Científica (2015–2018)") + ylab("PP_top5")+ ggtitle("A proporção de publicações de uma universidade que pertencem ao 5% mais citado") + 
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat14$PP_top5)+0.005))
                #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
                ggplotly(plot14, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
             }
             
          }else
          {
             if(input$frac8 == "0")
             {
                dat14 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University %in% c(input$options8,input$optionsescolha05),
                          Period=="2015–2018", 
                          Frac_counting=="0") %>% select(1,9,45)
                plot14 <- dat14 %>% 
                   ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), PP_top5),y = PP_top5, fill = University, label= round(PP_top5, digits = 2), 
                              text=paste(University,"<br>","PP_top5% :",PP_top5, "<br>", 
                                         "Período:", "2015-2018"))) +
                   geom_col(position = "dodge", show.legend = FALSE) +
                   xlab("Área Científica (2015–2018)") + ylab("PP_top5")+ ggtitle("Comparação: A proporção de publicações de uma universidade que pertencem ao 5% mais citado") + 
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat14$PP_top5)+500))
                ggplotly(plot14, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
                
             }else
             {
                dat14 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University %in% c(input$options8,input$optionsescolha05),
                          Period=="2015–2018", 
                          Frac_counting=="1") %>% select(1,9,45)
                plot14 <- dat14 %>% 
                   ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), PP_top5),y = PP_top5, fill = University, label= round(PP_top5, digits = 2), 
                              text=paste(University,"<br>","PP_top5% :",PP_top5, "<br>", 
                                         "Período:", "2015-2018"))) +
                   geom_col(position = "dodge", show.legend = FALSE) +
                   xlab("Área Científica (2015–2018)") + ylab("PP_top5")+ ggtitle("Comparação: A proporção de publicações de uma universidade que pertencem ao 5% mais citado") + 
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat14$PP_top5)+500))
                ggplotly(plot14, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
             }
             
          }
       }
   )
   
   output$textImp_p6 <- renderText({
      if(input$frac3 == "0"){
         HTML(paste("A contagem não fracionada leva em conta o número absoluto de publicações de uma universidade."))
         
      }else{
         "A contagem fracionada leva em conta a proporção de cada universidade em um determinado trabalho em conjunto com outras universidades."
      }
   })
   output$plot8 <- renderPlotly(
       {
          dados <- dados %>% dplyr::filter(year==input$AnoP_Top10)
          if(input$optionsescolha06 ==  "Null")
          {
             if(input$frac3 == "0")
             {
                
                dat8 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University==input$options3,
                                 Period==ifelse(input$AnoP_Top10=="2020","2015–2018","2016–2019"), 
                                 Frac_counting=="0")
                
                plot8 <- ggplot(dat8, aes(stringr::str_wrap(Field,width = 10), P_top10, fill=Field, label= round(P_top10, digits = 2), 
                                          text=paste("Número de Publicações:",P_top10, "<br>", 
                                                     "Período:", Period))) +
                   geom_col( show.legend = FALSE) + 
                   xlab(paste0("Área Científica", "(",ifelse(input$AnoP_Top10=="2020","2015–2018","2016–2019"),")")) +  ylab("P_top10") + ggtitle("O número de publicações de uma universidade que pertencem ao 10% mais citado") + 
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat8$P_top10)+10)) + 
                   theme(legend.position = "bottom")
                ggplotly(plot8, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
             }else
             {
                dat8 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University==input$options3,
                                 Period==ifelse(input$AnoP_Top10=="2020","2015–2018","2016–2019"), 
                                 Frac_counting=="1")
                
                plot8 <- ggplot(dat8, aes(stringr::str_wrap(Field,width = 10), P_top10, fill=Field, label= round(P_top10, digits = 2), 
                                          text=paste("Número de Publicações:",P_top10, "<br>", 
                                                     "Período:", Period))) +
                   geom_col( show.legend = FALSE) + 
                   xlab(paste0("Área Científica", "(",ifelse(input$AnoP_Top10=="2020","2015–2018","2016–2019"),")")) +  ylab("P_top10") + ggtitle("O número de publicações de uma universidade que pertencem ao 10% mais citado") + 
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat8$P_top10)+10)) + 
                   theme(legend.position = "bottom")
                ggplotly(plot8, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
             }
          }else
          {
             if(input$frac3 == "0")
             {
                dat8 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University %in% c(input$options3,input$optionsescolha06),
                          Period=="2015–2018", 
                          Frac_counting=="0") %>% select(1,9,20)
                plot8 <- dat8 %>%  
                   ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), P_top10),y = P_top10, fill = University, label= round(P_top10, digits = 2), 
                              text=paste(University,"<br>","P_top10% :",P_top10, "<br>", 
                                         "Período:", "2015-2018"))) +
                   geom_col(position = "dodge", show.legend = FALSE) +
                   xlab(paste0("Área Científica", "(",ifelse(input$AnoP_Top10=="2020","2015–2018","2016–2019"),")")) + ylab("P_top10")+ ggtitle("Comparação: O número de publicações de uma universidade que pertencem ao 10% mais citado") + 
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat8$P_top10)+10))+
                theme(legend.position = "bottom")
                ggplotly(plot8, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
             }else
             {
                dat8 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University %in% c(input$options3,input$optionsescolha06),
                          Period=="2015–2018", 
                          Frac_counting=="1") %>% select(1,9,20)
                plot8 <- dat8 %>%  
                   ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), P_top10),y = P_top10, fill = University, label= round(P_top10, digits = 2), 
                              text=paste(University,"<br>","P_top10% :",P_top10, "<br>", 
                                         "Período:", "2015-2018"))) +
                   geom_col(position = "dodge", show.legend = FALSE) +
                   xlab(paste0("Área Científica", "(",ifelse(input$AnoP_Top10=="2020","2015–2018","2016–2019"),")")) + ylab("P_top10")+ ggtitle("Comparação: O número de publicações de uma universidade que pertencem ao 10% mais citado") + 
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat8$P_top10)+10))+
                theme(legend.position = "bottom")
                ggplotly(plot8, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
             }
          }
       }
   )
   output$textImp_p7 <- renderText({
      if(input$frac9 == "0"){
         HTML(paste("A contagem não fracionada leva em conta o número absoluto de publicações de uma universidade."))
         
      }else{
         "A contagem fracionada leva em conta a proporção de cada universidade em um determinado trabalho em conjunto com outras universidades."
      }
   })
   output$plot15 <- renderPlotly(
       {
          dados <- dados %>% dplyr::filter(year==input$AnoImpact_P)
          if(input$optionsescolha07 == "Null")
          {
             if(input$frac9 == "0")
             {
                dat15 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University==input$options9,
                          Period=="2015–2018", 
                          Frac_counting=="0")
                plot15 <- ggplot(dat15,aes(stringr::str_wrap(Field,width = 10), PP_top10, fill=Field, label= round(PP_top10, digits = 2), 
                                           text=paste("PP Top 10% :",PP_top10, "<br>", 
                                                      "Período:", Period))) +
                   geom_col(show.legend = FALSE) + 
                   xlab("Área Científica (2015–2018)") + ylab("PP_top10")+ ggtitle("A proporção de publicações de uma universidade que pertencem ao 10% mais citado") + 
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat15$PP_top10)+0.005))
                #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
                ggplotly(plot15, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
             }else
             {
                dat15 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University==input$options9,
                          Period=="2015–2018", 
                          Frac_counting=="1")
                plot15 <- ggplot(dat15,aes(stringr::str_wrap(Field,width = 10), PP_top10, fill=Field, label= round(PP_top10, digits = 2), 
                                           text=paste("PP Top 10% :",PP_top10, "<br>", 
                                                      "Período:", Period))) +
                   geom_col(show.legend = FALSE) + 
                   xlab("Área Científica (2015–2018)") + ylab("PP_top10")+ ggtitle("A proporção de publicações de uma universidade que pertencem ao 10% mais citado") + 
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat15$PP_top10)+0.005))
                #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
                ggplotly(plot15, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
             }
             
          }
          else
          {
             if(input$frac9 == "0")
             {
                dat15 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University%in% c(input$options9,input$optionsescolha07),
                          Period=="2015–2018", 
                          Frac_counting=="0") %>% select(1,9,48)
                plot15 <- dat15 %>% 
                   ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), PP_top10),y = PP_top10, fill = University, label= round(PP_top10, digits = 2), 
                              text=paste(University,"<br>","PP_top10% :",PP_top10, "<br>", 
                                         "Período:", "2015-2018"))) +
                   geom_col(position = "dodge", show.legend = FALSE) +
                   xlab("Área Científica (2015–2018)") + ylab("PP_top10")+ ggtitle("Comparação: A proporção de publicações de uma universidade que pertencem ao 10% mais citado")+ 
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat15$PP_top10)+500))
                ggplotly(plot15, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
             }else
             {
                dat15 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University%in% c(input$options9,input$optionsescolha07),
                          Period=="2015–2018", 
                          Frac_counting=="1") %>% select(1,9,48)
                plot15 <- dat15 %>% 
                   ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), PP_top10),y = PP_top10, fill = University, label= round(PP_top10, digits = 2), 
                              text=paste(University,"<br>","PP_top10% :",PP_top10, "<br>", 
                                         "Período:", "2015-2018"))) +
                   geom_col(position = "dodge", show.legend = FALSE) +
                   xlab("Área Científica (2015–2018)") + ylab("PP_top10")+ ggtitle("Comparação: A proporção de publicações de uma universidade que pertencem ao 10% mais citado")+ 
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat15$PP_top10)+500))
                ggplotly(plot15, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
             }
             
          }
          
       }
   )
   output$textImp_p8 <- renderText({
      if(input$frac4 == "0"){
         HTML(paste("A contagem não fracionada leva em conta o número absoluto de publicações de uma universidade."))
         
      }else{
         "A contagem fracionada leva em conta a proporção de cada universidade em um determinado trabalho em conjunto com outras universidades."
      }
   })
   output$plot9 <- renderPlotly(
       {
          dados <- dados %>% dplyr::filter(year==input$AnoP_Top50)
          if(input$optionsescolha08 == "Null")
          {
             if(input$frac4 == "0")
             {
                dat9 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University==input$options4,
                                 Period==ifelse(input$AnoP_Top50=="2020","2015–2018","2016–2019"), 
                                 Frac_counting=="0")
                
                plot9 <- ggplot(dat9, aes(stringr::str_wrap(Field,width = 10), P_top50, fill=Field, label= round(P_top50, digits = 2), 
                                          text=paste("Número de Publicações:",P_top50, "<br>", 
                                                     "Período:", Period))) +
                   geom_col( show.legend = FALSE) + 
                   xlab(paste0("Área Científica", "(",ifelse(input$AnoP_Top50=="2020","2015–2018","2016–2019"),")")) +  ylab("P_top50") + ggtitle("O número de publicações de uma universidade que pertencem ao 50% mais citado") + 
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat9$P_top50)+10)) + 
                   theme(legend.position = "bottom")
                ggplotly(plot9, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
                
             }else
             {
                dat9 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University==input$options4,
                                 Period==ifelse(input$AnoP_Top50=="2020","2015–2018","2016–2019"), 
                                 Frac_counting=="1")
                
                plot9 <- ggplot(dat9, aes(stringr::str_wrap(Field,width = 10), P_top50, fill=Field, label= round(P_top50, digits = 2), 
                                          text=paste("Número de Publicações:",P_top50, "<br>", 
                                                     "Período:", Period))) +
                   geom_col( show.legend = FALSE) + 
                   xlab(paste0("Área Científica", "(",ifelse(input$AnoP_Top50=="2020","2015–2018","2016–2019"),")")) +  ylab("P_top50") + ggtitle("O número de publicações de uma universidade que pertencem ao 50% mais citado") + 
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat9$P_top50)+10)) + 
                   theme(legend.position = "bottom")
                ggplotly(plot9, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
             }
             
          }else
          {
             if(input$frac4 == "0")
             {
                
                dat9 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University %in% c(input$options4,input$optionsescolha08),
                          Period=="2015–2018", 
                          Frac_counting=="0") %>%  select(1,9,21)
                plot9 <- dat9 %>% 
                   ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), P_top50),y = P_top50, fill = University, label= round(P_top50, digits = 2), 
                              text=paste(University,"<br>","P_top50% :",P_top50, "<br>", 
                                         "Período:", "2015-2018"))) +
                   geom_col(position = "dodge", show.legend = FALSE) +
                   xlab(paste0("Área Científica", "(",ifelse(input$AnoP_Top50=="2020","2015–2018","2016–2019"),")")) +ylab("P_top50")+ ggtitle("Comparação: O número de publicações de uma universidade que pertencem ao 50% mais citado")+ 
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat9$P_top50)+10))+
                theme(legend.position = "bottom")
                ggplotly(plot9, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
             }else
             {
                dat9 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University %in% c(input$options4,input$optionsescolha08),
                          Period=="2015–2018", 
                          Frac_counting=="1") %>%  select(1,9,21)
                plot9 <- dat9 %>% 
                   ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), P_top50),y = P_top50, fill = University, label= round(P_top50, digits = 2), 
                              text=paste(University,"<br>","P_top50% :",P_top50, "<br>", 
                                         "Período:", "2015-2018"))) +
                      geom_col(position = "dodge", show.legend = FALSE) +
                   xlab(paste0("Área Científica", "(",ifelse(input$AnoP_Top50=="2020","2015–2018","2016–2019"),")")) + ylab("P_top50")+ ggtitle("Comparação: O número de publicações de uma universidade que pertencem ao 50% mais citado")+ 
                      geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat9$P_top50)+10))+
                theme(legend.position = "bottom")
                ggplotly(plot9, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
             }
          }
         
       }
   )
   output$textImp_p9 <- renderText({
      if(input$frac10 == "0"){
         HTML(paste("A contagem não fracionada leva em conta o número absoluto de publicações de uma universidade."))
         
      }else{
         "A contagem fracionada leva em conta a proporção de cada universidade em um determinado trabalho em conjunto com outras universidades."
      }
   })
   output$plot16 <- renderPlotly(
       {
          dados <- dados %>% dplyr::filter(year==input$AnoImpact_P)
          if(input$optionsescolha09 =="0")
          {
             if(input$frac10 == "0")
             {
                dat16 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University==input$options10,
                          Period=="2015–2018", 
                          Frac_counting=="0")
                plot16 <- ggplot(dat16,aes(stringr::str_wrap(Field,width = 10), PP_top50, fill=Field, label= round(PP_top50, digits = 2), 
                                           text=paste("PP Top 50% :",PP_top50, "<br>", 
                                                      "Período:", Period))) +
                   geom_col(show.legend = FALSE) + 
                   xlab("Área Científica (2015–2018)") + ylab("PP_top50") + ggtitle("A proporção de publicações de uma universidade que pertencem ao 50% mais citado")+
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat16$PP_top50)+0.005))
                #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
                ggplotly(plot16, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
             }else
             {
                dat16 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University==input$options10,
                          Period=="2015–2018", 
                          Frac_counting=="1")
                plot16 <- ggplot(dat16,aes(stringr::str_wrap(Field,width = 10), PP_top50, fill=Field, label= round(PP_top50, digits = 2), 
                                           text=paste("PP Top 50% :",PP_top50, "<br>", 
                                                      "Período:", Period))) +
                   geom_col(show.legend = FALSE) + 
                   xlab("Área Científica (2015–2018)") + ylab("PP_top50") + ggtitle("A proporção de publicações de uma universidade que pertencem ao 50% mais citado")+
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat16$PP_top50)+0.005))
                #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
                ggplotly(plot16, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
             }
             
          }else
          {
             if(input$frac10 == "0")
             {
                dat16 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University%in% c(input$options10,input$optionsescolha09),
                          Period=="2015–2018", 
                          Frac_counting=="0") %>% select(1,9,51)
                plot16 <- dat16 %>%  
                   ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), PP_top50),y = PP_top50, fill = University, label= round(PP_top50, digits = 2), 
                              text=paste(University,"<br>","PP_top50% :",PP_top50, "<br>", 
                                         "Período:", "2015-2018"))) +
                   geom_col(position = "dodge", show.legend = FALSE) +
                   xlab("Área Científica (2015–2018)") + ylab("PP_top50")+ ggtitle("Comparação: A proporção de publicações de uma universidade que pertencem ao 50% mais citado")+
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat16$PP_top50)+0.005))
                ggplotly(plot16, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
             }else
             {
                dat16 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University%in% c(input$options10,input$optionsescolha09),
                          Period=="2015–2018", 
                          Frac_counting=="1") %>% select(1,9,51)
                plot16 <- dat16 %>%  
                   ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), PP_top50),y = PP_top50, fill = University, label= round(PP_top50, digits = 2), 
                              text=paste(University,"<br>","PP_top50% :",PP_top50, "<br>", 
                                         "Período:", "2015-2018"))) +
                   geom_col(position = "dodge", show.legend = FALSE) +
                   xlab("Área Científica (2015–2018)") + ylab("PP_top50")+ ggtitle("Comparação: A proporção de publicações de uma universidade que pertencem ao 50% mais citado")+
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat16$PP_top50)+0.005))
                ggplotly(plot16, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
             }
             
          }
       }
   )
   output$textImp_p10 <- renderText({
      if(input$frac5 == "0"){
         HTML(paste("A contagem não fracionada leva em conta o número absoluto de publicações de uma universidade."))
         
      }else{
         "A contagem fracionada leva em conta a proporção de cada universidade em um determinado trabalho em conjunto com outras universidades."
      }
   })
   output$plot10 <- renderPlotly(
       {
          dados <- dados %>% dplyr::filter(year==input$AnoTCS)
          if(input$optionsescolha10 ==  "Null")
          {
             if(input$frac5 == "0")
             {
                dat10 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University==input$options5,
                                 Period==ifelse(input$AnoTCS=="2020","2015–2018","2016–2019"), 
                                 Frac_counting=="0")
                
                plot10 <- ggplot(dat10, aes(stringr::str_wrap(Field,width = 10), TCS, fill=Field, label= round(TCS, digits = 2), 
                                          text=paste("Número de Publicações:",TCS, "<br>", 
                                                     "Período:", Period))) +
                   geom_col( show.legend = FALSE) + 
                   xlab(paste0("Área Científica", "(",ifelse(input$AnpTCS =="2020","2015–2018","2016–2019"),")")) +  ylab("TCS") + ggtitle("O número total de citações das publicações de uma universidade")+
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat10$TCS)+10)) + 
                   theme(legend.position = "bottom")
                ggplotly(plot10, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
                
             }else
             {
                dat10 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University==input$options5,
                                 Period==ifelse(input$AnoTCS=="2020","2015–2018","2016–2019"), 
                                 Frac_counting=="1")
                
                plot10 <- ggplot(dat10, aes(stringr::str_wrap(Field,width = 10), TCS, fill=Field, label= round(TCS, digits = 2), 
                                            text=paste("Número de Publicações:",TCS, "<br>", 
                                                       "Período:", Period))) +
                   geom_col( show.legend = FALSE) + 
                   xlab(paste0("Área Científica", "(",ifelse(input$AnpTCS =="2020","2015–2018","2016–2019"),")")) +  ylab("TCS") + ggtitle("O número total de citações das publicações de uma universidade")+
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat10$TCS)+10)) + 
                   theme(legend.position = "bottom")
                ggplotly(plot10, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
             }
          }else
          {
             if(input$frac5 == "0")
             {
                dat10 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University %in% c(input$options5,input$optionsescolha10),
                          Period=="2015–2018", 
                          Frac_counting=="0") %>% select(1,9,16)
                plot10 <- dat10 %>% 
                   ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), TCS),y = TCS, fill = University, label= round(TCS, digits = 2), 
                              text=paste(University,"<br>","TCS% :",TCS, "<br>", 
                                         "Período:", "2015-2018"))) +
                   geom_col(position = "dodge", show.legend = FALSE) +
                   xlab(paste0("Área Científica", "(",ifelse(input$AnpTCS =="2020","2015–2018","2016–2019"),")")) +  ylab("TCS")+  ggtitle("Comparação: O número total de citações das publicações de uma universidade")+
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat10$TCS)+500))
                ggplotly(plot10, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
             }else
             {
                dat10 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University %in% c(input$options5,input$optionsescolha10),
                          Period=="2015–2018", 
                          Frac_counting=="1") %>% select(1,9,16)
                plot10 <- dat10 %>% 
                   ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), TCS),y = TCS, fill = University, label= round(TCS, digits = 2), 
                              text=paste(University,"<br>","TCS% :",TCS, "<br>", 
                                         "Período:", "2015-2018"))) +
                   geom_col(position = "dodge", show.legend = FALSE) +
                   xlab(paste0("Área Científica", "(",ifelse(input$AnpTCS =="2020","2015–2018","2016–2019"),")")) +  ylab("TCS")+  ggtitle("Comparação: O número total de citações das publicações de uma universidade")+
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat10$TCS)+500))
                ggplotly(plot10, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
             }
          }
       }
   )
   output$textImp_p11 <- renderText({
      if(input$frac11 == "0"){
         HTML(paste("A contagem não fracionada leva em conta o número absoluto de publicações de uma universidade."))
         
      }else{
         "A contagem fracionada leva em conta a proporção de cada universidade em um determinado trabalho em conjunto com outras universidades."
      }
   })
   output$plot17 <- renderPlotly(
       {
          dados <- dados %>% dplyr::filter(year==input$AnoTNCS)
          if(input$optionsescolha11 ==  "Null")
          {
             
             if(input$frac11 == "0")
             {
                dat17 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University==input$options11,
                                 Period==ifelse(input$AnoTNCS=="2020","2015–2018","2016–2019"), 
                                 Frac_counting=="0")
                
                plot17 <- ggplot(dat17, aes(stringr::str_wrap(Field,width = 10), TNCS, fill=Field, label= round(TNCS, digits = 2), 
                                            text=paste("Número de Publicações:",TNCS, "<br>", 
                                                       "Período:", Period))) +
                   geom_col( show.legend = FALSE) + 
                   xlab(paste0("Área Científica", "(",ifelse(input$AnoTNCS =="2020","2015–2018","2016–2019"),")")) +  ylab("TNCS") + ggtitle("O número total de citações das publicações de uma universidade")+
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat17$TNCS)+10)) + 
                   theme(legend.position = "bottom")
                ggplotly(plot17, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
               
             }else
             {
                dat17 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University==input$options11,
                                 Period==ifelse(input$AnoTNCS=="2020","2015–2018","2016–2019"), 
                                 Frac_counting=="1")
                
                plot17 <- ggplot(dat17, aes(stringr::str_wrap(Field,width = 10), TNCS, fill=Field, label= round(TNCS, digits = 2), 
                                            text=paste("Número de Publicações:",TNCS, "<br>", 
                                                       "Período:", Period))) +
                   geom_col( show.legend = FALSE) + 
                   xlab(paste0("Área Científica", "(",ifelse(input$AnoTNCS =="2020","2015–2018","2016–2019"),")")) +  ylab("TNCS") + ggtitle("O número total de citações das publicações de uma universidade")+
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat17$TNCS)+10)) + 
                   theme(legend.position = "bottom")
                ggplotly(plot17, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
             }
             
          }
          else
          {
             
             if(input$frac11 == "0")
             {
                dat17 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University %in% c(input$options11,input$optionsescolha11),
                          Period=="2015–2018", 
                          Frac_counting=="0") %>% select(1,9,17)
                plot17 <- dat17 %>% 
                   ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), TNCS),y = TNCS, fill = University, label= round(TNCS, digits = 2), 
                              text=paste(University,"<br>","TNCS% :",TNCS, "<br>", 
                                         "Período:", "2015-2018"))) +
                   geom_col(position = "dodge", show.legend = FALSE) +
                   xlab(paste0("Área Científica", "(",ifelse(input$AnoTNCS =="2020","2015–2018","2016–2019"),")")) +  ylab("TNCS")+ ggtitle("Comparação: O número total de citações das publicações de uma universidade")+
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat17$TNCS)+10))+
                theme(legend.position = "bottom")
                ggplotly(plot17, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
             }else
             {
                dat17 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University %in% c(input$options11,input$optionsescolha11),
                          Period=="2015–2018", 
                          Frac_counting=="1") %>% select(1,9,17)
                plot17 <- dat17 %>% 
                   ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), TNCS),y = TNCS, fill = University, label= round(TNCS, digits = 2), 
                              text=paste(University,"<br>","TNCS% :",TNCS, "<br>", 
                                         "Período:", "2015-2018"))) +
                   geom_col(position = "dodge", show.legend = FALSE) +
                   xlab(paste0("Área Científica", "(",ifelse(input$AnoTNCS =="2020","2015–2018","2016–2019"),")")) +  ylab("TNCS")+ ggtitle("Comparação: O número total de citações das publicações de uma universidade")+
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat17$TNCS)+10))+
                theme(legend.position = "bottom")
                ggplotly(plot17, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
             }
             
          }
     
       }
   )
   output$textImp_p12 <- renderText({
      if(input$frac6 == "0"){
         HTML(paste("A contagem não fracionada leva em conta o número absoluto de publicações de uma universidade."))
         
      }else{
         "A contagem fracionada leva em conta a proporção de cada universidade em um determinado trabalho em conjunto com outras universidades."
      }
   })
   output$plot12 <- renderPlotly(
       {
          dados <- dados %>% dplyr::filter(year==input$AnoMCS)
          if(input$optionsescolha12 == "Null")
          {
             
             if(input$frac6 == "0")
             {
                dat12 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University==input$options6,
                                 Period==ifelse(input$AnoMCS=="2020","2015–2018","2016–2019"), 
                                 Frac_counting=="0")
                
                plot12 <- ggplot(dat12, aes(stringr::str_wrap(Field,width = 10), MCS, fill=Field, label= round(MCS, digits = 2), 
                                            text=paste("Número de Publicações:",MCS, "<br>", 
                                                       "Período:", Period))) +
                   geom_col( show.legend = FALSE) + 
                   xlab(paste0("Área Científica", "(",ifelse(input$AnoMCS =="2020","2015–2018","2016–2019"),")")) +  ylab("TNCS") + ggtitle("O número total de citações das publicações de uma universidade")+
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat12$MCS)+10)) + 
                   theme(legend.position = "bottom")
                ggplotly(plot12, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
             }else
             {
                dat12 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University==input$options6,
                                 Period==ifelse(input$AnoMCS=="2020","2015–2018","2016–2019"), 
                                 Frac_counting=="1")
                
                plot12 <- ggplot(dat12, aes(stringr::str_wrap(Field,width = 10), MCS, fill=Field, label= round(MCS, digits = 2), 
                                            text=paste("Número de Publicações:",MCS, "<br>", 
                                                       "Período:", Period))) +
                   geom_col( show.legend = FALSE) + 
                   xlab(paste0("Área Científica", "(",ifelse(input$AnoMCS =="2020","2015–2018","2016–2019"),")")) +  ylab("TNCS") + ggtitle("O número total de citações das publicações de uma universidade")+
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat12$MCS)+10)) + 
                   theme(legend.position = "bottom")
                ggplotly(plot12, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
             }
             
          }else
          {
             
             if(input$frac6 == "0")
             {
                dat12 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University %in% c(input$options6,input$optionsescolha12),
                          Period=="2015–2018", 
                          Frac_counting=="0") %>% select(1,9,36)
                plot12 <- dat12 %>% 
                   ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), MCS),y = MCS, fill = University, label= round(MCS, digits = 2), 
                              text=paste(University,"<br>","MCS% :",MCS, "<br>", 
                                         "Período:", "2015-2018"))) +
                   geom_col(position = "dodge", show.legend = FALSE) +
                   xlab(paste0("Área Científica", "(",ifelse(input$AnoMCS =="2020","2015–2018","2016–2019"),")")) + ylab("MCS")+ ggtitle("Comparação: O número total de citações das publicações de uma universidade")+
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat12$MCS)))+
                theme(legend.position = "bottom")
                ggplotly(plot12, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
             }else
             {
                dat12 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University %in% c(input$options6,input$optionsescolha12),
                          Period=="2015–2018", 
                          Frac_counting=="1") %>% select(1,9,36)
                plot12 <- dat12 %>% 
                   ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), MCS),y = MCS, fill = University, label= round(MCS, digits = 2), 
                              text=paste(University,"<br>","MCS% :",MCS, "<br>", 
                                         "Período:", "2015-2018"))) +
                   geom_col(position = "dodge", show.legend = FALSE) +
                   xlab(paste0("Área Científica", "(",ifelse(input$AnoMCS =="2020","2015–2018","2016–2019"),")")) + ylab("MCS")+ ggtitle("Comparação: O número total de citações das publicações de uma universidade")+
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat12$MCS)))+
                theme(legend.position = "bottom")
                ggplotly(plot12, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
             }
             
          }
       }
   )
   output$textImp_p13 <- renderText({
      if(input$frac12 == "0"){
         HTML(paste("A contagem não fracionada leva em conta o número absoluto de publicações de uma universidade."))
         
      }else{
         "A contagem fracionada leva em conta a proporção de cada universidade em um determinado trabalho em conjunto com outras universidades."
      }
   })
   output$plot18 <- renderPlotly(
       {
          dados <- dados %>% dplyr::filter(year==input$AnoMNCS)
          if(input$optionsescolha13 == "Null"){
             if(input$frac12 == "0")
             {
                dat18 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University==input$options12,
                                 Period==ifelse(input$AnoMNCS=="2020","2015–2018","2016–2019"), 
                                 Frac_counting=="0")
                
                plot18 <- ggplot(dat18, aes(stringr::str_wrap(Field,width = 10), MNCS, fill=Field, label= round(MNCS, digits = 2), 
                                            text=paste("Número de Publicações:",MNCS, "<br>", 
                                                       "Período:", Period))) +
                   geom_col( show.legend = FALSE) + 
                   xlab(paste0("Área Científica", "(",ifelse(input$AnoMNCS =="2020","2015–2018","2016–2019"),")")) +  ylab("MNCS")+ ggtitle("O número médio de citações das publicações de uma universidade por campo e ano de publicação")+
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat18$MNCS)+10)) + 
                   theme(legend.position = "bottom")
                ggplotly(plot18, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
             }else
             {
                dat18 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University==input$options12,
                                 Period==ifelse(input$AnoMNCS=="2020","2015–2018","2016–2019"), 
                                 Frac_counting=="1")
                
                plot18 <- ggplot(dat18, aes(stringr::str_wrap(Field,width = 10), MNCS, fill=Field, label= round(MNCS, digits = 2), 
                                            text=paste("Número de Publicações:",MNCS, "<br>", 
                                                       "Período:", Period))) +
                   geom_col( show.legend = FALSE) + 
                   xlab(paste0("Área Científica", "(",ifelse(input$AnoMNCS =="2020","2015–2018","2016–2019"),")")) +  ylab("MNCS")+ ggtitle("O número médio de citações das publicações de uma universidade por campo e ano de publicação")+
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat18$MNCS)+10)) + 
                   theme(legend.position = "bottom")
                ggplotly(plot18, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
             }
             
          }else
          {
             if(input$frac12 == "0")
             {
                dat18 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University %in% c(input$options12,input$optionsescolha13),
                          Period=="2015–2018", 
                          Frac_counting=="0") %>% select(1,9,39)
                plot18 <- dat18 %>% 
                   ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), MNCS),y = MNCS, fill = University, label= round(MNCS, digits = 2), 
                              text=paste(University,"<br>","MNCS% :",MNCS, "<br>", 
                                         "Período:", "2015-2018"))) +
                   geom_col(position = "dodge", show.legend = FALSE) +
                   xlab(paste0("Área Científica", "(",ifelse(input$AnoMNCS =="2020","2015–2018","2016–2019"),")")) + ylab("MNCS")+ggtitle("Comparação: O número médio de citações das publicações de uma universidade por campo e ano de publicação")+
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat18$MNCS)))+
                theme(legend.position = "bottom")
                ggplotly(plot18, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
             }else
             {
                dat18 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
                   dplyr::filter(University %in% c(input$options12,input$optionsescolha13),
                          Period=="2015–2018", 
                          Frac_counting=="1") %>% select(1,9,39)
                plot18 <- dat18 %>% 
                   ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), MNCS),y = MNCS, fill = University, label= round(MNCS, digits = 2), 
                              text=paste(University,"<br>","MNCS% :",MNCS, "<br>", 
                                         "Período:", "2015-2018"))) +
                   geom_col(position = "dodge", show.legend = FALSE) +
                   xlab(paste0("Área Científica", "(",ifelse(input$AnoMNCS =="2020","2015–2018","2016–2019"),")")) + ylab("MNCS")+ggtitle("Comparação: O número médio de citações das publicações de uma universidade por campo e ano de publicação")+
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat18$MNCS)))+
                theme(legend.position = "bottom")
                ggplotly(plot18, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
             }
          }
       }
   )
   output$plot19 <- renderPlotly(
      {
         dados <- dados %>% dplyr::filter(year==input$AnoP_collab)
         if(input$optionsescolha14 == "Null")
         {
            dat19 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University==input$options14,
                             Period==ifelse(input$AnoP_collab=="2020","2015–2018","2016–2019"), 
                             Frac_counting=="0")
            
            plot19 <- ggplot(dat19, aes(stringr::str_wrap(Field,width = 10), P_collab, fill=Field, label= round(P_collab, digits = 2), 
                                        text=paste("P_collab:",P_collab, "<br>", 
                                                   "Período:", Period))) +
               geom_col( show.legend = FALSE) + 
               xlab(paste0("Área Científica", "(",ifelse(input$AnoP_collab=="2020","2015–2018","2016–2019"),")")) + ylab("P_collab")+ ggtitle("O número de publicações de uma universidade que foram coautoria de uma ou mais outras organizações") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat19$P_collab)+500)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot19, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
            
         }else
         {
            dat19 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University %in% c(input$options14,input$optionsescolha14),
                      Period=="2015–2018", Frac_counting == "0") %>% select(1,9,22)
            plot19 <- dat19 %>% 
               ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), P_collab),y = P_collab, fill = University, label= round(P_collab, digits = 2), 
                          text=paste(University,"<br>","P_collab :",P_collab, "<br>", 
                                     "Período:", "2015-2018"))) +
               geom_col(position = "dodge", show.legend = FALSE) +
               xlab(paste0("Área Científica", "(",ifelse(input$AnoP_collab=="2020","2015–2018","2016–2019"),")")) + ylab("P_collab")+ ggtitle("O número de publicações de uma universidade que foram coautoria de uma ou mais outras organizações") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat19$P_collab)+500)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot19, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
         }
      }
   )
   output$plot35 <- renderPlotly(
      {
         dados <- dados %>% dplyr::filter(year==input$AnoImpact_P)
         if(input$optionsescolha15 == "Null")
         {
            dat35 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University==input$options30,
                      Period=="2015–2018", Frac_counting == "0") 
            plot35 <- ggplot(dat35,aes(stringr::str_wrap(Field,width = 10), PP_collab, fill=Field, label= round(PP_collab, digits = 2), 
                                       text=paste("PP_collab :",PP_collab, "<br>", 
                                                  "Período:", Period))) +
               geom_col(show.legend = FALSE) + 
               xlab("Área Científica (2015–2018)") + ylab("PP_collab")+ggtitle("Comparação: A proporção de publicações de uma universidade que foram coautoria de uma ou mais outras organizações") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat35$PP_collab)+0.05))
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot35, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
            
         }else
         {
            dat35 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University %in% c(input$options30,input$optionsescolha15),
                      Period=="2015–2018", Frac_counting == "0") %>% select(1,9,54)
            plot35 <- dat35 %>% 
               ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), PP_collab),y = PP_collab, fill = University, label= round(PP_collab, digits = 2), 
                          text=paste(University,"<br>","PP_collab :",PP_collab, "<br>", 
                                     "Período:", "2015-2018"))) +
               geom_col(position = "dodge", show.legend = FALSE) +
               xlab("Área Científica (2015–2018)") + ylab("PP_collab")+ggtitle("Comparação: A proporção de publicações de uma universidade que foram coautoria de uma ou mais outras organizações")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat35$PP_collab)+0.05))
            ggplotly(plot35, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
            
         }
      }
   )
   output$plot20 <- renderPlotly(
      {
         dados <- dados %>% dplyr::filter(year==input$AnoP_int_collab)
         if(input$optionsescolha16 == "Null")
         {
            dat20 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University==input$options15,
                             Period==ifelse(input$AnoP_int_collab=="2020","2015–2018","2016–2019"), 
                             Frac_counting=="0")
            
            plot20 <- ggplot(dat20, aes(stringr::str_wrap(Field,width = 10), P_int_collab, fill=Field, label= round(P_int_collab, digits = 2), 
                                        text=paste("P_int_collab:",P_int_collab, "<br>", 
                                                   "Período:", Period))) +
               geom_col( show.legend = FALSE) + 
               xlab(paste0("Área Científica", "(",ifelse(input$AnoP_int_collab=="2020","2015–2018","2016–2019"),")")) + ylab("P_int_collab")+ ggtitle("O número de publicações de uma universidade que foram coautoria de dois ou mais países") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat20$P_int_collab)+500)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot20, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
            
         }else 
         {
            dat20 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University%in% c(input$options15,input$optionsescolha16),
                      Period=="2015–2018", Frac_counting == "0") %>%  select(1,9,23)
            plot20 <- dat20 %>% 
               ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), P_int_collab),y = P_int_collab, fill = University, label= round(P_int_collab, digits = 2), 
                          text=paste(University,"<br>","P_int_collab :",P_int_collab, "<br>", 
                                     "Período:", "2015-2018"))) +
               geom_col(position = "dodge", show.legend = FALSE) +
               xlab(paste0("Área Científica", "(",ifelse(input$AnoP_int_collab=="2020","2015–2018","2016–2019"),")")) + ylab("P_int_collab")+ ggtitle("O número de publicações de uma universidade que foram coautoria de dois ou mais países") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat20$P_int_collab)+500)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot20, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
         }
      }
   )
   output$plot36 <- renderPlotly(
      {
         dados <- dados %>% dplyr::filter(year==input$AnoImpact_P)
         if(input$optionsescolha17 == "Null")
         {
            dat36 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University==input$options31,
                      Period=="2015–2018", Frac_counting == "0")
            plot36 <- ggplot(dat36, aes(stringr::str_wrap(Field,width = 10), PP_int_collab, fill=Field, label= round(PP_int_collab, digits = 2), 
                                        text=paste("PP_int_collab :",PP_int_collab, "<br>", 
                                                   "Período:", Period))) +
               geom_col(show.legend = FALSE) + 
               xlab("Área Científica (2015–2018)") + ylab("PP_int_collab") + ggtitle("A proporção de publicações de uma universidade que foram coautoria de dois ou mais países")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat36$PP_int_collab)+0.05))
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot36, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
            
         }else
         {
            dat36 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University %in% c(input$options31,input$optionsescolha17),
                      Period=="2015–2018", Frac_counting == "0") %>% select(1,9,57)
            plot36 <- dat36 %>% 
               ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), PP_int_collab),y = PP_int_collab, fill = University, label= round(PP_int_collab, digits = 2), 
                          text=paste(University,"<br>","PP_int_collab :",PP_int_collab, "<br>", 
                                     "Período:", "2015-2018"))) +
               geom_col(position = "dodge", show.legend = FALSE) +
               xlab("Área Científica (2015–2018)") + ylab("PP_int_collab")+ ggtitle("Comparação: A proporção de publicações de uma universidade que foram coautoria de dois ou mais países")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat36$PP_int_collab)+0.05))
            ggplotly(plot36, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
            
         }
      }
   )
   output$plot21 <- renderPlotly(
      {
         dados <- dados %>% dplyr::filter(year==input$AnoP_industry_collab)
         if(input$optionsescolha18 == "Null")
         {
            dat21 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University==input$options16,
                             Period==ifelse(input$AnoP_int_collab=="2020","2015–2018","2016–2019"), 
                             Frac_counting=="0")
            
            plot21 <- ggplot(dat21, aes(stringr::str_wrap(Field,width = 10), P_industry_collab, fill=Field, label= round(P_industry_collab, digits = 2), 
                                        text=paste("P_industry_collab:",P_industry_collab, "<br>", 
                                                   "Período:", Period))) +
               geom_col( show.legend = FALSE) + 
               xlab(paste0("Área Científica", "(",ifelse(input$AnoP_industry_collab=="2020","2015–2018","2016–2019"),")")) + ylab("P_industry_collab")+ ggtitle("O número de publicações de uma universidade que foram coautoria de uma ou mais organizações industriais") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat21$P_industry_collab)+10)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot21, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
            
         }else
         {
            dat21 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University  %in% c(input$options16,input$optionsescolha18),
                      Period=="2015–2018", Frac_counting == "0") %>% select(1,9,24)
            plot21 <- dat21 %>% 
               ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), P_industry_collab),y = P_industry_collab, fill = University, label= round(P_industry_collab, digits = 2), 
                          text=paste(University,"<br>","P_industry_collab :",P_industry_collab, "<br>", 
                                     "Período:", "2015-2018"))) +
               geom_col(position = "dodge", show.legend = FALSE) +
               xlab(paste0("Área Científica", "(",ifelse(input$AnoP_industry_collab=="2020","2015–2018","2016–2019"),")")) + ylab("P_industry_collab")+ ggtitle("O número de publicações de uma universidade que foram coautoria de uma ou mais organizações industriais") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat21$P_industry_collab)+10)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot21, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
         }
         
      }
   )
   
   output$plot37 <- renderPlotly(
      {
         dados <- dados %>% dplyr::filter(year==input$AnoImpact_P)
         if(input$optionsescolha19 == "Null")
         {
            dat37 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University==input$options32,
                      Period=="2015–2018", Frac_counting == "0")
            plot37 <- ggplot(dat37,aes(stringr::str_wrap(Field,width = 10), PP_industry_collab, fill=Field, label= round(PP_industry_collab, digits = 2), 
                                       text=paste("PP_industry_collab :",PP_industry_collab, "<br>", 
                                                  "Período:", Period))) +
               geom_col(show.legend = FALSE) + 
               xlab("Área Científica (2015–2018)") + ylab("PP_industry_collab")+ ggtitle("A proporção de publicações de uma universidade que foram coautoria de uma ou mais organizações industriais") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat37$PP_industry_collab)))
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot37, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
            
         }else
         {
            dat37 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University %in% c(input$options32,input$optionsescolha19),
                      Period=="2015–2018", Frac_counting == "0") %>%  select(1,9,60)
            plot37 <- dat37 %>% 
               ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), PP_industry_collab),y = PP_industry_collab, fill = University, label= round(PP_industry_collab, digits = 2), 
                          text=paste(University,"<br>","PP_industry_collab :",PP_industry_collab, "<br>", 
                                     "Período:", "2015-2018"))) +
               geom_col(position = "dodge", show.legend = FALSE) +
               xlab("Área Científica (2015–2018)") + ylab("PP_industry_collab")+ ggtitle("Comparação: A proporção de publicações de uma universidade que foram coautoria de uma ou mais organizações industriais")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat37$PP_industry_collab)+0.05))
            ggplotly(plot37, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
            
         }
      }
   )
   
   output$plot22 <- renderPlotly(
      {
         dados <- dados %>% dplyr::filter(year==input$AnoP_short_dist_collab)
         if(input$optionsescolha20 == "Null")
         {
            dat22 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University==input$options17,
                             Period==ifelse(input$AnoP_short_dist_collab=="2020","2015–2018","2016–2019"), 
                             Frac_counting=="0")
            
            plot22 <- ggplot(dat22, aes(stringr::str_wrap(Field,width = 10), P_industry_collab, fill=Field, label= round(P_short_dist_collab, digits = 2), 
                                        text=paste("P_short_dist_collab:",P_short_dist_collab, "<br>", 
                                                   "Período:", Period))) +
               geom_col( show.legend = FALSE) + 
               xlab(paste0("Área Científica", "(",ifelse(input$AnoP_short_dist_collab=="2020","2015–2018","2016–2019"),")")) + ylab("P_short_dist_collab")+ ggtitle("O número de publicações de uma universidade com uma distância geográfica de colaboração inferior a 100 km.") +
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat22$P_short_dist_collab)+10)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot22, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
            
         }else
         {
            dat22 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University %in% c(input$options17,input$optionsescolha20),
                      Period=="2015–2018", Frac_counting == "0") %>% select(1,9,25)
            plot22 <- dat22 %>% 
               ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), P_short_dist_collab),y = P_short_dist_collab, fill = University, label= round(P_short_dist_collab, digits = 2), 
                          text=paste(University,"<br>","P_short_dist_collab :",P_short_dist_collab, "<br>", 
                                     "Período:", "2015-2018"))) +
               geom_col(position = "dodge", show.legend = FALSE) +
               xlab(paste0("Área Científica", "(",ifelse(input$AnoP_short_dist_collab=="2020","2015–2018","2016–2019"),")")) + ylab("P_short_dist_collab")+ ggtitle("O número de publicações de uma universidade com uma distância geográfica de colaboração inferior a 100 km.") +
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat22$P_short_dist_collab)+10)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot22, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
         }
      }
   )
   output$plot38 <- renderPlotly(
      {
         dados <- dados %>% dplyr::filter(year==input$AnoImpact_P)
         if(input$optionsescolha21 == "Null")
         {
            dat38 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University==input$options33,
                      Period=="2015–2018", Frac_counting == "0")
            plot38 <- ggplot(dat38, aes(stringr::str_wrap(Field,width = 10), PP_short_dist_collab, fill=Field, label= round(PP_short_dist_collab, digits = 2), 
                                        text=paste("PP_short_dist_collab :",PP_short_dist_collab, "<br>", 
                                                   "Período:", Period))) +
               geom_col(show.legend = FALSE) + 
               xlab("Área Científica (2015–2018)") + ylab("PP_short_dist_collab")+ ggtitle("A proporção de publicações de uma universidade com uma distância geográfica de colaboração inferior a 100 km.") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat38$PP_short_dist_collab)))
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot38, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
            
         }else
         {
            dat38 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University %in% c(input$options33,input$optionsescolha21),
                      Period=="2015–2018", Frac_counting == "0") %>%  select(1,9,63)
            plot38 <- dat38 %>% 
               ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), PP_short_dist_collab),y = PP_short_dist_collab, fill = University, label= round(PP_short_dist_collab, digits = 2), 
                          text=paste(University,"<br>","PP_short_dist_collab :",PP_short_dist_collab, "<br>", 
                                     "Período:", "2015-2018"))) +
               geom_col(position = "dodge", show.legend = FALSE) +
               xlab("Área Científica (2015–2018)") + ylab("PP_short_dist_collab")+ ggtitle("Comparação: A proporção de publicações de uma universidade com uma distância geográfica de colaboração inferior a 100 km.")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat38$PP_short_dist_collab)+0.05))
            ggplotly(plot38, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         }
      }
   )
   output$plot23 <- renderPlotly(
      {
         dados <- dados %>% dplyr::filter(year==input$AnoP_long_dist_collab)
         if(input$optionsescolha22 == "Null")
         {
            dat23 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University==input$options17,
                             Period==ifelse(input$AnoP_long_dist_collab=="2020","2015–2018","2016–2019"), 
                             Frac_counting=="0")
            
            plot23 <- ggplot(dat23, aes(stringr::str_wrap(Field,width = 10), P_industry_collab, fill=Field, label= round(P_long_dist_collab, digits = 2), 
                                        text=paste("P_long_dist_collab:",P_long_dist_collab, "<br>", 
                                                   "Período:", Period))) +
               geom_col( show.legend = FALSE) + 
               xlab(paste0("Área Científica", "(",ifelse(input$AnoP_long_dist_collab=="2020","2015–2018","2016–2019"),")")) + ylab("P_long_dist_collab")+ ggtitle("O número de publicações de uma universidade com uma distância geográfica de colaboração de mais de 5000 km.")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat23$P_long_dist_collab)+10)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot23, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
         
         }
         else
         {
            dat23 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University %in% c(input$options18,input$optionsescolha22),
                      Period=="2015–2018", Frac_counting == "0") %>% select(1,9,26)
            plot23 <- dat23 %>% 
               ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), P_long_dist_collab),y = P_long_dist_collab, fill = University, label= round(P_long_dist_collab, digits = 2), 
                          text=paste(University,"<br>","P_long_dist_collab :",P_long_dist_collab, "<br>", 
                                     "Período:", "2015-2018"))) +
               geom_col(position = "dodge", show.legend = FALSE) +
               xlab(paste0("Área Científica", "(",ifelse(input$AnoP_long_dist_collab=="2020","2015–2018","2016–2019"),")")) + ylab("P_long_dist_collab")+ ggtitle("O número de publicações de uma universidade com uma distância geográfica de colaboração de mais de 5000 km.")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat23$P_long_dist_collab)+10)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot23, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
            
         }
      }
   )
   output$plot39 <- renderPlotly(
      {
         dados <- dados %>% dplyr::filter(year==input$AnoImpact_P)
         if(input$optionsescolha23 == "Null")
         {
            dat39 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University==input$options34,
                      Period=="2015–2018", Frac_counting == "0")
            plot39 <- ggplot(dat39,aes(stringr::str_wrap(Field,width = 10), PP_long_dist_collab, fill=Field, label= round(PP_long_dist_collab, digits = 2), 
                                       text=paste("PP_long_dist_collab :",PP_long_dist_collab, "<br>", 
                                                  "Período:", Period))) +
               geom_col(show.legend = FALSE) + 
               xlab("Área Científica (2015–2018)") + ylab("PP_long_dist_collab") + ggtitle("A proporção de publicações de uma universidade com uma distância geográfica de colaboração de mais de 5000 km.")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat39$P_long_dist_collab)))
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot39, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
            
         }else
         {
            dat39 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University %in% c(input$options34,input$optionsescolha23),
                      Period=="2015–2018", Frac_counting == "0") %>% select(1,9,66)
            plot39 <- dat39 %>% 
               ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), PP_long_dist_collab),y = PP_long_dist_collab, fill = University, label= round(PP_long_dist_collab, digits = 2), 
                          text=paste(University,"<br>","PP_long_dist_collab :",PP_long_dist_collab, "<br>", 
                                     "Período:", "2015-2018"))) +
               geom_col(position = "dodge", show.legend = FALSE) +
               xlab("Área Científica (2015–2018)") + ylab("PP_long_dist_collab")+ ggtitle("Comparação: A proporção de publicações de uma universidade com uma distância geográfica de colaboração de mais de 5000 km.")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat39$PP_long_dist_collab)))
            ggplotly(plot39, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         }
      }
   )
   output$plot24 <- renderPlotly(
      {
         dados <- dados %>% dplyr::filter(year==input$AnoP_OA)
         if(input$optionsescolha24 ==  "Null")
         {
            dat24 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University==input$options19,
                             Period==ifelse(input$AnoP_OA=="2020","2015–2018","2016–2019"), 
                             Frac_counting=="0")
            
            plot24 <- ggplot(dat24, aes(stringr::str_wrap(Field,width = 10), P_OA, fill=Field, label= round(P_OA, digits = 2), 
                                        text=paste("P_OA:",P_OA, "<br>", 
                                                   "Período:", Period))) +
               geom_col( show.legend = FALSE) + 
               xlab(paste0("Área Científica", "(",ifelse(input$AnoP_OA=="2020","2015–2018","2016–2019"),")")) + ylab("P_OA")+ ggtitle("O número de publicações de acesso aberto de uma universidade.")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat24$P_OA)+10)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot24, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")

            
         }else
         {
            dat24 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University %in% c(input$options19,input$optionsescolha24),
                      Period=="2015–2018", Frac_counting ==  "0") %>% select(1,9,27)
            plot24 <- dat24 %>% 
               ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), P_OA),y = P_OA, fill = University, label= round(P_OA, digits = 2), 
                          text=paste(University,"<br>","P_OA :",P_OA, "<br>", 
                                     "Período:", "2015-2018"))) +
               geom_col(position = "dodge", show.legend = FALSE) +
               xlab(paste0("Área Científica", "(",ifelse(input$AnoP_OA=="2020","2015–2018","2016–2019"),")")) + ylab("P_OA")+ ggtitle("O número de publicações de acesso aberto de uma universidade.")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat24$P_OA)+10)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot24, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
         }
      }
   )
   output$plot40 <- renderPlotly(
      {
         dados <- dados %>% dplyr::filter(year==input$AnoImpact_P)
         if(input$optionsescolha25 == "Null")
         {
            dat40 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University==input$options35,
                      Period=="2015–2018", Frac_counting ==  "0") 
            plot40 <- ggplot(dat40,aes(stringr::str_wrap(Field,width = 10), PP_OA, fill=Field, label= round(PP_OA, digits = 2), 
                                       text=paste("PP_OA :",PP_OA, "<br>", 
                                                  "Período:", Period))) +
               geom_col(show.legend = FALSE) + 
               xlab("Área Científica (2015–2018)") + ylab("PP_OA") + ggtitle("A proporção de publicações de acesso aberto de uma universidade.")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat40$P_OA)))
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot40, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
            
         }else
         {
            dat40 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University %in% c(input$options35,input$optionsescolha25),
                      Period=="2015–2018", Frac_counting ==  "0") %>% select(1,9,69)
            plot40 <- dat40 %>% 
               ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), PP_OA),y = PP_OA, fill = University, label= round(PP_OA, digits = 2), 
                          text=paste(University,"<br>","PP_OA :",PP_OA, "<br>", 
                                     "Período:", "2015-2018"))) +
               geom_col(position = "dodge", show.legend = FALSE) +
               xlab("Área Científica (2015–2018)") + ylab("PP_OA")+ ggtitle("Comparação: A proporção de publicações de acesso aberto de uma universidade.")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat40$PP_OA)+0.05))
            ggplotly(plot40, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
            
         }
      }
   )
   output$plot25 <- renderPlotly(
      {
         dados <- dados %>% dplyr::filter(year==input$AnoP_gold_OA)
         if(input$optionsescolha26 == "Null")
         {
            dat25 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University==input$options20,
                             Period==ifelse(input$AnoP_gold_OA=="2020","2015–2018","2016–2019"), 
                             Frac_counting=="0")
            
            plot25 <- ggplot(dat25, aes(stringr::str_wrap(Field,width = 10), P_gold_OA, fill=Field, label= round(P_gold_OA, digits = 2), 
                                        text=paste("P_gold_OA:",P_gold_OA, "<br>", 
                                                   "Período:", Period))) +
               geom_col( show.legend = FALSE) + 
               xlab(paste0("Área Científica", "(",ifelse(input$AnoP_gold_OA=="2020","2015–2018","2016–2019"),")")) + ylab("P_gold_OA")+ ggtitle("O número de ouro em publicações de acesso aberto de uma universidade")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat25$P_gold_OA)+10)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot25, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
            
         }else
         {
            dat25 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University %in% c(input$options20,input$optionsescolha26),
                      Period=="2015–2018", Frac_counting ==  "0") %>% select(1,9,28)
            plot25 <- dat25 %>% 
               ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), P_gold_OA),y = P_gold_OA, fill = University, label= round(P_gold_OA, digits = 2), 
                          text=paste(University,"<br>","P_gold_OA :",P_gold_OA, "<br>", 
                                     "Período:", "2015-2018"))) +
               geom_col(position = "dodge", show.legend = FALSE) +
               xlab(paste0("Área Científica", "(",ifelse(input$AnoP_gold_OA=="2020","2015–2018","2016–2019"),")")) + ylab("P_gold_OA")+ ggtitle("O número de ouro em publicações de acesso aberto de uma universidade")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat25$P_gold_OA)+10)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot25, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
         }
      }
   )
   output$plot41 <- renderPlotly(
      {
         dados <- dados %>% dplyr::filter(year==input$AnoImpact_P)
         if(input$optionsescolha27 == "Null")
         {
            dat41 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University==input$options36,
                      Period=="2015–2018", Frac_counting ==  "0")
            plot41 <-  ggplot(dat41,aes(stringr::str_wrap(Field,width = 10), PP_gold_OA, fill=Field, label= round(PP_gold_OA, digits = 2), 
                                        text=paste("PP_gold_OA :",PP_gold_OA, "<br>", 
                                                   "Período:", Period))) +
               geom_col(show.legend = FALSE) + 
               xlab("Área Científica (2015–2018)") + ylab("PP_gold_OA")+ ggtitle("A proporção de ouro em publicações de acesso aberto de uma universidade") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat41$P_gold_OA)))
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot41, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
            
         }else
         {
            dat41 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University %in% c(input$options36,input$optionsescolha27),
                      Period=="2015–2018", Frac_counting ==  "0") %>% select (1,9,72)
            plot41 <- dat41 %>% 
               ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), PP_gold_OA),y = PP_gold_OA, fill = University, label= round(PP_gold_OA, digits = 2), 
                          text=paste(University,"<br>","PP_gold_OA :",PP_gold_OA, "<br>", 
                                     "Período:", "2015-2018"))) +
               geom_col(position = "dodge", show.legend = FALSE) +
               xlab("Área Científica (2015–2018)") + ylab("P_gold_OA")+ ggtitle("Comparação: A proporção de ouro em publicações de acesso aberto de uma universidade")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat41$PP_gold_OA)+0.05))
            ggplotly(plot41, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         }
      }
   )
   output$plot26 <- renderPlotly(
      {
         dados <- dados %>% dplyr::filter(year==input$AnoP_hybrid_OA)
         if(input$optionsescolha28 == "Null")
         {
            dat26 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University==input$options21,
                             Period==ifelse(input$AnoP_hybrid_OA=="2020","2015–2018","2016–2019"), 
                             Frac_counting=="0")
            
            plot26 <- ggplot(dat26, aes(stringr::str_wrap(Field,width = 10), P_hybrid_OA, fill=Field, label= round(P_hybrid_OA, digits = 2), 
                                        text=paste("P_hybrid_OA:",P_hybrid_OA, "<br>", 
                                                   "Período:", Period))) +
               geom_col( show.legend = FALSE) + 
               xlab(paste0("Área Científica", "(",ifelse(input$AnoP_hybrid_OA=="2020","2015–2018","2016–2019"),")")) + ylab("P_hybrid_OA")+ ggtitle("O número de publicações híbridas de acesso aberto de uma universidade")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat26$P_hybrid_OA)+10)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot26, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
            
            
         }else
         {
            dat26 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University %in% c(input$options21,input$optionsescolha28),
                      Period=="2015–2018", Frac_counting ==  "0") %>% select(1,9,29)
            plot26 <- dat26 %>% 
               ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), P_hybrid_OA),y = P_hybrid_OA, fill = University, label= round(P_hybrid_OA, digits = 2), 
                          text=paste(University,"<br>","P_hybrid_OA :",P_hybrid_OA, "<br>", 
                                     "Período:", "2015-2018"))) +
               geom_col(position = "dodge", show.legend = FALSE) +
               xlab(paste0("Área Científica", "(",ifelse(input$AnoP_hybrid_OA=="2020","2015–2018","2016–2019"),")")) + ylab("P_hybrid_OA")+ ggtitle("O número de publicações híbridas de acesso aberto de uma universidade")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat26$P_hybrid_OA)+10)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot26, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
         }
      }
   )
   output$plot42 <- renderPlotly(
      {
         dados <- dados %>% dplyr::filter(year==input$AnoImpact_P)
         if(input$optionsescolha29 == "Null")
         {
            dat42 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University==input$options37,
                      Period=="2015–2018", Frac_counting ==  "0")
            plot42 <- ggplot(dat42, aes(stringr::str_wrap(Field,width = 10), PP_hybrid_OA, fill=Field, label= round(PP_hybrid_OA, digits = 2), 
                                        text=paste("PP_hybrid_OA :",PP_hybrid_OA, "<br>", 
                                                   "Período:", Period))) +
               geom_col(show.legend = FALSE) + 
               xlab("Área Científica (2015–2018)") + ylab("PP_hybrid_OA") + ggtitle("A proporção de publicações híbridas de acesso aberto de uma universidade") +
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat42$PP_hybrid_OA)))
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot42, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
            
         }else
         {
            dat42 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University %in% c(input$options37,input$optionsescolha29),
                      Period=="2015–2018", Frac_counting ==  "0") %>% select(1,9,75)
            plot42 <- dat42 %>% 
               ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), PP_hybrid_OA),y = PP_hybrid_OA, fill = University, label= round(PP_hybrid_OA, digits = 2), 
                          text=paste(University,"<br>","PP_hybrid_OA :",PP_hybrid_OA, "<br>", 
                                     "Período:", "2015-2018"))) +
               geom_col(position = "dodge", show.legend = FALSE) +
               xlab("Área Científica (2015–2018)") + ylab("PP_hybrid_OA")+  ggtitle("Comparação: A proporção de publicações híbridas de acesso aberto de uma universidade") +
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat42$PP_hybrid_OA)+0.05))
            ggplotly(plot42, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         }
      }
   )
   output$plot27 <- renderPlotly(
      {
         dados <- dados %>% dplyr::filter(year==input$AnoP_bronze_OA)
         if(input$optionsescolha30 == "Null")
         {
            dat27 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University==input$options22,
                             Period==ifelse(input$AnoP_bronze_OA=="2020","2015–2018","2016–2019"), 
                             Frac_counting=="0")
            
            plot27 <- ggplot(dat27, aes(stringr::str_wrap(Field,width = 10), P_hybrid_OA, fill=Field, label= round(P_bronze_OA, digits = 2), 
                                        text=paste("P_bronze_OA:",P_bronze_OA, "<br>", 
                                                   "Período:", Period))) +
               geom_col( show.legend = FALSE) + 
               xlab(paste0("Área Científica", "(",ifelse(input$AnoP_bronze_OA=="2020","2015–2018","2016–2019"),")")) + ylab("P_bronze_OA")+ ggtitle("O número de publicações de acesso aberto em bronze de uma universidade")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat27$P_bronze_OA)+10)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot27, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")

            
         }else{
            dat27 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University %in% c(input$options22,input$optionsescolha30),
                      Period=="2015–2018", Frac_counting == "0") %>% select(1,9,30)
            plot27 <- dat27 %>% 
               ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), P_bronze_OA),y = P_bronze_OA, fill = University, label= round(P_bronze_OA, digits = 2), 
                          text=paste(University,"<br>","P_bronze_OA :",P_bronze_OA, "<br>", 
                                     "Período:", "2015-2018"))) +
               geom_col(position = "dodge", show.legend = FALSE) +
               xlab(paste0("Área Científica", "(",ifelse(input$AnoP_bronze_OA=="2020","2015–2018","2016–2019"),")")) + ylab("P_bronze_OA")+ ggtitle("O número de publicações de acesso aberto em bronze de uma universidade")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat27$P_bronze_OA)+10)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot27, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
         }
      }
   )
   output$plot43 <- renderPlotly(
      {
         dados <- dados %>% dplyr::filter(year==input$AnoImpact_P)
         if(input$optionsescolha31 == "Null")
         {
            dat43 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University==input$options38,
                      Period=="2015–2018", Frac_counting == "0") 
            plot43 <- ggplot(dat43,aes(stringr::str_wrap(Field,width = 10), PP_bronze_OA, fill=Field, label= round(PP_bronze_OA, digits = 2), 
                                       text=paste("PP_bronze_OA :",PP_bronze_OA, "<br>", 
                                                  "Período:", Period))) +
               geom_col(show.legend = FALSE) + 
               xlab("Área Científica (2015–2018)") + ylab("PP_bronze_OA") + ggtitle("A proporção de publicações de acesso aberto em bronze de uma universidade")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat43$PP_bronze_OA)))
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot43, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
            
         }else
         {
            dat43 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University %in% c(input$options38,input$optionsescolha31),
                      Period=="2015–2018", Frac_counting == "0") %>% select(1,9,78)
            plot43 <- dat43 %>% 
               ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), PP_bronze_OA),y = PP_bronze_OA, fill = University, label= round(PP_bronze_OA, digits = 2), 
                          text=paste(University,"<br>","PP_bronze_OA :",PP_bronze_OA, "<br>", 
                                     "Período:", "2015-2018"))) +
               geom_col(position = "dodge", show.legend = FALSE) +
               xlab("Área Científica (2015–2018)") + ylab("PP_bronze_OA")+ggtitle("Comparação: A proporção de publicações de acesso aberto em bronze de uma universidade")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat43$PP_bronze_OA)+0.05))
            ggplotly(plot43, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         }
      }
   )
   output$plot28 <- renderPlotly(
      {
         dados <- dados %>% dplyr::filter(year==input$AnoP_green_OA)
         if(input$optionsescolha32 == "Null")
         {
            dat28 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University==input$options23,
                             Period==ifelse(input$AnoP_green_OA=="2020","2015–2018","2016–2019"), 
                             Frac_counting=="0")
            
            plot28 <- ggplot(dat28, aes(stringr::str_wrap(Field,width = 10), P_green_OA, fill=Field, label= round(P_green_OA, digits = 2), 
                                        text=paste("P_green_OA:",P_green_OA, "<br>", 
                                                   "Período:", Period))) +
               geom_col( show.legend = FALSE) + 
               xlab(paste0("Área Científica", "(",ifelse(input$AnoP_green_OA=="2020","2015–2018","2016–2019"),")")) + ylab("P_green_OA")+ ggtitle("O número de publicações verdes de acesso aberto de uma universidade")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat28$P_green_OA)+10)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot28, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
            
         }else
         {
            dat28 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University %in% c(input$options23,input$optionsescolha32),
                      Period=="2015–2018", Frac_counting == "0") %>% select(1,9,31)
            plot28 <-  dat28 %>% 
               ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), P_green_OA),y = P_green_OA, fill = University, label= round(P_green_OA, digits = 2), 
                          text=paste(University,"<br>","P_green_OA :",P_green_OA, "<br>", 
                                     "Período:", "2015-2018"))) +
               geom_col(position = "dodge", show.legend = FALSE) +
               xlab(paste0("Área Científica", "(",ifelse(input$AnoP_green_OA=="2020","2015–2018","2016–2019"),")")) + ylab("P_green_OA")+ ggtitle("O número de publicações verdes de acesso aberto de uma universidade")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat28$P_green_OA)+10)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot28, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
         }
         
      }
   )
   output$plot44 <- renderPlotly(
      {
         dados <- dados %>% dplyr::filter(year==input$AnoImpact_P)
         if(input$optionsescolha33 == "Null")
         {
            dat44 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University==input$options39,
                      Period=="2015–2018", Frac_counting == "0")
            plot44 <-  ggplot(dat44,aes(stringr::str_wrap(Field,width = 10), PP_green_OA, fill=Field, label= round(PP_green_OA, digits = 2), 
                                        text=paste("PP_green_OA :",PP_green_OA, "<br>", 
                                                   "Período:", Period))) +
               geom_col(show.legend = FALSE) + 
               xlab("Área Científica (2015–2018)") + ylab("PP_green_OA") + ggtitle("A proporção de publicações verdes de acesso aberto de uma universidade")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat44$PP_green_OA)))
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot44, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
            
         }else
         {
            dat44 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University %in% c(input$options39,input$optionsescolha33),
                      Period=="2015–2018", Frac_counting == "0") %>% select(1,9,81)
            plot44 <-  dat44 %>% 
               ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), PP_green_OA),y = PP_green_OA, fill = University, label= round(PP_green_OA, digits = 2), 
                          text=paste(University,"<br>","PP_green_OA :",PP_green_OA, "<br>", 
                                     "Período:", "2015-2018"))) +
               geom_col(position = "dodge", show.legend = FALSE) +
               xlab("Área Científica (2015–2018)") + ylab("PP_green_OA")+ ggtitle("Comparação: A proporção de publicações verdes de acesso aberto de uma universidade")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat44$PP_green_OA)+0.05))
            ggplotly(plot44, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         }
      }
   )
   output$plot29 <- renderPlotly(
      {
         dados <- dados %>% dplyr::filter(year==input$AnoP_OA_unknown)
         if(input$optionsescolha34 == "Null")
         {
            dat29 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University==input$options24,
                             Period==ifelse(input$AnoP_OA_unknown=="2020","2015–2018","2016–2019"), 
                             Frac_counting=="0")
            
            plot29 <- ggplot(dat29, aes(stringr::str_wrap(Field,width = 10), P_OA_unknown, fill=Field, label= round(P_OA_unknown, digits = 2), 
                                        text=paste("P_OA_unknown:",P_OA_unknown, "<br>", 
                                                   "Período:", Period))) +
               geom_col( show.legend = FALSE) + 
               xlab(paste0("Área Científica", "(",ifelse(input$AnoP_OA_unknown=="2020","2015–2018","2016–2019"),")")) + ylab("P_OA_unknown")+ ggtitle("O número de publicações de uma universidade para as quais o status de acesso aberto é desconhecido")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat29$P_OA_unknown)+10)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot29, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")

            
         }else
         {
            dat29 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University %in% c(input$options24,input$optionsescolha34),
                      Period=="2015–2018", Frac_counting == "0") %>% select(1,9,32)
            plot29 <-  dat29 %>% 
               ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), P_OA_unknown),y = P_OA_unknown, fill = University, label= round(P_OA_unknown, digits = 2), 
                          text=paste(University,"<br>","P_OA_unknown :",P_OA_unknown, "<br>", 
                                     "Período:", "2015-2018"))) +
               geom_col(position = "dodge", show.legend = FALSE) +
               xlab(paste0("Área Científica", "(",ifelse(input$AnoP_OA_unknown=="2020","2015–2018","2016–2019"),")")) + ylab("P_OA_unknown")+ ggtitle("O número de publicações de uma universidade para as quais o status de acesso aberto é desconhecido")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat29$P_OA_unknown)+10)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot29, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
         }
         
      }
   )
   output$plot45 <- renderPlotly(
      {
         dados <- dados %>% dplyr::filter(year==input$AnoImpact_P)
         if(input$optionsescolha35 == "Null")
         {
            dat45 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University==input$options40,
                      Period=="2015–2018", Frac_counting == "0")
            plot45 <-  ggplot(dat45,aes(stringr::str_wrap(Field,width = 10), PP_OA_unknown, fill=Field, label= round(PP_OA_unknown, digits = 2), 
                                        text=paste("PP_OA_unknown :",PP_OA_unknown, "<br>", 
                                                   "Período:", Period))) +
               geom_col(show.legend = FALSE) + 
               xlab("Área Científica (2015–2018)") + ylab("PP_OA_unknown")+ ggtitle("A proporção de publicações de uma universidade para as quais o status de acesso aberto é desconhecido") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) +theme_bw()+ylim(c(0,max(dat45$PP_OA_unknown)))
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot45, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
            
         }else
         {
            dat45 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University %in% c(input$options40,input$optionsescolha35),
                      Period=="2015–2018", Frac_counting == "0") %>%  select(1,9,84)
            plot45 <- dat45 %>% 
               ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), PP_OA_unknown),y = PP_OA_unknown, fill = University, label= round(PP_OA_unknown, digits = 2), 
                          text=paste(University,"<br>","PP_OA_unknown :",PP_OA_unknown, "<br>", 
                                     "Período:", "2015-2018"))) +
               geom_col(position = "dodge", show.legend = FALSE) +
               xlab("Área Científica (2015–2018)") + ylab("PP_OA_unknown")+ ggtitle("Comparação: A proporção de publicações de uma universidade para as quais o status de acesso aberto é desconhecido")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat45$PP_OA_unknown)+0.05))
            ggplotly(plot45, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         }
      }
   )
   output$plot30 <- renderPlotly(
      {
         dados <- dados %>% dplyr::filter(year==input$Anogender_A)
         if(input$optionsescolha36 == "Null")
         {
            dat30 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University==input$options25,
                             Period==ifelse(input$Anogender_A=="2020","2015–2018","2016–2019"), 
                             Frac_counting=="0")
            
            plot30 <- ggplot(dat30, aes(stringr::str_wrap(Field,width = 10), gender_A, fill=Field, label= round(gender_A, digits = 2), 
                                        text=paste("gender_A:",gender_A, "<br>", 
                                                   "Período:", Period))) +
               geom_col( show.legend = FALSE) + 
               xlab(paste0("Área Científica", "(",ifelse(input$Anogender_A=="2020","2015–2018","2016–2019"),")")) + ylab("gender_A")+ ggtitle("O número total de autorias de uma universidade")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat30$gender_A)+10)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot30, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
            
         }else
         {
            dat30 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University %in% c(input$options25,input$optionsescolha36),
                      Period=="2015–2018", Frac_counting == "0") %>% select(1,9,14)
            plot30 <- dat30 %>% 
               ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), gender_A),y = gender_A, fill = University, label= round(gender_A, digits = 2), 
                          text=paste(University,"<br>","gender_A :",gender_A, "<br>", 
                                     "Período:", "2015-2018"))) +
               geom_col(position = "dodge", show.legend = FALSE) +
               xlab(paste0("Área Científica", "(",ifelse(input$Anogender_A=="2020","2015–2018","2016–2019"),")")) + ylab("gender_A")+ ggtitle("O número total de autorias de uma universidade")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat30$gender_A)+10)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot30, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
         }
      }
   )
   output$plot31 <- renderPlotly(
      {
         dados <- dados %>% dplyr::filter(year==input$Anogender_A_MF)
         if(input$optionsescolha37 == "Null")
         {
            dat31 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University==input$options26,
                             Period==ifelse(input$Anogender_A_MF=="2020","2015–2018","2016–2019"), 
                             Frac_counting=="0")
            
            plot31 <- ggplot(dat31, aes(stringr::str_wrap(Field,width = 10), gender_A_MF, fill=Field, label= round(gender_A_MF, digits = 2), 
                                        text=paste("gender_A_MF:",gender_A_MF, "<br>", 
                                                   "Período:", Period))) +
               geom_col( show.legend = FALSE) + 
               xlab(paste0("Área Científica", "(",ifelse(input$Anogender_A_MF=="2020","2015–2018","2016–2019"),")")) + ylab("gender_A_MF")+ ggtitle("O número de autorias de uma universidade pelas quais o gênero é conhecido")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat31$gender_A_MF)+10)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot31, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
            
         }else
         {
            dat31 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University %in% c(input$options26,input$optionsescolha37),
                      Period=="2015–2018", Frac_counting == "0") %>% select(1,9,15)
            plot31 <- dat31 %>% 
               ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), gender_A_MF),y = gender_A_MF, fill = University, label= round(gender_A_MF, digits = 2), 
                          text=paste(University,"<br>","gender_A_MF :",gender_A_MF, "<br>", 
                                     "Período:", "2015-2018"))) +
               geom_col(position = "dodge", show.legend = FALSE) +
               xlab(paste0("Área Científica", "(",ifelse(input$Anogender_A_MF=="2020","2015–2018","2016–2019"),")")) + ylab("gender_A_MF")+ ggtitle("O número de autorias de uma universidade pelas quais o gênero é conhecido")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat31$gender_A_MF)+10)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot31, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
         }
      }
   )
   output$plot32 <- renderPlotly(
      {
         dados <- dados %>% dplyr::filter(year==input$AnoA_gender_unknown)
         if(input$optionsescolha38 == "Null")
         {
            dat32 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University==input$options27,
                             Period==ifelse(input$AnoA_gender_unknown=="2020","2015–2018","2016–2019"), 
                             Frac_counting=="0")
            
            plot32 <- ggplot(dat32, aes(stringr::str_wrap(Field,width = 10), A_gender_unknown, fill=Field, label= round(A_gender_unknown, digits = 2), 
                                        text=paste("A_gender_unknown:",A_gender_unknown, "<br>", 
                                                   "Período:", Period))) +
               geom_col( show.legend = FALSE) + 
               xlab(paste0("Área Científica", "(",ifelse(input$AnoA_gender_unknown=="2020","2015–2018","2016–2019"),")")) + ylab("A_gender_unknown")+ ggtitle("O número de autorias de uma universidade para as quais o gênero é desconhecido")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat32$A_gender_unknown)+10)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot32, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
            
         }else
         {
            dat32 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University %in% c(input$options27,input$optionsescolha38),
                      Period=="2015–2018", Frac_counting == "0") %>% select(1,9,33)
            plot32 <- dat32 %>% 
               ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), A_gender_unknown),y = A_gender_unknown, fill = University, label= round(A_gender_unknown, digits = 2), 
                          text=paste(University,"<br>","A_gender_unknown :",A_gender_unknown, "<br>", 
                                     "Período:", "2015-2018"))) +
               geom_col(position = "dodge", show.legend = FALSE) +
               xlab(paste0("Área Científica", "(",ifelse(input$AnoA_gender_unknown=="2020","2015–2018","2016–2019"),")")) + ylab("A_gender_unknown")+ ggtitle("O número de autorias de uma universidade para as quais o gênero é desconhecido")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat32$A_gender_unknown)+10)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot32, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
         }
      }
   )
   output$plot46 <- renderPlotly(
      {
         dados <- dados %>% dplyr::filter(year==input$AnoImpact_P)
         if(input$optionsescolha39 == "Null")
         {
            dat46 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University==input$options41,
                      Period=="2015–2018", Frac_counting == "0")
            plot46 <-ggplot(dat46, aes(stringr::str_wrap(Field,width = 10), PA_gender_unknown, fill=Field, label= round(PA_gender_unknown, digits = 2), 
                                       text=paste("PA_gender_unknown :",PA_gender_unknown, "<br>", 
                                                  "Período:", Period))) +
               geom_col(show.legend = FALSE) + 
               xlab("Área Científica (2015–2018)") + ylab("PA_gender_unknown") + ggtitle("A proporção de autorias de uma universidade para as quais o gênero é desconhecido")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat46$PA_gender_unknown)))
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot46, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
            
         }else
         {
            dat46 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University %in% c(input$options41,input$optionsescolha39),
                      Period=="2015–2018", Frac_counting == "0") %>% select(1,9,87)
            plot46 <- dat46 %>% 
               ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), PA_gender_unknown),y = PA_gender_unknown, fill = University, label= round(PA_gender_unknown, digits = 2), 
                          text=paste(University,"<br>","PA_gender_unknown :",PA_gender_unknown, "<br>", 
                                     "Período:", "2015-2018"))) +
               geom_col(position = "dodge", show.legend = FALSE) +
               xlab("Área Científica (2015–2018)") + ylab("PA_gender_unknown")+ggtitle("Comparação: A proporção de autorias de uma universidade para as quais o gênero é desconhecido")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat46$PA_gender_unknown)+0.05))
            ggplotly(plot46, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         }
         
      }
   )
   output$plot33 <- renderPlotly(
      {
         dados <- dados %>% dplyr::filter(year==input$AnoA_M)
         if(input$optionsescolha40 == "Null")
         {
            dat33 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University==input$options28,
                             Period==ifelse(input$AnoA_M=="2020","2015–2018","2016–2019"), 
                             Frac_counting=="0")
            
            plot33 <- ggplot(dat33, aes(stringr::str_wrap(Field,width = 10), A_M, fill=Field, label= round(A_M, digits = 2), 
                                        text=paste("A_M:",A_M, "<br>", 
                                                   "Período:", Period))) +
               geom_col( show.legend = FALSE) + 
               xlab(paste0("Área Científica", "(",ifelse(input$AnoA_M=="2020","2015–2018","2016–2019"),")")) + ylab("A_M")+ ggtitle("O número de autorias masculinas de uma universidade")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat33$A_M)+10)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot33, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
            
         }else
         {
            dat33 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University %in% c(input$options28,input$optionsescolha40),
                      Period=="2015–2018", Frac_counting == "0") %>% select(1,9,34)
            plot33 <- dat33 %>% 
               ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), A_M),y = A_M, fill = University, label= round(A_M, digits = 2), 
                          text=paste(University,"<br>","A_M :",A_M, "<br>", 
                                     "Período:", "2015-2018"))) +
               geom_col(position = "dodge", show.legend = FALSE) +
               xlab(paste0("Área Científica", "(",ifelse(input$AnoA_M=="2020","2015–2018","2016–2019"),")")) + ylab("A_M")+ ggtitle("O número de autorias masculinas de uma universidade")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat33$A_M)+10)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot33, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
         }
      }
   )
   output$plot47 <- renderPlotly(
      {
         dados <- dados %>% dplyr::filter(year==input$AnoImpact_P)
         if(input$optionsescolha41 == "Null")
         {
            dat47 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University==input$options42,
                      Period=="2015–2018", Frac_counting == "0") 
            plot47 <- ggplot(dat47,aes(stringr::str_wrap(Field,width = 10), PA_M, fill=Field, label= round(PA_M, digits = 2), 
                                       text=paste("PA_M :",PA_M, "<br>", 
                                                  "Período:", Period))) +
               geom_col(show.legend = FALSE) + 
               xlab("Área Científica (2015–2018)") + ylab("PA_M")+ ggtitle("A proporção de autorias masculinas de uma universidade") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat47$PA_M)))
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot47, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
            
         }else
         {
            dat47 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University %in% c(input$options42,input$optionsescolha41),
                      Period=="2015–2018", Frac_counting == "0") %>% select(1,9,88)
            plot47 <- dat47 %>% 
               ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), PA_M),y = PA_M, fill = University, label= round(PA_M, digits = 2), 
                          text=paste(University,"<br>","PA_M :",PA_M, "<br>", 
                                     "Período:", "2015-2018"))) +
               geom_col(position = "dodge", show.legend = FALSE) +
               xlab("Área Científica (2015–2018)") + ylab("PA_M")+ggtitle("Comparação: O número de autorias masculinas de uma universidade")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat47$PA_M)))
            ggplotly(plot47, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         }
         
      }
   )
   output$plot34 <- renderPlotly(
      {
         dados <- dados %>% dplyr::filter(year==input$AnoA_F)
         if(input$optionsescolha42 == "Null")
         {
            dat34 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University==input$options44,
                             Period==ifelse(input$AnoA_F=="2020","2015–2018","2016–2019"), 
                             Frac_counting=="0")
            
            plot34 <- ggplot(dat34, aes(stringr::str_wrap(Field,width = 10), A_F, fill=Field, label= round(A_F, digits = 2), 
                                        text=paste("A_F:",A_F, "<br>", 
                                                   "Período:", Period))) +
               geom_col( show.legend = FALSE) + 
               xlab(paste0("Área Científica", "(",ifelse(input$AnoA_F=="2020","2015–2018","2016–2019"),")")) + ylab("A_F")+ ggtitle("O número de autorias femininas de uma universidade")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat34$A_F)+10)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot34, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
            
         }else
         {
            dat34 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University %in% c(input$options44,input$optionsescolha42),
                      Period=="2015–2018", Frac_counting == "0") %>% select(1,9,35)
            plot34 <-  dat34 %>% 
               ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), A_F),y = A_F, fill = University, label= round(A_F, digits = 2), 
                          text=paste(University,"<br>","A_F :",A_F, "<br>", 
                                     "Período:", "2015-2018"))) +
               geom_col(position = "dodge", show.legend = FALSE) +
               xlab(paste0("Área Científica", "(",ifelse(input$AnoA_F=="2020","2015–2018","2016–2019"),")")) + ylab("A_F")+ ggtitle("O número de autorias femininas de uma universidade")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat34$A_F)+10)) + 
               theme(legend.position = "bottom")
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
            ggplotly(plot34, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y =-0.2)) %>% style(textposition = "top")
   
         }
      }
   )
   output$plot48 <- renderPlotly(
      {
         dados <- dados %>% dplyr::filter(year==input$AnoImpact_P)
         if(input$optionsescolha43 == "Null")
         {
            dat48 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University==input$options43,
                      Period=="2015–2018", Frac_counting == "0")
            plot48 <-  ggplot(dat48,aes(stringr::str_wrap(Field,width = 10), PA_F, fill=Field, label= round(PA_F, digits = 2), 
                                        text=paste("PA_F :",PA_F, "<br>", 
                                                   "Período:", Period))) +
               geom_col(show.legend = FALSE) + 
               xlab("Área Científica (2015–2018)") + ylab("PA_F") + ggtitle("A proporção de autorias femininas de uma universidade")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat48$PA_F)))
            #theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
            ggplotly(plot48, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
            
         }else
         {
            dat48 <-dados %>% dplyr::filter(Country=="BRAZIL") %>% 
               dplyr::filter(University %in% c(input$options43,input$optionsescolha43),
                      Period=="2015–2018", Frac_counting == "0") %>% select(1,9,89)
            plot48 <- dat48 %>%  
               ggplot(aes(x =reorder(stringr::str_wrap(Field,width = 10), PA_F),y = PA_F, fill = University, label= round(PA_F, digits = 2), 
                          text=paste(University,"<br>","PA_F :",PA_F, "<br>", 
                                     "Período:", "2015-2018"))) +
               geom_col(position = "dodge", show.legend = FALSE) +
               xlab("Área Científica (2015–2018)") + ylab("PA_F")+ggtitle("Comparação: A proporção de autorias femininas de uma universidade")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat48$PA_F)))
            ggplotly(plot48, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         }
         
      }
   )
   
   output$tableBrazil <- DT::renderDataTable({
      DT::datatable(brazil,class = 'cell-border stripe',
                    colnames = c("Universidade","Periodo Final","Impacto", "Colaboração","Num. Top 10","Prop. Top 10","Num. Colab. Emp."),
                    extensions = 'Buttons', options = list(
                       dom = 'Bfrtip',
                       buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print')
                       
                    ))
   })  
   

   
})
