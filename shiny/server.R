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
    
    
    
    output$plot01 <- renderPlotly({
       if(input$optionsescolha01 == "Null")
       {
          if(input$frac01 == "0")
          {
             dat01 <-dados %>% filter(Country=="BRAZIL") %>% 
                filter(University==input$options01,
                       Period=="2015–2018", 
                       Frac_counting=="0")
             
             plot01 <- ggplot(dat01, aes(stringr::str_wrap(Field,width = 10), impact_P, fill=Field, label= round(impact_P, digits = 2), 
                                         text=paste("impact_P% :",impact_P, "<br>", 
                                                    "Período:", Period))) +
                geom_col( show.legend = FALSE) + 
                xlab("Área Científica (2015–2018)") + ylab("impact_P")+ ggtitle("O número de publicações de uma universidade") + 
                geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat01$impact_P)+500))
             #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
             ggplotly(plot01, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
          }else
          {
             dat01 <-dados %>% filter(Country=="BRAZIL") %>% 
                filter(University==input$options01,
                       Period=="2015–2018", 
                       Frac_counting=="1")
             
             plot01 <- ggplot(dat01, aes(stringr::str_wrap(Field,width = 10), impact_P, fill=Field, label= round(impact_P, digits = 2), 
                                         text=paste("impact_P% :",impact_P, "<br>", 
                                                    "Período:", Period))) +
                geom_col( show.legend = FALSE) + 
                xlab("Área Científica (2015–2018)") + ylab("impact_P")+ ggtitle("O número de publicações de uma universidade") + 
                geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat01$impact_P)+500))
             #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
             ggplotly(plot01, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
          }
       }
       else
       {
          if(input$frac01 == "0")
          {
             dat01 <-dados %>% filter(Country=="BRAZIL") %>% 
                filter(University %in% c(input$options01,input$optionsescolha01),
                       Period=="2015–2018", 
                       Frac_counting=="0") %>% select(1,9,11)
             plot01 <- dat01 %>% 
                ggplot(aes(x =reorder(Field, impact_P),y = impact_P, fill = University, label= round(impact_P, digits = 2), 
                           text=paste(University,"<br>","impact_P% :",impact_P, "<br>", 
                                      "Período:", "2015-2018"))) +
                geom_col(position = "dodge", show.legend = FALSE) +
                xlab("Área Científica (2015–2018)") + ylab("impact_P")+ ggtitle("Comparação: O número de publicações de uma universidade") + 
                geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat01$impact_P)+500))
             ggplotly(plot01, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
             
          }else
          {
             dat01 <-dados %>% filter(Country=="BRAZIL") %>% 
                filter(University %in% c(input$options01,input$optionsescolha01),
                       Period=="2015–2018", 
                       Frac_counting=="1") %>% select(1,9,11)
             plot01 <- dat01 %>% 
                ggplot(aes(x =reorder(Field, impact_P),y = impact_P, fill = University, label= round(impact_P, digits = 2), 
                           text=paste(University,"<br>","impact_P% :",impact_P, "<br>", 
                                      "Período:", "2015-2018"))) +
                geom_col(position = "dodge", show.legend = FALSE) +
                xlab("Área Científica (2015–2018)") + ylab("impact_P")+ ggtitle("Comparação: O número de publicações de uma universidade") + 
                geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat01$impact_P)+500))
             ggplotly(plot01, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
          }
       }
       
       
    })
    
    output$textImp_p <- renderText({
       if(input$frac01 == "0"){
          HTML(paste("<b>","Observação:","</b>", "Vou escrever sobre não fracionada."))
          
       }else{
          "**Observação:** Vou escrever sobre  fracionada."
       }
    })
    
    output$plot13 <- renderPlotly(
       {
          if(input$optionsescolha03 == "Null")
            {
             if(input$frac7 == "0")
             {
                dat13 <-dados %>% filter(Country=="BRAZIL") %>% 
                   filter(University==input$options7,
                          Period=="2015–2018", 
                          Frac_counting=="0")
                plot13 <- ggplot(dat13, aes(stringr::str_wrap(Field,width = 10), PP_top1, fill=Field, label= round(PP_top1, digits = 2), 
                                            text=paste("PP Top 1% :",PP_top1, "<br>", 
                                                       "Período:", Period))) +
                   geom_col( show.legend = FALSE) + 
                   xlab("Área Científica (2015–2018)") + ylab("PP_top1")+ ggtitle("A proporção de publicações de uma universidade que pertencem ao 1% mais citado") + 
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat13$PP_top1)+0.001))
                #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
                ggplotly(plot13, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
                
             }else
             {
                dat13 <-dados %>% filter(Country=="BRAZIL") %>% 
                   filter(University==input$options7,
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
                  dat13 <-dados %>% filter(Country=="BRAZIL") %>% 
                     filter(University %in% c(input$options7,input$optionsescolha03),
                            Period=="2015–2018", 
                            Frac_counting=="0") %>% select(1,9,42)
                  plot13 <- dat13 %>% 
                     ggplot(aes(x =reorder(Field, PP_top1),y = PP_top1, fill = University, label= round(PP_top1, digits = 2), 
                                text=paste(University,"<br>","PP_top1% :",PP_top1, "<br>", 
                                           "Período:", "2015-2018"))) +
                     geom_col(position = "dodge", show.legend = FALSE) +
                     xlab("Área Científica (2015–2018)") + ylab("PP_top1")+ ggtitle("A proporção de publicações de uma universidade que pertencem ao 1% mais citado") + 
                     geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat13$PP_top1)+500))
                  ggplotly(plot13, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
                  
               }else
               {
                  dat13 <-dados %>% filter(Country=="BRAZIL") %>% 
                     filter(University==input$options7,
                            Period=="2015–2018", 
                            Frac_counting=="1")%>% select(1,9,42)
                  plot13 <- dat13 %>% 
                     ggplot(aes(x =reorder(Field, PP_top1),y = PP_top1, fill = University, label= round(PP_top1, digits = 2), 
                                text=paste(University,"<br>","PP_top1% :",PP_top1, "<br>", 
                                           "Período:", "2015-2018"))) +
                     geom_col(position = "dodge", show.legend = FALSE) +
                     xlab("Área Científica (2015–2018)") + ylab("PP_top1")+ ggtitle("A proporção de publicações de uma universidade que pertencem ao 1% mais citado") + 
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
       plot2 <- dados %>% filter(Country=="BRAZIL") %>% 
           filter(University=="UNIVERSIDADE FEDERAL DE VICOSA",
                  Period=="2015–2018", 
                  Frac_counting=="1") %>% 
           ggplot(aes(Field, impact_P, fill=Field, label= round(impact_P, digits = 2), 
                      text=paste("Produção:",impact_P, "<br>", 
                                 "Período:", Period))) +
           geom_col(aes(Field, impact_P), show.legend = FALSE) + 
          xlab("Área Científica (2015–2018)") + ylab("Número de Publicações com Impacto") + 
           geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw() +
           theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
       ggplotly(plot2, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
       
   }) 
   
   output$plot11 <- renderPlotly({
       
       plot11 <- dados %>% filter(Country=="BRAZIL") %>% 
           filter(University==input$options,
                  Period=="2015–2018", 
                  Frac_counting=="1") %>% 
           ggplot(aes(Field, impact_P, fill=Field, label= round(impact_P, digits = 2), 
                      text=paste("Produção:",impact_P, "<br>", 
                                 "Período:", Period))) +
           geom_col(aes(Field, impact_P), show.legend = FALSE) + 
          xlab("Área Científica (2015–2018)") + ylab("Número de Publicações com Impacto") + 
           geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw() +
           theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
       ggplotly(plot11, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
       
   })
   
   output$plot6 <- renderPlotly(
       {
         if(input$optionsescolha02 == "Null")
            {
               if(input$frac == "0")
               {
                  dat6 <-dados %>% filter(Country=="BRAZIL") %>% 
                     filter(University==input$options1,
                            Period=="2015–2018", 
                            Frac_counting=="0") 
                  plot6 <- ggplot(dat6, aes(stringr::str_wrap(Field,width = 10), P_top1, fill=Field, label= round(P_top1, digits = 2), 
                                            text=paste("P Top 1% :",P_top1, "<br>", 
                                                       "Período:", Period))) +
                     geom_col(show.legend = FALSE) + 
                     xlab("Área Científica (2015–2018)") + ylab("P_top1") + ggtitle("O número de publicações de uma universidade que pertencem ao 1% mais citado")+ 
                     geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat6$P_top1)+0.5))
                  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
                  ggplotly(plot6, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
               }else
               {
                  dat6 <-dados %>% filter(Country=="BRAZIL") %>% 
                     filter(University==input$options1,
                            Period=="2015–2018", 
                            Frac_counting=="1") 
                  plot6 <- ggplot(dat6, aes(stringr::str_wrap(Field,width = 10), P_top1, fill=Field, label= round(P_top1, digits = 2), 
                                            text=paste("P Top 1% :",P_top1, "<br>", 
                                                       "Período:", Period))) +
                     geom_col(show.legend = FALSE) + 
                     xlab("Área Científica (2015–2018)") + ylab("P_top1") + ggtitle("O número de publicações de uma universidade que pertencem ao 1% mais citado")+ 
                     geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat6$P_top1)+0.5))
                  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
                  ggplotly(plot6, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
               }
               
            }else{
               if(input$frac == "0")
               {
                  dat6 <-dados %>% filter(Country=="BRAZIL") %>% 
                     filter(University %in% c(input$options1,input$optionsescolha02),
                            Period=="2015–2018", 
                            Frac_counting=="0") %>% select(1,9,18)
                  plot6 <- dat6 %>% 
                     ggplot(aes(x =reorder(Field, P_top1),y = P_top1, fill = University, label= round(P_top1, digits = 2), 
                                text=paste(University,"<br>","P_top1% :",P_top1, "<br>", 
                                           "Período:", "2015-2018"))) +
                     geom_col(position = "dodge", show.legend = FALSE) +
                     xlab("Área Científica (2015–2018)") + ylab("P_top1")+ ggtitle("O número de publicações de uma universidade que pertencem ao 1% mais citado") + 
                     geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat6$P_top1)+500))
                  ggplotly(plot6, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
                  
               }else
               {
                  dat6 <-dados %>% filter(Country=="BRAZIL") %>% 
                     filter(University %in% c(input$options1,input$optionsescolha02),
                            Period=="2015–2018", 
                            Frac_counting=="1") %>% select(1,9,18)
                  plot6 <- dat6 %>% 
                     ggplot(aes(x =reorder(Field, P_top1),y = P_top1, fill = University, label= round(P_top1, digits = 2), 
                                text=paste(University,"<br>","P_top1% :",P_top1, "<br>", 
                                           "Período:", "2015-2018"))) +
                     geom_col(position = "dodge", show.legend = FALSE) +
                     xlab("Área Científica (2015–2018)") + ylab("P_top1")+ ggtitle("O número de publicações de uma universidade que pertencem ao 1% mais citado") + 
                     geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat6$P_top1)+500))
                  ggplotly(plot6, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
               }
         }
       }
   )
   
   output$plot7 <- renderPlotly(
       {
          if(input$optionsescolha04 == "Null")
             {
                
                if(input$frac2 == "0")
                {
                   dat7 <-dados %>% filter(Country=="BRAZIL") %>% 
                      filter(University==input$options2,
                             Period=="2015–2018", 
                             Frac_counting=="0")
                   plot7 <- ggplot(dat7,aes(stringr::str_wrap(Field,width = 10), P_top5, fill=Field, label= round(P_top5, digits = 2), 
                                            text=paste("P Top 5% :",P_top5, "<br>", 
                                                       "Período:", Period))) +
                      geom_col( show.legend = FALSE) + 
                      xlab("Área Científica (2015–2018)") + ylab("P_top5")+ ggtitle("O número de publicações de uma universidade que pertencem ao 5% mais citado") + 
                      geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat7$P_top5)+0.5))
                   #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
                   ggplotly(plot7, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
                }else
                {
                   dat7 <-dados %>% filter(Country=="BRAZIL") %>% 
                      filter(University==input$options2,
                             Period=="2015–2018", 
                             Frac_counting=="1")
                   plot7 <- ggplot(dat7,aes(stringr::str_wrap(Field,width = 10), P_top5, fill=Field, label= round(P_top5, digits = 2), 
                                            text=paste("P Top 5% :",P_top5, "<br>", 
                                                       "Período:", Period))) +
                      geom_col( show.legend = FALSE) + 
                      xlab("Área Científica (2015–2018)") + ylab("P_top5")+ ggtitle("O número de publicações de uma universidade que pertencem ao 5% mais citado") + 
                      geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat7$P_top5)+0.5))
                   #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
                   ggplotly(plot7, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
                }
             
            }else
            {
               if(input$frac2 == "0")
               {
                  dat7 <-dados %>% filter(Country=="BRAZIL") %>% 
                     filter(University %in% c(input$options2,input$optionsescolha04),
                            Period=="2015–2018", 
                            Frac_counting=="0") %>% select(1,9,19)
                  plot7 <- dat7 %>%  
                     ggplot(aes(x =reorder(Field, P_top5),y = P_top5, fill = University, label= round(P_top5, digits = 2), 
                                text=paste(University,"<br>","P_top5% :",P_top5, "<br>", 
                                           "Período:", "2015-2018"))) +
                     geom_col(position = "dodge", show.legend = FALSE) +
                     xlab("Área Científica (2015–2018)") + ylab("P_top5")+ ggtitle("O número de publicações de uma universidade que pertencem ao 5% mais citado") + 
                     geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat7$P_top5)+500))
                  ggplotly(plot7, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
                  
               }else
               {
                  dat7 <-dados %>% filter(Country=="BRAZIL") %>% 
                     filter(University %in% c(input$options2,input$optionsescolha04),
                            Period=="2015–2018", 
                            Frac_counting=="1") %>% select(1,9,19)
                  plot7 <- dat7 %>%  
                     ggplot(aes(x =reorder(Field, P_top5),y = P_top5, fill = University, label= round(P_top5, digits = 2), 
                                text=paste(University,"<br>","P_top5% :",P_top5, "<br>", 
                                           "Período:", "2015-2018"))) +
                     geom_col(position = "dodge", show.legend = FALSE) +
                     xlab("Área Científica (2015–2018)") + ylab("P_top5")+ ggtitle("O número de publicações de uma universidade que pertencem ao 5% mais citado") + 
                     geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat7$P_top5)+500))
                  ggplotly(plot7, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
                  
               }
            }
       }
   )
   output$plot14 <- renderPlotly(
       {
          if(input$optionsescolha05 == "Null")
          {
             if(input$frac8 == "0")
             {
                dat14 <-dados %>% filter(Country=="BRAZIL") %>% 
                   filter(University==input$options8,
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
                dat14 <-dados %>% filter(Country=="BRAZIL") %>% 
                   filter(University==input$options8,
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
                dat14 <-dados %>% filter(Country=="BRAZIL") %>% 
                   filter(University %in% c(input$options8,input$optionsescolha05),
                          Period=="2015–2018", 
                          Frac_counting=="0") %>% select(1,9,45)
                plot14 <- dat14 %>% 
                   ggplot(aes(x =reorder(Field, PP_top5),y = PP_top5, fill = University, label= round(PP_top5, digits = 2), 
                              text=paste(University,"<br>","PP_top5% :",PP_top5, "<br>", 
                                         "Período:", "2015-2018"))) +
                   geom_col(position = "dodge", show.legend = FALSE) +
                   xlab("Área Científica (2015–2018)") + ylab("PP_top5")+ ggtitle("A proporção de publicações de uma universidade que pertencem ao 5% mais citado") + 
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat14$PP_top5)+500))
                ggplotly(plot14, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
                
             }else
             {
                dat14 <-dados %>% filter(Country=="BRAZIL") %>% 
                   filter(University %in% c(input$options8,input$optionsescolha05),
                          Period=="2015–2018", 
                          Frac_counting=="1") %>% select(1,9,45)
                plot14 <- dat14 %>% 
                   ggplot(aes(x =reorder(Field, PP_top5),y = PP_top5, fill = University, label= round(PP_top5, digits = 2), 
                              text=paste(University,"<br>","PP_top5% :",PP_top5, "<br>", 
                                         "Período:", "2015-2018"))) +
                   geom_col(position = "dodge", show.legend = FALSE) +
                   xlab("Área Científica (2015–2018)") + ylab("PP_top5")+ ggtitle("A proporção de publicações de uma universidade que pertencem ao 5% mais citado") + 
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat14$PP_top5)+500))
                ggplotly(plot14, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
             }
             
          }
       }
   )
   
   output$plot8 <- renderPlotly(
       {
          if(input$optionsescolha06 ==  "Null")
          {
             if(input$frac3 == "0")
             {
                dat8 <-dados %>% filter(Country=="BRAZIL") %>% 
                   filter(University==input$options3,
                          Period=="2015–2018", 
                          Frac_counting=="0")
                plot8 <- ggplot(dat8,aes(stringr::str_wrap(Field,width = 10), P_top10, fill=Field, label= round(P_top10, digits = 2), 
                                         text=paste("P Top 10% :",P_top10, "<br>", 
                                                    "Período:", Period))) +
                   geom_col(show.legend = FALSE) + 
                   xlab("Área Científica (2015–2018)")+ ylab("P_top10")+ ggtitle("O número de publicações de uma universidade que pertencem ao 10% mais citado") + 
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat8$P_top10)+0.5))
                #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
                ggplotly(plot8, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
             }else
             {
                dat8 <-dados %>% filter(Country=="BRAZIL") %>% 
                   filter(University==input$options3,
                          Period=="2015–2018", 
                          Frac_counting=="1")
                plot8 <- ggplot(dat8,aes(stringr::str_wrap(Field,width = 10), P_top10, fill=Field, label= round(P_top10, digits = 2), 
                                         text=paste("P Top 10% :",P_top10, "<br>", 
                                                    "Período:", Period))) +
                   geom_col(show.legend = FALSE) + 
                   xlab("Área Científica (2015–2018)")+ ylab("P_top10")+ ggtitle("O número de publicações de uma universidade que pertencem ao 10% mais citado") + 
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat8$P_top10)+0.5))
                #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
                ggplotly(plot8, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
             }
          }else
          {
             if(input$frac3 == "0")
             {
                dat8 <-dados %>% filter(Country=="BRAZIL") %>% 
                   filter(University %in% c(input$options3,input$optionsescolha06),
                          Period=="2015–2018", 
                          Frac_counting=="0") %>% select(1,9,20)
                plot8 <- dat8 %>%  
                   ggplot(aes(x =reorder(Field, P_top10),y = P_top10, fill = University, label= round(P_top10, digits = 2), 
                              text=paste(University,"<br>","P_top10% :",P_top10, "<br>", 
                                         "Período:", "2015-2018"))) +
                   geom_col(position = "dodge", show.legend = FALSE) +
                   xlab("Área Científica (2015–2018)") + ylab("P_top10")+ ggtitle("O número de publicações de uma universidade que pertencem ao 10% mais citado") + 
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat8$P_top10)+500))
                ggplotly(plot8, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
             }else
             {
                dat8 <-dados %>% filter(Country=="BRAZIL") %>% 
                   filter(University %in% c(input$options3,input$optionsescolha06),
                          Period=="2015–2018", 
                          Frac_counting=="1") %>% select(1,9,20)
                plot8 <- dat8 %>%  
                   ggplot(aes(x =reorder(Field, P_top10),y = P_top10, fill = University, label= round(P_top10, digits = 2), 
                              text=paste(University,"<br>","P_top10% :",P_top10, "<br>", 
                                         "Período:", "2015-2018"))) +
                   geom_col(position = "dodge", show.legend = FALSE) +
                   xlab("Área Científica (2015–2018)") + ylab("P_top10")+ ggtitle("O número de publicações de uma universidade que pertencem ao 10% mais citado") + 
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat8$P_top10)+500))
                ggplotly(plot8, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
             }
          }
       }
   )
   output$plot15 <- renderPlotly(
       {
          if(input$optionsescolha07 == "Null")
          {
             if(input$frac9 == "0")
             {
                dat15 <-dados %>% filter(Country=="BRAZIL") %>% 
                   filter(University==input$options9,
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
                dat15 <-dados %>% filter(Country=="BRAZIL") %>% 
                   filter(University==input$options9,
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
                dat15 <-dados %>% filter(Country=="BRAZIL") %>% 
                   filter(University%in% c(input$options9,input$optionsescolha07),
                          Period=="2015–2018", 
                          Frac_counting=="0") %>% select(1,9,48)
                plot15 <- dat15 %>% 
                   ggplot(aes(x =reorder(Field, PP_top10),y = PP_top10, fill = University, label= round(PP_top10, digits = 2), 
                              text=paste(University,"<br>","PP_top10% :",PP_top10, "<br>", 
                                         "Período:", "2015-2018"))) +
                   geom_col(position = "dodge", show.legend = FALSE) +
                   xlab("Área Científica (2015–2018)") + ylab("PP_top10")+ ggtitle("A proporção de publicações de uma universidade que pertencem ao 10% mais citado")+ 
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat15$PP_top10)+500))
                ggplotly(plot15, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
             }else
             {
                dat15 <-dados %>% filter(Country=="BRAZIL") %>% 
                   filter(University%in% c(input$options9,input$optionsescolha07),
                          Period=="2015–2018", 
                          Frac_counting=="1") %>% select(1,9,48)
                plot15 <- dat15 %>% 
                   ggplot(aes(x =reorder(Field, PP_top10),y = PP_top10, fill = University, label= round(PP_top10, digits = 2), 
                              text=paste(University,"<br>","PP_top10% :",PP_top10, "<br>", 
                                         "Período:", "2015-2018"))) +
                   geom_col(position = "dodge", show.legend = FALSE) +
                   xlab("Área Científica (2015–2018)") + ylab("PP_top10")+ ggtitle("A proporção de publicações de uma universidade que pertencem ao 10% mais citado")+ 
                   geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat15$PP_top10)+500))
                ggplotly(plot15, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
             }
             
          }
          
       }
   )
   output$plot9 <- renderPlotly(
       {
          if(input$frac4 == "0")
          {
          
              dat9 <-dados %>% filter(Country=="BRAZIL") %>% 
                  filter(University==input$options4,
                         Period=="2015–2018", 
                         Frac_counting==input$frac4) 
              plot9 <- ggplot(dat9, aes(stringr::str_wrap(Field,width = 10), P_top50, fill=Field, label= round(P_top50, digits = 2), 
                             text=paste("P Top 50% :",P_top50, "<br>", 
                                        "Período:", Period))) +
                  geom_col(show.legend = FALSE) + 
                 xlab("Área Científica (2015–2018)")+ ylab("P_top50")+ ggtitle("O número de publicações de uma universidade que pertencem ao 50% mais citado") + 
                  geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat9$P_top50)+0.5))
                  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
              ggplotly(plot9, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
          }else
          {
             dat9 <-dados %>% filter(Country=="BRAZIL") %>% 
                filter(University==input$options4,
                       Period=="2015–2018", 
                       Frac_counting==input$frac4) 
             plot9 <- ggplot(dat9, aes(stringr::str_wrap(Field,width = 10), P_top50, fill=Field, label= round(P_top50, digits = 2), 
                                       text=paste("P Top 50% :",P_top50, "<br>", 
                                                  "Período:", Period))) +
                geom_col(show.legend = FALSE) + 
                xlab("Área Científica (2015–2018)")+ ylab("P_top50")+ ggtitle("O número de publicações de uma universidade que pertencem ao 50% mais citado") + 
                geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat9$P_top50)+0.5))
             #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
             ggplotly(plot9, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
          }
       }
   )
   output$plot16 <- renderPlotly(
       {
          if(input$frac10 == "0")
          {
              dat16 <-dados %>% filter(Country=="BRAZIL") %>% 
                  filter(University==input$options10,
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
             dat16 <-dados %>% filter(Country=="BRAZIL") %>% 
                filter(University==input$options10,
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
       }
   )
   output$plot10 <- renderPlotly(
       {
          if(input$frac5 == "0")
          {
              dat10 <-dados %>% filter(Country=="BRAZIL") %>% 
                  filter(University==input$options5,
                         Period=="2015–2018", 
                         Frac_counting=="0")
             plot10 <- ggplot(dat10,aes(stringr::str_wrap(Field,width = 10), TCS, fill=Field, label= round(TCS, digits = 2), 
                             text=paste("TCS :",TCS, "<br>", 
                                        "Período:", Period))) +
                  geom_col(show.legend = FALSE) + 
                xlab("Área Científica (2015–2018)") + ylab("TCS") + ggtitle("O número total de citações das publicações de uma universidade")+
                  geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat10$TCS)+1000))
                  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
              ggplotly(plot10, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
          }else
          {
             dat10 <-dados %>% filter(Country=="BRAZIL") %>% 
                filter(University==input$options5,
                       Period=="2015–2018", 
                       Frac_counting=="1")
             plot10 <- ggplot(dat10,aes(stringr::str_wrap(Field,width = 10), TCS, fill=Field, label= round(TCS, digits = 2), 
                                        text=paste("TCS :",TCS, "<br>", 
                                                   "Período:", Period))) +
                geom_col(show.legend = FALSE) + 
                xlab("Área Científica (2015–2018)") + ylab("TCS") + ggtitle("O número total de citações das publicações de uma universidade")+
                geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat10$TCS)+1000))
             #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
             ggplotly(plot10, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
          }
       }
   )
   output$plot17 <- renderPlotly(
       {
          if(input$frac11 == "0")
          {
              dat17 <-dados %>% filter(Country=="BRAZIL") %>% 
                  filter(University==input$options11,
                         Period=="2015–2018", 
                         Frac_counting=="0") 
              plot17 <- ggplot(dat17,aes(stringr::str_wrap(Field,width = 10), TNCS, fill=Field, label= round(TNCS, digits = 2), 
                             text=paste("TNCS :",TNCS, "<br>", 
                                        "Período:", Period))) +
                  geom_col(show.legend = FALSE) + 
                 xlab("Área Científica (2015–2018)")+ ylab("TNCS") + ggtitle("O número total de citações das publicações de uma universidade")+
                  geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat17$TNCS)+500))
                  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
              ggplotly(plot17, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
          }else
          {
             dat17 <-dados %>% filter(Country=="BRAZIL") %>% 
                filter(University==input$options11,
                       Period=="2015–2018", 
                       Frac_counting=="1") 
             plot17 <- ggplot(dat17,aes(stringr::str_wrap(Field,width = 10), TNCS, fill=Field, label= round(TNCS, digits = 2), 
                                        text=paste("TNCS :",TNCS, "<br>", 
                                                   "Período:", Period))) +
                geom_col(show.legend = FALSE) + 
                xlab("Área Científica (2015–2018)")+ ylab("TNCS") + ggtitle("O número total de citações das publicações de uma universidade")+
                geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat17$TNCS)+500))
             #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
             ggplotly(plot17, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
          }
       }
   )
   output$plot12 <- renderPlotly(
       {
          if(input$frac6 == "0")
          {
              dat12 <-dados %>% filter(Country=="BRAZIL") %>% 
                  filter(University==input$options6,
                         Period=="2015–2018", 
                         Frac_counting=="0") 
              plot12 <- ggplot(dat12,aes(stringr::str_wrap(Field,width = 10), MCS, fill=Field, label= round(MCS, digits = 2), 
                             text=paste("MCS :",MCS, "<br>", 
                                        "Período:", Period))) +
                  geom_col( show.legend = FALSE) + 
                 xlab("Área Científica (2015–2018)") + ylab("MCS") + ggtitle("O número médio de citações das publicações de uma universidade")+
                  geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat12$MCS)+1))
                  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
              ggplotly(plot12, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
          }else
          {
             dat12 <-dados %>% filter(Country=="BRAZIL") %>% 
                filter(University==input$options6,
                       Period=="2015–2018", 
                       Frac_counting=="1") 
             plot12 <- ggplot(dat12,aes(stringr::str_wrap(Field,width = 10), MCS, fill=Field, label= round(MCS, digits = 2), 
                                        text=paste("MCS :",MCS, "<br>", 
                                                   "Período:", Period))) +
                geom_col( show.legend = FALSE) + 
                xlab("Área Científica (2015–2018)") + ylab("MCS") + ggtitle("O número médio de citações das publicações de uma universidade")+
                geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat12$MCS)+1))
             #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
             ggplotly(plot12, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
          }
       }
   )
   output$plot18 <- renderPlotly(
       {
          if(input$frac12 == "0")
          {
              dat18 <-dados %>% filter(Country=="BRAZIL") %>% 
                  filter(University==input$options12,
                         Period=="2015–2018", 
                         Frac_counting=="0")
              plot18 <-ggplot(dat18,aes(stringr::str_wrap(Field,width = 10), MNCS, fill=Field, label= round(MNCS, digits = 2), 
                             text=paste("MNCS :",MNCS, "<br>", 
                                        "Período:", Period))) +
                  geom_col(show.legend = FALSE) + 
                 xlab("Área Científica (2015–2018)") + ylab("MNCS") + ggtitle("O número médio de citações das publicações de uma universidade por campo e ano de publicação")+
                  geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat18$MNCS)+0.05))
                  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
              ggplotly(plot18, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
          }else
          {
             dat18 <-dados %>% filter(Country=="BRAZIL") %>% 
                filter(University==input$options12,
                       Period=="2015–2018", 
                       Frac_counting=="1")
             plot18 <-ggplot(dat18,aes(stringr::str_wrap(Field,width = 10), MNCS, fill=Field, label= round(MNCS, digits = 2), 
                                       text=paste("MNCS :",MNCS, "<br>", 
                                                  "Período:", Period))) +
                geom_col(show.legend = FALSE) + 
                xlab("Área Científica (2015–2018)") + ylab("MNCS") + ggtitle("O número médio de citações das publicações de uma universidade por campo e ano de publicação")+
                geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat18$MNCS)+0.05))
             #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
             ggplotly(plot18, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
          }
       }
   )
   output$plot19 <- renderPlotly(
      {
         dat19 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options14,
                   Period=="2015–2018", Frac_counting == "0") 
         plot19 <- ggplot(dat19,aes(stringr::str_wrap(Field,width = 10), P_collab, fill=Field, label= round(P_collab, digits = 2), 
                       text=paste("P_collab :",P_collab, "<br>", 
                                  "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
            xlab("Área Científica (2015–2018)") + ylab("P_collab")+ggtitle("O número de publicações de uma universidade que foram coautoria de uma ou mais outras organizações") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat19$P_collab)+1000))
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot19, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot35 <- renderPlotly(
      {
         dat35 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options30,
                   Period=="2015–2018", Frac_counting == "0") 
         plot35 <- ggplot(dat35,aes(stringr::str_wrap(Field,width = 10), PP_collab, fill=Field, label= round(PP_collab, digits = 2), 
                                    text=paste("PP_collab :",PP_collab, "<br>", 
                                               "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
            xlab("Área Científica (2015–2018)") + ylab("PP_collab")+ggtitle("A proporção de publicações de uma universidade que foram coautoria de uma ou mais outras organizações") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat35$PP_collab)+0.05))
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot35, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot20 <- renderPlotly(
      {
         dat20 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options15,
                   Period=="2015–2018", Frac_counting == "0")
            plot20 <- ggplot(dat20, aes(stringr::str_wrap(Field,width = 10), P_int_collab, fill=Field, label= round(P_int_collab, digits = 2), 
                       text=paste("P_int_collab :",P_int_collab, "<br>", 
                                  "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
               xlab("Área Científica (2015–2018)") + ylab("P_int_collab") + ggtitle("O número de publicações de uma universidade que foram coautoria de dois ou mais países")+
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat20$P_int_collab)+1000))
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot20, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot36 <- renderPlotly(
      {
         dat36 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options31,
                   Period=="2015–2018", Frac_counting == "0")
         plot36 <- ggplot(dat36, aes(stringr::str_wrap(Field,width = 10), PP_int_collab, fill=Field, label= round(PP_int_collab, digits = 2), 
                                     text=paste("PP_int_collab :",PP_int_collab, "<br>", 
                                                "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
            xlab("Área Científica (2015–2018)") + ylab("PP_int_collab") + ggtitle("A proporção de publicações de uma universidade que foram coautoria de dois ou mais países")+
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat36$PP_int_collab)+0.05))
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot36, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot21 <- renderPlotly(
      {
         dat21 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options16,
                   Period=="2015–2018", Frac_counting == "0")
         plot21 <- ggplot(dat21,aes(stringr::str_wrap(Field,width = 10), P_industry_collab, fill=Field, label= round(P_industry_collab, digits = 2), 
                       text=paste("P_industry_collab :",P_industry_collab, "<br>", 
                                  "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
            xlab("Área Científica (2015–2018)") + ylab("P_industry_collab")+ ggtitle("O número de publicações de uma universidade que foram coautoria de uma ou mais organizações industriais") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat21$P_industry_collab)+20))
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot21, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   
   output$plot37 <- renderPlotly(
      {
         dat37 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options32,
                   Period=="2015–2018", Frac_counting == "0")
         plot37 <- ggplot(dat37,aes(stringr::str_wrap(Field,width = 10), PP_industry_collab, fill=Field, label= round(PP_industry_collab, digits = 2), 
                                    text=paste("PP_industry_collab :",PP_industry_collab, "<br>", 
                                               "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
            xlab("Área Científica (2015–2018)") + ylab("PP_industry_collab")+ ggtitle("A proporção de publicações de uma universidade que foram coautoria de uma ou mais organizações industriais") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat37$PP_industry_collab)))
         #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot37, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   
   output$plot22 <- renderPlotly(
      {
        dat22 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options17,
                   Period=="2015–2018", Frac_counting == "0")
            plot22 <- ggplot(dat22, aes(stringr::str_wrap(Field,width = 10), P_short_dist_collab, fill=Field, label= round(P_short_dist_collab, digits = 2), 
                       text=paste("P_short_dist_collab :",P_short_dist_collab, "<br>", 
                                  "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
               xlab("Área Científica (2015–2018)") + ylab("P_short_dist_collab")+ ggtitle("O número de publicações de uma universidade com uma distância geográfica de colaboração inferior a 100 km.") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat22$P_short_dist_collab)+20))
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot22, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot38 <- renderPlotly(
      {
         dat38 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options33,
                   Period=="2015–2018", Frac_counting == "0")
         plot38 <- ggplot(dat38, aes(stringr::str_wrap(Field,width = 10), PP_short_dist_collab, fill=Field, label= round(PP_short_dist_collab, digits = 2), 
                                     text=paste("PP_short_dist_collab :",PP_short_dist_collab, "<br>", 
                                                "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
            xlab("Área Científica (2015–2018)") + ylab("PP_short_dist_collab")+ ggtitle("A proporção de publicações de uma universidade com uma distância geográfica de colaboração inferior a 100 km.") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat38$PP_short_dist_collab)))
         #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot38, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot23 <- renderPlotly(
      {
         dat23 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options18,
                   Period=="2015–2018", Frac_counting == "0")
            plot23 <- ggplot(dat23,aes(stringr::str_wrap(Field,width = 10), P_long_dist_collab, fill=Field, label= round(P_long_dist_collab, digits = 2), 
                       text=paste("P_long_dist_collab :",P_long_dist_collab, "<br>", 
                                  "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
               xlab("Área Científica (2015–2018)") + ylab("P_long_dist_collab") + ggtitle("O número de publicações de uma universidade com uma distância geográfica de colaboração de mais de 5000 km.")+
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat23$P_long_dist_collab)+1000))
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot23, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot39 <- renderPlotly(
      {
         dat39 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options34,
                   Period=="2015–2018", Frac_counting == "0")
         plot39 <- ggplot(dat39,aes(stringr::str_wrap(Field,width = 10), PP_long_dist_collab, fill=Field, label= round(PP_long_dist_collab, digits = 2), 
                                    text=paste("PP_long_dist_collab :",PP_long_dist_collab, "<br>", 
                                               "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
            xlab("Área Científica (2015–2018)") + ylab("PP_long_dist_collab") + ggtitle("A proporção de publicações de uma universidade com uma distância geográfica de colaboração de mais de 5000 km.")+
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat39$P_long_dist_collab)))
         #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot39, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot24 <- renderPlotly(
      {
         dat24 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options19,
                   Period=="2015–2018", Frac_counting ==  "0") 
           plot24 <- ggplot(dat24,aes(stringr::str_wrap(Field,width = 10), P_OA, fill=Field, label= round(P_OA, digits = 2), 
                       text=paste("P_OA :",P_OA, "<br>", 
                                  "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
              xlab("Área Científica (2015–2018)") + ylab("P_OA") + ggtitle("O número de publicações de acesso aberto de uma universidade.")+
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat24$P_OA)+20))
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot24, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot40 <- renderPlotly(
      {
         dat40 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options35,
                   Period=="2015–2018", Frac_counting ==  "0") 
         plot40 <- ggplot(dat40,aes(stringr::str_wrap(Field,width = 10), PP_OA, fill=Field, label= round(PP_OA, digits = 2), 
                                    text=paste("PP_OA :",PP_OA, "<br>", 
                                               "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
            xlab("Área Científica (2015–2018)") + ylab("PP_OA") + ggtitle("A proporção de publicações de acesso aberto de uma universidade.")+
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat40$P_OA)))
         #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot40, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot25 <- renderPlotly(
      {
         dat25 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options20,
                   Period=="2015–2018", Frac_counting ==  "0")
          plot25 <-  ggplot(dat25,aes(stringr::str_wrap(Field,width = 10), P_gold_OA, fill=Field, label= round(P_gold_OA, digits = 2), 
                       text=paste("P_gold_OA :",P_gold_OA, "<br>", 
                                  "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
             xlab("Área Científica (2015–2018)") + ylab("P_gold_OA")+ ggtitle("O número de ouro em publicações de acesso aberto de uma universidade") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat25$P_gold_OA)+20))
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot25, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot41 <- renderPlotly(
      {
         dat41 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options36,
                   Period=="2015–2018", Frac_counting ==  "0")
         plot41 <-  ggplot(dat41,aes(stringr::str_wrap(Field,width = 10), PP_gold_OA, fill=Field, label= round(PP_gold_OA, digits = 2), 
                                     text=paste("PP_gold_OA :",PP_gold_OA, "<br>", 
                                                "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
            xlab("Área Científica (2015–2018)") + ylab("PP_gold_OA")+ ggtitle("A proporção de ouro em publicações de acesso aberto de uma universidade") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat41$P_gold_OA)))
         #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot41, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot26 <- renderPlotly(
      {
         dat26 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options21,
                   Period=="2015–2018", Frac_counting ==  "0")
            plot26 <- ggplot(dat26, aes(stringr::str_wrap(Field,width = 10), P_hybrid_OA, fill=Field, label= round(P_hybrid_OA, digits = 2), 
                       text=paste("P_hybrid_OA :",P_hybrid_OA, "<br>", 
                                  "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
               xlab("Área Científica (2015–2018)") + ylab("P_hybrid_OA") + ggtitle("O número de publicações híbridas de acesso aberto de uma universidade") +
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat26$P_hybrid_OA)+20))
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot26, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot42 <- renderPlotly(
      {
         dat42 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options37,
                   Period=="2015–2018", Frac_counting ==  "0")
         plot42 <- ggplot(dat42, aes(stringr::str_wrap(Field,width = 10), PP_hybrid_OA, fill=Field, label= round(PP_hybrid_OA, digits = 2), 
                                     text=paste("PP_hybrid_OA :",PP_hybrid_OA, "<br>", 
                                                "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
            xlab("Área Científica (2015–2018)") + ylab("PP_hybrid_OA") + ggtitle("A proporção de publicações híbridas de acesso aberto de uma universidade") +
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat42$PP_hybrid_OA)))
         #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot42, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot27 <- renderPlotly(
      {
         dat27 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options22,
                   Period=="2015–2018", Frac_counting == "0") 
            plot27 <- ggplot(dat27,aes(stringr::str_wrap(Field,width = 10), P_bronze_OA, fill=Field, label= round(P_bronze_OA, digits = 2), 
                       text=paste("P_bronze_OA :",P_bronze_OA, "<br>", 
                                  "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
               xlab("Área Científica (2015–2018)") + ylab("P_bronze_OA") + ggtitle("O número de publicações de acesso aberto em bronze de uma universidade")+
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat27$P_bronze_OA)+20))
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot27, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot43 <- renderPlotly(
      {
         dat43 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options38,
                   Period=="2015–2018", Frac_counting == "0") 
         plot43 <- ggplot(dat43,aes(stringr::str_wrap(Field,width = 10), PP_bronze_OA, fill=Field, label= round(PP_bronze_OA, digits = 2), 
                                    text=paste("PP_bronze_OA :",PP_bronze_OA, "<br>", 
                                               "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
            xlab("Área Científica (2015–2018)") + ylab("PP_bronze_OA") + ggtitle("A proporção de publicações de acesso aberto em bronze de uma universidade")+
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat43$PP_bronze_OA)))
         #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot43, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot28 <- renderPlotly(
      {
         dat28 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options23,
                   Period=="2015–2018", Frac_counting == "0")
          plot28 <-  ggplot(dat28,aes(stringr::str_wrap(Field,width = 10), P_green_OA, fill=Field, label= round(P_green_OA, digits = 2), 
                       text=paste("P_green_OA :",P_green_OA, "<br>", 
                                  "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
             xlab("Área Científica (2015–2018)") + ylab("P_green_OA") + ggtitle("O número de publicações verdes de acesso aberto de uma universidade")+
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat28$P_green_OA)+20))
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot28, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot44 <- renderPlotly(
      {
         dat44 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options39,
                   Period=="2015–2018", Frac_counting == "0")
         plot44 <-  ggplot(dat44,aes(stringr::str_wrap(Field,width = 10), PP_green_OA, fill=Field, label= round(PP_green_OA, digits = 2), 
                                     text=paste("PP_green_OA :",PP_green_OA, "<br>", 
                                                "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
            xlab("Área Científica (2015–2018)") + ylab("PP_green_OA") + ggtitle("A proporção de publicações verdes de acesso aberto de uma universidade")+
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat44$PP_green_OA)))
         #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot44, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot29 <- renderPlotly(
      {
         dat29 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options24,
                   Period=="2015–2018", Frac_counting == "0")
          plot29 <-  ggplot(dat29,aes(stringr::str_wrap(Field,width = 10), P_OA_unknown, fill=Field, label= round(P_OA_unknown, digits = 2), 
                       text=paste("P_OA_unknown :",P_OA_unknown, "<br>", 
                                  "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
             xlab("Área Científica (2015–2018)") + ylab("P_OA_unknown")+ ggtitle("O número de publicações de uma universidade para as quais o status de acesso aberto é desconhecido") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) +theme_bw()+ylim(c(0,max(dat29$P_OA_unknown)+20))
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot29, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot45 <- renderPlotly(
      {
         dat45 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options40,
                   Period=="2015–2018", Frac_counting == "0")
         plot45 <-  ggplot(dat45,aes(stringr::str_wrap(Field,width = 10), PP_OA_unknown, fill=Field, label= round(PP_OA_unknown, digits = 2), 
                                     text=paste("PP_OA_unknown :",PP_OA_unknown, "<br>", 
                                                "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
            xlab("Área Científica (2015–2018)") + ylab("PP_OA_unknown")+ ggtitle("A proporção de publicações de uma universidade para as quais o status de acesso aberto é desconhecido") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) +theme_bw()+ylim(c(0,max(dat45$PP_OA_unknown)))
         #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot45, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot30 <- renderPlotly(
      {
         dat30 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options25,
                   Period=="2015–2018", Frac_counting == "0")
           plot30 <- ggplot(dat30,aes(stringr::str_wrap(Field,width = 10), gender_A, fill=Field, label= round(gender_A, digits = 2), 
                       text=paste("gender_A :",gender_A, "<br>", 
                                  "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
              xlab("Área Científica (2015–2018)") + ylab("gender_A") +ggtitle("O número total de autorias de uma universidade") +
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat30$gender_A)+200))
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot30, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot31 <- renderPlotly(
      {
         dat31 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options26,
                   Period=="2015–2018", Frac_counting == "0")
            plot31 <- ggplot(dat31, aes(stringr::str_wrap(Field,width = 10), gender_A_MF, fill=Field, label= round(gender_A_MF, digits = 2), 
                       text=paste("gender_A_MF :",gender_A_MF, "<br>", 
                                  "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
               xlab("Área Científica (2015–2018)") + ylab("gender_A_MF")+ ggtitle("O número de autorias de uma universidade pelas quais o gênero é conhecido") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat31$gender_A_MF)+200))
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot31, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot32 <- renderPlotly(
      {
        dat32 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options27,
                   Period=="2015–2018", Frac_counting == "0")
        plot32 <-ggplot(dat32, aes(stringr::str_wrap(Field,width = 10), A_gender_unknown, fill=Field, label= round(A_gender_unknown, digits = 2), 
                       text=paste("A_gender_unknown :",A_gender_unknown, "<br>", 
                                  "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
           xlab("Área Científica (2015–2018)") + ylab("A_gender_unknown") + ggtitle("O número de autorias de uma universidade para as quais o gênero é desconhecido")+
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat32$A_gender_unknown)+30))
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot32, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot46 <- renderPlotly(
      {
         dat46 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options41,
                   Period=="2015–2018", Frac_counting == "0")
         plot46 <-ggplot(dat46, aes(stringr::str_wrap(Field,width = 10), PA_gender_unknown, fill=Field, label= round(PA_gender_unknown, digits = 2), 
                                    text=paste("PA_gender_unknown :",PA_gender_unknown, "<br>", 
                                               "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
            xlab("Área Científica (2015–2018)") + ylab("PA_gender_unknown") + ggtitle("A proporção de autorias de uma universidade para as quais o gênero é desconhecido")+
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat46$PA_gender_unknown)))
         #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot46, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot33 <- renderPlotly(
      {
         dat33 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options28,
                   Period=="2015–2018", Frac_counting == "0") 
          plot33 <- ggplot(dat33,aes(stringr::str_wrap(Field,width = 10), A_M, fill=Field, label= round(A_M, digits = 2), 
                       text=paste("A_M :",A_M, "<br>", 
                                  "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
             xlab("Área Científica (2015–2018)") + ylab("A_M")+ ggtitle("O número de autorias masculinas de uma universidade") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat33$A_M)+150))
            #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot33, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot47 <- renderPlotly(
      {
         dat47 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options42,
                   Period=="2015–2018", Frac_counting == "0") 
         plot47 <- ggplot(dat47,aes(stringr::str_wrap(Field,width = 10), PA_M, fill=Field, label= round(PA_M, digits = 2), 
                                    text=paste("PA_M :",PA_M, "<br>", 
                                               "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
            xlab("Área Científica (2015–2018)") + ylab("PA_M")+ ggtitle("A proporção de autorias masculinas de uma universidade") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat47$PA_M)))
         #theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot47, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot34 <- renderPlotly(
      {
         dat34 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options29,
                   Period=="2015–2018", Frac_counting == "0")
          plot34 <-  ggplot(dat34,aes(stringr::str_wrap(Field,width = 10), A_F, fill=Field, label= round(A_F, digits = 2), 
                       text=paste("A_F :",A_F, "<br>", 
                                  "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
             xlab("Área Científica (2015–2018)") + ylab("A_F") + ggtitle("O número de autorias femininas de uma universidade")+
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat34$A_F)+70))
            #theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
         ggplotly(plot34, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot48 <- renderPlotly(
      {
         dat48 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options43,
                   Period=="2015–2018", Frac_counting == "0")
         plot48 <-  ggplot(dat48,aes(stringr::str_wrap(Field,width = 10), PA_F, fill=Field, label= round(PA_F, digits = 2), 
                                     text=paste("PA_F :",PA_F, "<br>", 
                                                "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
            xlab("Área Científica (2015–2018)") + ylab("PA_F") + ggtitle("A proporção de autorias femininas de uma universidade")+
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat48$PA_F)))
         #theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
         ggplotly(plot48, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot49 <- renderPlotly(
      {
         dat49 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$selectUniversidades01,
                   University == input$selectUniversidades02,
                   Period=="2015–2018", Frac_counting == "0")
         plot49 <-  ggplot(dat49,aes(stringr::str_wrap(Field,width = 10), input$selectIndicadores01, fill=Field, label= round(input$selectIndicadores01, digits = 2), 
                                     text=paste("input$selectIndicadores01 :",input$selectIndicadores01, "<br>", 
                                                "Período:", Period))) +
            geom_col(show.legend = FALSE) + 
            xlab("Área Científica (2015–2018)") + ylab("input$selectIndicadores01") +
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()
         #theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
         ggplotly(plot49, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
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
