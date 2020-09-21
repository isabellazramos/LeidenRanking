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
    
    output$plot37 <- renderPlot({
       data2 %>% 
          leaflet() %>% 
          addTiles() %>% 
          addMarkers(lng = data2$Longitude, lat = data2$Latitude, popup = data2$University,
                     clusterOptions = markerClusterOptions(maxClusterRadius = 15))
       
    })
    
    output$plot1 <- renderPlotly({
        plot1 <- dados %>% filter(Country=="BRAZIL") %>% 
            filter(University=="UNIVERSIDADE FEDERAL DE VICOSA",
                   Period=="2014–2017", 
                   Frac_counting=="0") %>% 
            ggplot(aes(Field, impact_P, fill=Field, label= round(impact_P, digits = 2), 
                       text=paste("Produção:",impact_P, "<br>", 
                                  "Período:", Period))) +
            geom_col(aes(Field, impact_P), show.legend = FALSE) + 
            xlab("Área Ciêntífica (2014-2017)") + ylab("Número de Publicações com Impacto") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw() + 
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
        ggplotly(plot1, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
        
    })
    
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
                  Period=="2014–2017", 
                  Frac_counting=="1") %>% 
           ggplot(aes(Field, impact_P, fill=Field, label= round(impact_P, digits = 2), 
                      text=paste("Produção:",impact_P, "<br>", 
                                 "Período:", Period))) +
           geom_col(aes(Field, impact_P), show.legend = FALSE) + 
           xlab("Área Ciêntífica (2014-2017)") + ylab("Número de Publicações com Impacto") + 
           geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw() +
           theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
       ggplotly(plot2, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
       
   }) 
   
   output$plot11 <- renderPlotly({
       
       plot11 <- dados %>% filter(Country=="BRAZIL") %>% 
           filter(University==input$options,
                  Period=="2014–2017", 
                  Frac_counting=="1") %>% 
           ggplot(aes(Field, impact_P, fill=Field, label= round(impact_P, digits = 2), 
                      text=paste("Produção:",impact_P, "<br>", 
                                 "Período:", Period))) +
           geom_col(aes(Field, impact_P), show.legend = FALSE) + 
           xlab("Área Ciêntífica (2014-2017)") + ylab("Número de Publicações com Impacto") + 
           geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw() +
           theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
       ggplotly(plot11, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
       
   })
   
   output$plot6 <- renderPlotly(
       {
           dat6 <-dados %>% filter(Country=="BRAZIL") %>% 
               filter(University==input$options1,
                      Period=="2014–2017", 
                      Frac_counting==input$frac) 
           plot6 <- ggplot(dat6, aes(Field, P_top1, fill=Field, label= round(P_top1, digits = 2), 
                          text=paste("P Top 1% :",P_top1, "<br>", 
                                     "Período:", Period))) +
               geom_col(aes(Field, P_top1), show.legend = FALSE) + 
               xlab("Área Ciêntífica (2014-2017)") + ylab("P_top1") + ggtitle("O número de publicações de uma universidade que pertencem ao 1% mais citado")+ 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat6$P_top1)+20))+
               theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
           ggplotly(plot6, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
           
       }
   )
   output$plot13 <- renderPlotly(
       {
          dat13 <-dados %>% filter(Country=="BRAZIL") %>% 
               filter(University==input$options7,
                      Period=="2014–2017", 
                      Frac_counting==input$frac7)
          plot13 <- ggplot(dat13, aes(Field, PP_top1, fill=Field, label= round(PP_top1, digits = 2), 
                          text=paste("PP Top 1% :",PP_top1, "<br>", 
                                     "Período:", Period))) +
               geom_col(aes(Field, PP_top1), show.legend = FALSE) + 
               xlab("Área Ciêntífica (2014-2017)") + ylab("PP_top1")+ ggtitle("A proporção de publicações de uma universidade que pertencem ao 1% mais citado") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat13$PP_top1)+20))+
               theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
           ggplotly(plot13, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
           
       }
   )
   
   output$plot7 <- renderPlotly(
       {
           dat7 <-dados %>% filter(Country=="BRAZIL") %>% 
               filter(University==input$options2,
                      Period=="2014–2017", 
                      Frac_counting==input$frac2)
          plot7 <- ggplot(dat7,aes(Field, P_top5, fill=Field, label= round(P_top5, digits = 2), 
                          text=paste("P Top 5% :",P_top5, "<br>", 
                                     "Período:", Period))) +
               geom_col(aes(Field, P_top5), show.legend = FALSE) + 
               xlab("Área Ciêntífica (2014-2017)") + ylab("P_top5")+ ggtitle("O número de publicações de uma universidade que pertencem ao 5% mais citado") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat7$P_top5)+20))+
               theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
           ggplotly(plot7, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
           
       }
   )
   output$plot14 <- renderPlotly(
       {
           dat14 <-dados %>% filter(Country=="BRAZIL") %>% 
               filter(University==input$options8,
                      Period=="2014–2017", 
                      Frac_counting==input$frac8) 
            plot14 <- ggplot(dat14, aes(Field, PP_top5, fill=Field, label= round(PP_top5, digits = 2), 
                          text=paste("PP Top 5% :",PP_top5, "<br>", 
                                     "Período:", Period))) +
               geom_col(aes(Field, PP_top5), show.legend = FALSE) + 
               xlab("Área Ciêntífica (2014-2017)") + ylab("PP_top5")+ ggtitle("A proporção de publicações de uma universidade que pertencem ao 5% mais citado") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat14$PP_top5)+20))+
               theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
           ggplotly(plot14, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
           
       }
   )
   
   output$plot8 <- renderPlotly(
       {
           dat8 <-dados %>% filter(Country=="BRAZIL") %>% 
               filter(University==input$options3,
                      Period=="2014–2017", 
                      Frac_counting==input$frac3)
           plot8 <- ggplot(dat8,aes(Field, P_top10, fill=Field, label= round(P_top10, digits = 2), 
                          text=paste("P Top 10% :",P_top10, "<br>", 
                                     "Período:", Period))) +
               geom_col(aes(Field, P_top10), show.legend = FALSE) + 
               xlab("Área Ciêntífica (2014-2017)") + ylab("P_top10")+ ggtitle("O número de publicações de uma universidade que pertencem ao 10% mais citado") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat8$P_top10)+20))+
               theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
           ggplotly(plot8, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
           
       }
   )
   output$plot15 <- renderPlotly(
       {
           dat15 <-dados %>% filter(Country=="BRAZIL") %>% 
               filter(University==input$options9,
                      Period=="2014–2017", 
                      Frac_counting==input$frac9)
          plot15 <- ggplot(dat15,aes(Field, PP_top10, fill=Field, label= round(PP_top10, digits = 2), 
                          text=paste("PP Top 10% :",PP_top10, "<br>", 
                                     "Período:", Period))) +
               geom_col(aes(Field, PP_top10), show.legend = FALSE) + 
               xlab("Área Ciêntífica (2014-2017)") + ylab("PP_top10")+ ggtitle("A proporção de publicações de uma universidade que pertencem ao 10% mais citado") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat15$PP_top10)+20))+
               theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
           ggplotly(plot15, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
           
       }
   )
   output$plot9 <- renderPlotly(
       {
           dat9 <-dados %>% filter(Country=="BRAZIL") %>% 
               filter(University==input$options4,
                      Period=="2014–2017", 
                      Frac_counting==input$frac4) 
           plot9 <- ggplot(dat9, aes(Field, P_top50, fill=Field, label= round(P_top50, digits = 2), 
                          text=paste("P Top 50% :",P_top50, "<br>", 
                                     "Período:", Period))) +
               geom_col(aes(Field, P_top50), show.legend = FALSE) + 
               xlab("Área Ciêntífica (2014-2017)") + ylab("P_top50")+ ggtitle("O número de publicações de uma universidade que pertencem ao 50% mais citado") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat9$P_top50)+20))+
               theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
           ggplotly(plot9, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
           
       }
   )
   output$plot16 <- renderPlotly(
       {
           dat16 <-dados %>% filter(Country=="BRAZIL") %>% 
               filter(University==input$options10,
                      Period=="2014–2017", 
                      Frac_counting==input$frac10)
          plot16 <- ggplot(dat16,aes(Field, PP_top50, fill=Field, label= round(PP_top50, digits = 2), 
                          text=paste("PP Top 50% :",PP_top50, "<br>", 
                                     "Período:", Period))) +
               geom_col(aes(Field, PP_top50), show.legend = FALSE) + 
               xlab("Área Ciêntífica (2014-2017)") + ylab("PP_top50") + ggtitle("A proporção de publicações de uma universidade que pertencem ao 50% mais citado")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat16$PP_top50)+20))+
               theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
           ggplotly(plot16, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
           
       }
   )
   output$plot10 <- renderPlotly(
       {
           dat10 <-dados %>% filter(Country=="BRAZIL") %>% 
               filter(University==input$options5,
                      Period=="2014–2017", 
                      Frac_counting==input$frac5)
          plot10 <- ggplot(dat10,aes(Field, TCS, fill=Field, label= round(TCS, digits = 2), 
                          text=paste("TCS :",TCS, "<br>", 
                                     "Período:", Period))) +
               geom_col(aes(Field, TCS), show.legend = FALSE) + 
               xlab("Área Ciêntífica (2014-2017)") + ylab("TCS") + ggtitle("O número total de citações das publicações de uma universidade")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat10$TCS)+20))+
               theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
           ggplotly(plot10, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
           
       }
   )
   output$plot17 <- renderPlotly(
       {
           dat17 <-dados %>% filter(Country=="BRAZIL") %>% 
               filter(University==input$options11,
                      Period=="2014–2017", 
                      Frac_counting==input$frac11) 
           plot17 <- ggplot(dat17,aes(Field, TNCS, fill=Field, label= round(TNCS, digits = 2), 
                          text=paste("TNCS :",TNCS, "<br>", 
                                     "Período:", Period))) +
               geom_col(aes(Field, TNCS), show.legend = FALSE) + 
               xlab("Área Ciêntífica (2014-2017)") + ylab("TNCS") + ggtitle("O número total de citações das publicações de uma universidade")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat17$TNCS)+20))+
               theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
           ggplotly(plot17, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
           
       }
   )
   output$plot12 <- renderPlotly(
       {
           dat12 <-dados %>% filter(Country=="BRAZIL") %>% 
               filter(University==input$options6,
                      Period=="2014–2017", 
                      Frac_counting==input$frac6) 
           plot12 <- ggplot(dat12,aes(Field, MCS, fill=Field, label= round(MCS, digits = 2), 
                          text=paste("MCS :",MCS, "<br>", 
                                     "Período:", Period))) +
               geom_col(aes(Field, MCS), show.legend = FALSE) + 
               xlab("Área Ciêntífica (2014-2017)") + ylab("MCS") + ggtitle("O número médio de citações das publicações de uma universidade")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat12$MCS)+20))+
               theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
           ggplotly(plot12, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
           
       }
   )
   output$plot18 <- renderPlotly(
       {
           dat18 <-dados %>% filter(Country=="BRAZIL") %>% 
               filter(University==input$options12,
                      Period=="2014–2017", 
                      Frac_counting==input$frac12)
           plot18 <-ggplot(dat18,aes(Field, MNCS, fill=Field, label= round(MNCS, digits = 2), 
                          text=paste("MNCS :",MNCS, "<br>", 
                                     "Período:", Period))) +
               geom_col(aes(Field, MNCS), show.legend = FALSE) + 
               xlab("Área Ciêntífica (2014-2017)") + ylab("MNCS") + ggtitle("O número médio de citações das publicações de uma universidade, normalizadas por campo e ano de publicação")+
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat18$MNCS)+20))+
               theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
           ggplotly(plot18, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
           
       }
   )
   output$plot19 <- renderPlotly(
      {
         dat19 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options14,
                   Period=="2014–2017", Frac_counting == "0") 
         plot19 <- ggplot(dat19,aes(Field, P_collab, fill=Field, label= round(P_collab, digits = 2), 
                       text=paste("P_collab :",P_collab, "<br>", 
                                  "Período:", Period))) +
            geom_col(aes(Field, P_collab), show.legend = FALSE) + 
            xlab("Área Ciêntífica (2014-2017)") + ylab("P_collab")+ggtitle("O número de publicações de uma universidade que foram coautoria de uma ou mais outras organizações") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat19$P_collab)+20))+
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot19, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot35 <- renderPlotly(
      {
         dat35 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options30,
                   Period=="2014–2017", Frac_counting == "0") 
         plot35 <- ggplot(dat35,aes(Field, PP_collab, fill=Field, label= round(PP_collab, digits = 2), 
                                    text=paste("PP_collab :",PP_collab, "<br>", 
                                               "Período:", Period))) +
            geom_col(aes(Field, PP_collab), show.legend = FALSE) + 
            xlab("Área Ciêntífica (2014-2017)") + ylab("PP_collab")+ggtitle("A proporção de publicações de uma universidade que foram coautoria de uma ou mais outras organizações") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat35$PP_collab)+20))+
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot35, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot20 <- renderPlotly(
      {
         dat20 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options15,
                   Period=="2014–2017", Frac_counting == "0")
            plot20 <- ggplot(dat20, aes(Field, P_int_collab, fill=Field, label= round(P_int_collab, digits = 2), 
                       text=paste("P_int_collab :",P_int_collab, "<br>", 
                                  "Período:", Period))) +
            geom_col(aes(Field, P_int_collab), show.legend = FALSE) + 
            xlab("Área Ciêntífica (2014-2017)") + ylab("P_int_collab") + ggtitle("O número de publicações de uma universidade que foram coautoria de dois ou mais países")+
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat20$P_int_collab)+20))+
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot20, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot36 <- renderPlotly(
      {
         dat36 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options31,
                   Period=="2014–2017", Frac_counting == "0")
         plot36 <- ggplot(dat36, aes(Field, PP_int_collab, fill=Field, label= round(PP_int_collab, digits = 2), 
                                     text=paste("PP_int_collab :",PP_int_collab, "<br>", 
                                                "Período:", Period))) +
            geom_col(aes(Field, PP_int_collab), show.legend = FALSE) + 
            xlab("Área Ciêntífica (2014-2017)") + ylab("PP_int_collab") + ggtitle("A proporção de publicações de uma universidade que foram coautoria de dois ou mais países")+
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat36$PP_int_collab)+20))+
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot36, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot21 <- renderPlotly(
      {
         dat21 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options16,
                   Period=="2014–2017", Frac_counting == "0")
         plot21 <- ggplot(dat21,aes(Field, P_industry_collab, fill=Field, label= round(P_industry_collab, digits = 2), 
                       text=paste("P_industry_collab :",P_industry_collab, "<br>", 
                                  "Período:", Period))) +
            geom_col(aes(Field, P_industry_collab), show.legend = FALSE) + 
            xlab("Área Ciêntífica (2014-2017)") + ylab("P_industry_collab")+ ggtitle("O número de publicações de uma universidade que foram coautoria de uma ou mais organizações industriais") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat21$P_industry_collab)+20))+
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot21, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot22 <- renderPlotly(
      {
        dat22 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options17,
                   Period=="2014–2017", Frac_counting == "0")
            plot22 <- ggplot(dat22, aes(Field, P_short_dist_collab, fill=Field, label= round(P_short_dist_collab, digits = 2), 
                       text=paste("P_short_dist_collab :",P_short_dist_collab, "<br>", 
                                  "Período:", Period))) +
            geom_col(aes(Field, P_short_dist_collab), show.legend = FALSE) + 
            xlab("Área Ciêntífica (2014-2017)") + ylab("P_short_dist_collab")+ ggtitle("O número de publicações de uma universidade com uma distância geográfica de colaboração inferior a 100 km.") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat22$P_short_dist_collab)+20)) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot22, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot23 <- renderPlotly(
      {
         dat23 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options18,
                   Period=="2014–2017", Frac_counting == "0")
            plot23 <- ggplot(dat23,aes(Field, P_long_dist_collab, fill=Field, label= round(P_long_dist_collab, digits = 2), 
                       text=paste("P_long_dist_collab :",P_long_dist_collab, "<br>", 
                                  "Período:", Period))) +
            geom_col(aes(Field, P_long_dist_collab), show.legend = FALSE) + 
            xlab("Área Ciêntífica (2014-2017)") + ylab("P_long_dist_collab") + ggtitle("O número de publicações de uma universidade com uma distância geográfica de colaboração de mais de 5000 km.")+
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat23$P_long_dist_collab)+20))+
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot23, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot24 <- renderPlotly(
      {
         dat24 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options19,
                   Period=="2014–2017", Frac_counting ==  "0") 
           plot24 <- ggplot(dat24,aes(Field, P_OA, fill=Field, label= round(P_OA, digits = 2), 
                       text=paste("P_OA :",P_OA, "<br>", 
                                  "Período:", Period))) +
            geom_col(aes(Field, P_OA), show.legend = FALSE) + 
            xlab("Área Ciêntífica (2014-2017)") + ylab("P_OA") + ggtitle("O número de publicações de acesso aberto de uma universidade.")+
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat24$P_OA)+20))+
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot24, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot25 <- renderPlotly(
      {
         dat25 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options20,
                   Period=="2014–2017", Frac_counting ==  "0")
          plot25 <-  ggplot(dat25,aes(Field, P_gold_OA, fill=Field, label= round(P_gold_OA, digits = 2), 
                       text=paste("P_gold_OA :",P_gold_OA, "<br>", 
                                  "Período:", Period))) +
            geom_col(aes(Field, P_gold_OA), show.legend = FALSE) + 
            xlab("Área Ciêntífica (2014-2017)") + ylab("P_gold_OA")+ ggtitle("O número de ouro em publicações de acesso aberto de uma universidade") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat25$P_gold_OA)+20))+
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot25, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot26 <- renderPlotly(
      {
         dat26 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options21,
                   Period=="2014–2017", Frac_counting ==  "0")
            plot26 <- ggplot(dat26, aes(Field, P_hybrid_OA, fill=Field, label= round(P_hybrid_OA, digits = 2), 
                       text=paste("P_hybrid_OA :",P_hybrid_OA, "<br>", 
                                  "Período:", Period))) +
            geom_col(aes(Field, P_hybrid_OA), show.legend = FALSE) + 
            xlab("Área Ciêntífica (2014-2017)") + ylab("P_hybrid_OA") + ggtitle("O número de publicações híbridas de acesso aberto de uma universidade") +
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat26$P_hybrid_OA)+20))+
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot26, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot27 <- renderPlotly(
      {
         dat27 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options22,
                   Period=="2014–2017", Frac_counting == "0") 
            plot27 <- ggplot(dat27,aes(Field, P_bronze_OA, fill=Field, label= round(P_bronze_OA, digits = 2), 
                       text=paste("P_bronze_OA :",P_bronze_OA, "<br>", 
                                  "Período:", Period))) +
            geom_col(aes(Field, P_bronze_OA), show.legend = FALSE) + 
            xlab("Área Ciêntífica (2014-2017)") + ylab("P_bronze_OA") + ggtitle("O número de publicações de acesso aberto em bronze de uma universidade")+
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat27$P_bronze_OA)+20))+
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot27, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot28 <- renderPlotly(
      {
         dat28 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options23,
                   Period=="2014–2017", Frac_counting == "0")
          plot28 <-  ggplot(dat28,aes(Field, P_green_OA, fill=Field, label= round(P_green_OA, digits = 2), 
                       text=paste("P_green_OA :",P_green_OA, "<br>", 
                                  "Período:", Period))) +
            geom_col(aes(Field, P_green_OA), show.legend = FALSE) + 
            xlab("Área Ciêntífica (2014-2017)") + ylab("P_green_OA") + ggtitle("O número de publicações verdes de acesso aberto de uma universidade")+
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat28$P_green_OA)+20))+
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot28, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot29 <- renderPlotly(
      {
         dat29 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options24,
                   Period=="2014–2017", Frac_counting == "0")
          plot29 <-  ggplot(dat29,aes(Field, P_OA_unknown, fill=Field, label= round(P_OA_unknown, digits = 2), 
                       text=paste("P_OA_unknown :",P_OA_unknown, "<br>", 
                                  "Período:", Period))) +
            geom_col(aes(Field, P_OA_unknown), show.legend = FALSE) + 
            xlab("Área Ciêntífica (2014-2017)") + ylab("P_OA_unknown")+ ggtitle("O número de publicações de uma universidade para as quais o status de acesso aberto é desconhecido") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) +theme_bw()+ylim(c(0,max(dat29$P_OA_unknown)+20))+
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot29, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot30 <- renderPlotly(
      {
         dat30 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options25,
                   Period=="2014–2017", Frac_counting == "0")
           plot30 <- ggplot(dat30,aes(Field, gender_A, fill=Field, label= round(gender_A, digits = 2), 
                       text=paste("gender_A :",gender_A, "<br>", 
                                  "Período:", Period))) +
            geom_col(aes(Field, gender_A), show.legend = FALSE) + 
            xlab("Área Ciêntífica (2014-2017)") + ylab("gender_A") +ggtitle("O número total de autorias de uma universidade") +
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat30$gender_A)+200))+
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot30, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot31 <- renderPlotly(
      {
         dat31 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options26,
                   Period=="2014–2017", Frac_counting == "0")
            plot31 <- ggplot(dat31, aes(Field, gender_A_MF, fill=Field, label= round(gender_A_MF, digits = 2), 
                       text=paste("gender_A_MF :",gender_A_MF, "<br>", 
                                  "Período:", Period))) +
            geom_col(aes(Field, gender_A_MF), show.legend = FALSE) + 
            xlab("Área Ciêntífica (2014-2017)") + ylab("gender_A_MF")+ ggtitle("O número de autorias de uma universidade pelas quais o gênero é conhecido") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ ylim(c(0,max(dat31$gender_A_MF)+200))+
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot31, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot32 <- renderPlotly(
      {
        dat32 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options27,
                   Period=="2014–2017", Frac_counting == "0")
        plot32 <-ggplot(dat32, aes(Field, A_gender_unknown, fill=Field, label= round(A_gender_unknown, digits = 2), 
                       text=paste("A_gender_unknown :",A_gender_unknown, "<br>", 
                                  "Período:", Period))) +
            geom_col(aes(Field, A_gender_unknown), show.legend = FALSE) + 
            xlab("Área Ciêntífica (2014-2017)") + ylab("A_gender_unknown") + ggtitle("O número de autorias de uma universidade para as quais o gênero é desconhecido")+
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat32$A_gender_unknown)+30))+
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot32, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot33 <- renderPlotly(
      {
         dat33 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options28,
                   Period=="2014–2017", Frac_counting == "0") 
          plot33 <- ggplot(dat33,aes(Field, A_M, fill=Field, label= round(A_M, digits = 2), 
                       text=paste("A_M :",A_M, "<br>", 
                                  "Período:", Period))) +
            geom_col(aes(Field, A_M), show.legend = FALSE) + 
            xlab("Área Ciêntífica (2014-2017)") + ylab("A_M")+ ggtitle("O número de autorias masculinas de uma universidade") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat33$A_M)+150))+
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
         ggplotly(plot33, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot34 <- renderPlotly(
      {
         dat34 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options29,
                   Period=="2014–2017", Frac_counting == "0")
          plot34 <-  ggplot(dat34,aes(Field, A_F, fill=Field, label= round(A_F, digits = 2), 
                       text=paste("A_F :",A_F, "<br>", 
                                  "Período:", Period))) +
            geom_col(aes(Field, A_F), show.legend = FALSE) + 
            xlab("Área Ciêntífica (2014-2017)") + ylab("A_F") + ggtitle("O número de autorias femininas de uma universidade")+
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+ylim(c(0,max(dat34$A_F)+70))+
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
         ggplotly(plot34, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   
   output$tableBrazil <- renderTable({ head( brazil, n = -1 )},  
                             bordered = TRUE)  
   
})
