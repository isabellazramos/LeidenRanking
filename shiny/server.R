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
       
       plot21 <- dados %>% filter(Country=="BRAZIL") %>% 
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
       ggplotly(plot21, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
       
   })
   
   output$plot6 <- renderPlotly(
       {
           plot6 <-dados %>% filter(Country=="BRAZIL") %>% 
               filter(University==input$options1,
                      Period=="2014–2017", 
                      Frac_counting==input$frac) %>% 
               ggplot(aes(Field, P_top1, fill=Field, label= round(P_top1, digits = 2), 
                          text=paste("P Top 1% :",P_top1, "<br>", 
                                     "Período:", Period))) +
               geom_col(aes(Field, P_top1), show.legend = FALSE) + 
               xlab("Área Ciêntífica (2014-2017)") + ylab("O número de publicações de uma universidade que pertencem ao 1% mais citado com mais frequência.") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+
               theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
           ggplotly(plot6, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
           
       }
   )
   output$plot13 <- renderPlotly(
       {
           plot13 <-dados %>% filter(Country=="BRAZIL") %>% 
               filter(University==input$options7,
                      Period=="2014–2017", 
                      Frac_counting==input$frac7) %>% 
               ggplot(aes(Field, PP_top1, fill=Field, label= round(PP_top1, digits = 2), 
                          text=paste("PP Top 1% :",PP_top1, "<br>", 
                                     "Período:", Period))) +
               geom_col(aes(Field, PP_top1), show.legend = FALSE) + 
               xlab("Área Ciêntífica (2014-2017)") + ylab("A proporção de publicações de uma universidade que pertencem ao 1% mais citado com mais frequência.") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+
               theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
           ggplotly(plot13, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
           
       }
   )
   
   output$plot7 <- renderPlotly(
       {
           plot7 <-dados %>% filter(Country=="BRAZIL") %>% 
               filter(University==input$options2,
                      Period=="2014–2017", 
                      Frac_counting==input$frac2) %>% 
               ggplot(aes(Field, P_top5, fill=Field, label= round(P_top5, digits = 2), 
                          text=paste("P Top 5% :",P_top5, "<br>", 
                                     "Período:", Period))) +
               geom_col(aes(Field, P_top5), show.legend = FALSE) + 
               xlab("Área Ciêntífica (2014-2017)") + ylab("O número de publicações de uma universidade que pertencem ao 5% mais citado com mais frequência.") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+
               theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
           ggplotly(plot7, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
           
       }
   )
   output$plot14 <- renderPlotly(
       {
           plot14 <-dados %>% filter(Country=="BRAZIL") %>% 
               filter(University==input$options8,
                      Period=="2014–2017", 
                      Frac_counting==input$frac8) %>% 
               ggplot(aes(Field, PP_top5, fill=Field, label= round(PP_top5, digits = 2), 
                          text=paste("PP Top 5% :",PP_top5, "<br>", 
                                     "Período:", Period))) +
               geom_col(aes(Field, PP_top5), show.legend = FALSE) + 
               xlab("Área Ciêntífica (2014-2017)") + ylab("A proporção de publicações de uma universidade que pertencem ao 5% mais citado com mais frequência.") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+
               theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
           ggplotly(plot14, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
           
       }
   )
   
   output$plot8 <- renderPlotly(
       {
           plot8 <-dados %>% filter(Country=="BRAZIL") %>% 
               filter(University==input$options3,
                      Period=="2014–2017", 
                      Frac_counting==input$frac3) %>% 
               ggplot(aes(Field, P_top10, fill=Field, label= round(P_top10, digits = 2), 
                          text=paste("P Top 10% :",P_top10, "<br>", 
                                     "Período:", Period))) +
               geom_col(aes(Field, P_top10), show.legend = FALSE) + 
               xlab("Área Ciêntífica (2014-2017)") + ylab("O número de publicações de uma universidade que pertencem ao 10% mais citado com mais frequência.") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+
               theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
           ggplotly(plot8, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
           
       }
   )
   output$plot15 <- renderPlotly(
       {
           plot15 <-dados %>% filter(Country=="BRAZIL") %>% 
               filter(University==input$options9,
                      Period=="2014–2017", 
                      Frac_counting==input$frac9) %>% 
               ggplot(aes(Field, PP_top10, fill=Field, label= round(PP_top10, digits = 2), 
                          text=paste("PP Top 10% :",PP_top10, "<br>", 
                                     "Período:", Period))) +
               geom_col(aes(Field, PP_top10), show.legend = FALSE) + 
               xlab("Área Ciêntífica (2014-2017)") + ylab("A proporção de publicações de uma universidade que pertencem ao 10% mais citado com mais frequência.") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+
               theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
           ggplotly(plot15, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
           
       }
   )
   output$plot9 <- renderPlotly(
       {
           plot9 <-dados %>% filter(Country=="BRAZIL") %>% 
               filter(University==input$options4,
                      Period=="2014–2017", 
                      Frac_counting==input$frac4) %>% 
               ggplot(aes(Field, P_top50, fill=Field, label= round(P_top50, digits = 2), 
                          text=paste("P Top 50% :",P_top50, "<br>", 
                                     "Período:", Period))) +
               geom_col(aes(Field, P_top50), show.legend = FALSE) + 
               xlab("Área Ciêntífica (2014-2017)") + ylab("O número de publicações de uma universidade que pertencem ao 50% mais citado com mais frequência.") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+
               theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
           ggplotly(plot9, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
           
       }
   )
   output$plot16 <- renderPlotly(
       {
           plot16 <-dados %>% filter(Country=="BRAZIL") %>% 
               filter(University==input$options10,
                      Period=="2014–2017", 
                      Frac_counting==input$frac10) %>% 
               ggplot(aes(Field, PP_top50, fill=Field, label= round(PP_top50, digits = 2), 
                          text=paste("PP Top 50% :",PP_top50, "<br>", 
                                     "Período:", Period))) +
               geom_col(aes(Field, PP_top50), show.legend = FALSE) + 
               xlab("Área Ciêntífica (2014-2017)") + ylab("A proporção de publicações de uma universidade que pertencem ao 50% mais citado com mais frequência.") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+
               theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
           ggplotly(plot16, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
           
       }
   )
   output$plot10 <- renderPlotly(
       {
           plot10 <-dados %>% filter(Country=="BRAZIL") %>% 
               filter(University==input$options5,
                      Period=="2014–2017", 
                      Frac_counting==input$frac5) %>% 
               ggplot(aes(Field, TCS, fill=Field, label= round(TCS, digits = 2), 
                          text=paste("TCS :",TCS, "<br>", 
                                     "Período:", Period))) +
               geom_col(aes(Field, TCS), show.legend = FALSE) + 
               xlab("Área Ciêntífica (2014-2017)") + ylab(" O número total de citações das publicações de uma universidade.") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+
               theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
           ggplotly(plot10, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
           
       }
   )
   output$plot17 <- renderPlotly(
       {
           plot17 <-dados %>% filter(Country=="BRAZIL") %>% 
               filter(University==input$options11,
                      Period=="2014–2017", 
                      Frac_counting==input$frac11) %>% 
               ggplot(aes(Field, TNCS, fill=Field, label= round(TNCS, digits = 2), 
                          text=paste("TNCS :",TNCS, "<br>", 
                                     "Período:", Period))) +
               geom_col(aes(Field, TNCS), show.legend = FALSE) + 
               xlab("Área Ciêntífica (2014-2017)") + ylab("O número total de citações das publicações de uma universidade, normalizadas por campo e ano de publicação.") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+
               theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
           ggplotly(plot17, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
           
       }
   )
   output$plot12 <- renderPlotly(
       {
           plot12 <-dados %>% filter(Country=="BRAZIL") %>% 
               filter(University==input$options6,
                      Period=="2014–2017", 
                      Frac_counting==input$frac6) %>% 
               ggplot(aes(Field, MCS, fill=Field, label= round(MCS, digits = 2), 
                          text=paste("MCS :",MCS, "<br>", 
                                     "Período:", Period))) +
               geom_col(aes(Field, MCS), show.legend = FALSE) + 
               xlab("Área Ciêntífica (2014-2017)") + ylab(" O número médio de citações das publicações de uma universidade.") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+
               theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
           ggplotly(plot12, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
           
       }
   )
   output$plot18 <- renderPlotly(
       {
           plot18 <-dados %>% filter(Country=="BRAZIL") %>% 
               filter(University==input$options12,
                      Period=="2014–2017", 
                      Frac_counting==input$frac12) %>% 
               ggplot(aes(Field, MNCS, fill=Field, label= round(MNCS, digits = 2), 
                          text=paste("MNCS :",MNCS, "<br>", 
                                     "Período:", Period))) +
               geom_col(aes(Field, MNCS), show.legend = FALSE) + 
               xlab("Área Ciêntífica (2014-2017)") + ylab(" O número médio de citações das publicações de uma universidade, normalizadas por campo e ano de publicação.") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+
               theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
           ggplotly(plot18, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
           
       }
   )
   output$plot19 <- renderPlotly(
      {
         plot19 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options14,
                   Period=="2014–2017") %>% 
            ggplot(aes(Field, P_collab, fill=Field, label= round(P_collab, digits = 2), 
                       text=paste("P_collab :",P_collab, "<br>", 
                                  "Período:", Period))) +
            geom_col(aes(Field, P_collab), show.legend = FALSE) + 
            xlab("Área Ciêntífica (2014-2017)") + ylab(" O número de publicações de uma universidade que foram coautoria de uma ou mais outras organizações. ") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
         ggplotly(plot19, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot20 <- renderPlotly(
      {
         plot20 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options15,
                   Period=="2014–2017") %>% 
            ggplot(aes(Field, P_int_collab, fill=Field, label= round(P_int_collab, digits = 2), 
                       text=paste("P_int_collab :",P_int_collab, "<br>", 
                                  "Período:", Period))) +
            geom_col(aes(Field, P_int_collab), show.legend = FALSE) + 
            xlab("Área Ciêntífica (2014-2017)") + ylab("O número de publicações de uma universidade que foram coautoria de dois ou mais países.") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
         ggplotly(plot20, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot21 <- renderPlotly(
      {
         plot21 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options16,
                   Period=="2014–2017") %>% 
            ggplot(aes(Field, P_industry_collab, fill=Field, label= round(P_industry_collab, digits = 2), 
                       text=paste("P_industry_collab :",P_industry_collab, "<br>", 
                                  "Período:", Period))) +
            geom_col(aes(Field, P_industry_collab), show.legend = FALSE) + 
            xlab("Área Ciêntífica (2014-2017)") + ylab("O número de publicações de uma universidade que foram coautoria de uma ou mais organizações industriais.") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
         ggplotly(plot21, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot22 <- renderPlotly(
      {
         plot22 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options17,
                   Period=="2014–2017") %>% 
            ggplot(aes(Field, P_short_dist_collab, fill=Field, label= round(P_short_dist_collab, digits = 2), 
                       text=paste("P_short_dist_collab :",P_short_dist_collab, "<br>", 
                                  "Período:", Period))) +
            geom_col(aes(Field, P_short_dist_collab), show.legend = FALSE) + 
            xlab("Área Ciêntífica (2014-2017)") + ylab("O número de publicações de uma universidade com uma distância geográfica de colaboração inferior a 100 km.") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
         ggplotly(plot22, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot23 <- renderPlotly(
      {
         plot23 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options18,
                   Period=="2014–2017") %>% 
            ggplot(aes(Field, P_long_dist_collab, fill=Field, label= round(P_long_dist_collab, digits = 2), 
                       text=paste("P_long_dist_collab :",P_long_dist_collab, "<br>", 
                                  "Período:", Period))) +
            geom_col(aes(Field, P_long_dist_collab), show.legend = FALSE) + 
            xlab("Área Ciêntífica (2014-2017)") + ylab("O número de publicações de uma universidade com uma distância geográfica de colaboração de mais de 5000 km.") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
         ggplotly(plot23, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot24 <- renderPlotly(
      {
         plot24 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options19,
                   Period=="2014–2017") %>% 
            ggplot(aes(Field, P_OA, fill=Field, label= round(P_OA, digits = 2), 
                       text=paste("P_OA :",P_OA, "<br>", 
                                  "Período:", Period))) +
            geom_col(aes(Field, P_OA), show.legend = FALSE) + 
            xlab("Área Ciêntífica (2014-2017)") + ylab("O número de publicações de acesso aberto de uma universidade.") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
         ggplotly(plot24, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot25 <- renderPlotly(
      {
         plot25 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options20,
                   Period=="2014–2017") %>% 
            ggplot(aes(Field, P_gold_OA, fill=Field, label= round(P_gold_OA, digits = 2), 
                       text=paste("P_gold_OA :",P_gold_OA, "<br>", 
                                  "Período:", Period))) +
            geom_col(aes(Field, P_gold_OA), show.legend = FALSE) + 
            xlab("Área Ciêntífica (2014-2017)") + ylab(" O número de ouro em publicações de acesso aberto de uma universidade.") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
         ggplotly(plot25, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot26 <- renderPlotly(
      {
         plot26 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options21,
                   Period=="2014–2017") %>% 
            ggplot(aes(Field, P_hybrid_OA, fill=Field, label= round(P_hybrid_OA, digits = 2), 
                       text=paste("P_hybrid_OA :",P_hybrid_OA, "<br>", 
                                  "Período:", Period))) +
            geom_col(aes(Field, P_hybrid_OA), show.legend = FALSE) + 
            xlab("Área Ciêntífica (2014-2017)") + ylab("O número de publicações híbridas de acesso aberto de uma universidade.") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
         ggplotly(plot26, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot27 <- renderPlotly(
      {
         plot27 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options22,
                   Period=="2014–2017") %>% 
            ggplot(aes(Field, P_bronze_OA, fill=Field, label= round(P_bronze_OA, digits = 2), 
                       text=paste("P_bronze_OA :",P_bronze_OA, "<br>", 
                                  "Período:", Period))) +
            geom_col(aes(Field, P_bronze_OA), show.legend = FALSE) + 
            xlab("Área Ciêntífica (2014-2017)") + ylab("O número de publicações de acesso aberto em bronze de uma universidade.") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
         ggplotly(plot27, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot28 <- renderPlotly(
      {
         plot28 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options23,
                   Period=="2014–2017") %>% 
            ggplot(aes(Field, P_green_OA, fill=Field, label= round(P_green_OA, digits = 2), 
                       text=paste("P_green_OA :",P_green_OA, "<br>", 
                                  "Período:", Period))) +
            geom_col(aes(Field, P_green_OA), show.legend = FALSE) + 
            xlab("Área Ciêntífica (2014-2017)") + ylab("O número de publicações verdes de acesso aberto de uma universidade.") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
         ggplotly(plot28, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
   output$plot29 <- renderPlotly(
      {
         plot29 <-dados %>% filter(Country=="BRAZIL") %>% 
            filter(University==input$options24,
                   Period=="2014–2017") %>% 
            ggplot(aes(Field, P_OA_unknown, fill=Field, label= round(P_OA_unknown, digits = 2), 
                       text=paste("P_OA_unknown :",P_OA_unknown, "<br>", 
                                  "Período:", Period))) +
            geom_col(aes(Field, P_OA_unknown), show.legend = FALSE) + 
            xlab("Área Ciêntífica (2014-2017)") + ylab("O número de publicações de uma universidade para as quais o status de acesso aberto é desconhecido.") + 
            geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
         ggplotly(plot29, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
         
      }
   )
})
