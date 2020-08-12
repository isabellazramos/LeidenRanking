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
                      Frac_counting=="1") %>% 
               ggplot(aes(Field, P_top1, fill=Field, label= round(P_top1, digits = 2), 
                          text=paste("P Top 1% :",P_top1, "<br>", 
                                     "Período:", Period))) +
               geom_col(aes(Field, P_top1), show.legend = FALSE) + 
               xlab("Área Ciêntífica (2014-2017)") + ylab("O número e a proporção de publicações de uma universidade que pertencem ao 1% mais citado com mais frequência.") + 
               geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + theme_bw()+
               theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
           ggplotly(plot6, tooltip = "text") %>% layout(showlegend = FALSE) %>% style(textposition = "top")
           
       }
   )
    
})
