#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI for application
shinyUI(fluidPage(
    navbarPage(title="Leiden Ranking",
               tabPanel("Descrição",
                        tabsetPanel(
                            tabPanel("Sobre o Dashboard",
                                     fluidRow(
                                         column(12,
                                                DT::dataTableOutput("table1")))),
                            ##
                            tabPanel("Banco de Dados",
                                     fluidRow(column(3, 
                                                     selectInput("Opções", 
                                                                 strong("Escolha uma opção:"), 
                                                                 choices=c("Opção 1"="age2",
                                                                           "Opção 2"="age3"),
                                                                 selected = "age2")),
                                              column(9,
                                                plotlyOutput("plot1", height = 800)))),
                            
                            ##
                            tabPanel("subMenu3",
                                     fluidRow(column(3, 
                                                     selectInput("options", 
                                                                 strong("Escolha uma opção:"), 
                                                                 choices=c("UFV"="UNIVERSIDADE FEDERAL DE VICOSA",
                                                                           "UFPB"="FEDERAL UNIVERSITY OF PARAIBA",
                                                                           "UFPE"="FEDERAL UNIVERSITY OF PERNAMBUCO"),
                                                                 selected = "age2")),
                                              column(9,
                                                     plotlyOutput("plot11", height = 800))))
                            
                        )# barra de navegacao interna
                    ),# barra de navegacao superior (Dados do Participante)
               
               tabPanel("Menu2",
                        tabsetPanel(
                          ##
                          tabPanel("SubMenu2.1",
                                   fluidRow(column(9,
                                                   leafletOutput("plot2", height = 600)))),
                          tabPanel("SubMenu2.2",
                                   fluidRow(column(9,
                                                   plotlyOutput("plot3", height = 600))))
                          
                          
                        )# barra de navegacao interna
               ),# barra de navegacao superior (Dados da Escola)
               
               tabPanel("Menu3",
                        tabsetPanel(
                          ##
                          tabPanel("SubMenu3.1",
                                   fluidRow(column(9,
                                                   leafletOutput("plot4", height = 600)))),
                          tabPanel("SubMenu3.2",
                                   fluidRow(column(9,
                                                   plotlyOutput("plot5", height = 600))))
                          
                          
                        )# barra de navegacao interna
               ),# barra de navegacao superior (Dados da Escola)
               
               tabPanel("Indicadores de Impacto Científico",
                        tabsetPanel(
                          ##
                          tabPanel("P_Top1",
                                   fluidRow(column(9, 
                                                   selectInput("options1", 
                                                               strong("Escolha uma opção:"), 
                                                               choices=c("FEDERAL UNIVERSITY OF PARAIBA",
                                                                          "FEDERAL UNIVERSITY OF PERNAMBUCO",        
                                                                          "FEDERAL UNIVERSITY OF RIO GRANDE DO NORTE" ,
                                                                          "FEDERAL UNIVERSITY OF RIO GRANDE DO SUL",  
                                                                          "FEDERAL UNIVERSITY OF SANTA MARIA",
                                                                          "FEDERAL UNIVERSITY OF SAO CARLOS",         
                                                                          "FEDERAL UNIVERSITY OF UBERLANDIA",
                                                                          "RIO DE JANEIRO STATE UNIVERSITY",          
                                                                          "STATE UNIVERSITY OF MARINGA",               
                                                                          "UNIVERSIDADE DE BRASILIA",                 
                                                                          "UNIVERSIDADE ESTADUAL PAULISTA",
                                                                          "UNIVERSIDADE FEDERAL DA BAHIA",            
                                                                          "UNIVERSIDADE FEDERAL DE GOIAS",
                                                                          "UNIVERSIDADE FEDERAL DE MINAS GERAIS",     
                                                                          "UNIVERSIDADE FEDERAL DE SANTA CATARINA",
                                                                          "UNIVERSIDADE FEDERAL DE SAO PAULO",   
                                                                          "UNIVERSIDADE FEDERAL DE VICOSA",
                                                                          "UNIVERSIDADE FEDERAL DO CEARA",            
                                                                          "UNIVERSIDADE FEDERAL DO PARANA",
                                                                          "UNIVERSIDADE FEDERAL DO RIO DE JANEIRO",   
                                                                          "UNIVERSIDADE FEDERAL FLUMINENSE",
                                                                          "UNIVERSITY OF CAMPINAS",                   
                                                                          "UNIVERSITY OF SAO PAULO"),
                                                               selected = "age2")),
                                            column(9,
                                                   plotlyOutput("plot6", height = 800))))
                          
                          
                        )# barra de navegacao interna
               )# barra de navegacao superior (Dados da Escola)
               
    )#navbarPage
)#fluidPage
)#shinyUI