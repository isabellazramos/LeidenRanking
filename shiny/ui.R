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
               )# barra de navegacao superior (Dados da Escola)
               
    )#navbarPage
)#fluidPage
)#shinyUI

