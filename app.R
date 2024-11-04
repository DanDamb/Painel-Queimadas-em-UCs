#
# App Shiny web 
# Para rodar, clique no botão 'Run App' ou 'Reload App' logo acima (no RStudio)
#
# Alternativamente: execute todo o código do script.
#
###
## Principal Referência: Shiny
#    https://shiny.posit.co/
#
## Publicado em :
# https://bs1v8e-daniel-dambroski0c0defreitas.shinyapps.io/EstudoDataQua/
#
## Fonte dos dados originais
# https://www.gov.br/icmbio/pt-br/acesso-a-informacao/dados-abertos
# https://dados.gov.br/dados/conjuntos-dados/incendios-em-unidades-de-conservacao-federais



library(tidyr)
library(dplyr)
library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(rsconnect)

#########
# Conexão ao servidor
#rsconnect::setAccountInfo(name='bs1v8e-daniel-dambroski0c0defreitas',
#                          token='CCFB31F9A9E526C8B8C475AC4BBC4CE9',
#                          secret='<SECRET>')

library(readr)


###########
# Carregando do arquivo
# library(readr)
# q_df <- read_delim("~/Documentos/Curo_R_420/Projetos_R/Dados/queimadas_UCs_federais.csv", 
#                     delim = ";", escape_double = FALSE, col_types = cols(ANO = col_integer()), 
#                     locale = locale(decimal_mark = ",", grouping_mark = "."), 
#                     trim_ws = TRUE)
# View(q_df) 


###
## Carregando os dados do Dropbox
#
#link_url <- "https://www.dropbox.com/scl/fi/w0be02uu6cfgwbkgynnfx/queimadas_UCs_federais.csv?rlkey=uciu3fz6g6wc8tpj15sg0sywa&st=mq8vlubx&dl=1"
#q_df <- read_csv2(link_url)


############

############
## INTERFACE DE USUÁRIO
## Header
dashHeader <- dashboardHeader(title = "Painel Queimadas",
                              dropdownMenu(type = "messages",
                                           messageItem(
                                             from = "CGPRO",
                                             message = "Queda do nº de AIs.",
                                             time = "2014-12-01 12:22"
                                           ),
                                           messageItem(
                                             from = "CGIMP",
                                             message = "Condicionantes descumpridas.",
                                             icon = icon("question"),
                                             time = "13:45"
                                           ),
                                           messageItem(
                                             from = "CGEEZ",
                                             message = "Alertas de emergências",
                                             icon = icon("life-ring"),
                                             time = "2014-12-01"
                                           )
                              ),
                              dropdownMenu(type = "notifications",
                                           notificationItem(
                                             text = "5 denúncias",
                                             icon("users")
                                           ),
                                           notificationItem(
                                             text = "12 condicionantes",
                                             icon("truck"),
                                             status = "success"
                                           ),
                                           notificationItem(
                                             text = "2 focos de calor%",
                                             icon = icon("exclamation-triangle"),
                                             status = "warning"
                                           )
                              ),
                              dropdownMenu(type = "tasks", badgeStatus = "success",
                                           taskItem(value = 90, color = "green",
                                                    "Autorizações pinus"
                                           ),
                                           taskItem(value = 67, color = "aqua",
                                                    "Autorizações água"
                                           ),
                                           taskItem(value = 35, color = "yellow",
                                                    "Autorizações coleta"
                                           ),
                                           taskItem(value = 8, color = "red",
                                                    "Autorizações energia"
                                           )
                              )
)
## Sidebar
dashSidebar <-  dashboardSidebar(
  sidebarMenu(
    menuItem("Painel", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", tabName = "widgets", icon = icon("th"),
             badgeLabel = "new", badgeColor = "green"),
    menuItem("Dados", icon = icon("database"), 
             href = "https://www.kaggle.com/datasets/dandamb/reas-queimadas-em-ucs-federais"),
    menuItem("Código", icon = icon("file-code"), 
             href = "https://github.com/DanDamb/Painel-Queimadas-em-UCs")
  )
)
####
## Body
#
dashBody <-  dashboardBody(
  tabItems(
    # First tab content - PAINEL
    tabItem(tabName = "dashboard",
            fluidRow(
              box(
                title = "Total de área queimada a cada ano.", 
                status = "primary", 
                solidHeader = F,
                collapsible = TRUE,
                plotlyOutput("plot3", height = 250)
              ),
              box(
                title = "Áreas queimadas a cada ano.", 
                status = "warning", 
                solidHeader = F,
                collapsible = TRUE,
                plotlyOutput("plot4", height = 250)
              )
            ),
            fluidRow(
              box(title = "Histograma do número de queimadas por ano.",
                  status = "success",
                  collapsible = TRUE,
                  plotOutput("plot1", 
                             height = 250)),
              
              box(
                title = "Controle do histograma.",
                status = "success",
                collapsible = TRUE,
                sliderInput("slider", "número de classes:", 1, 24, 6, 1)
              )
            ),
            fluidRow(
              box(title = "Histograma do tamamho das áreas queimadas.", 
                  status = "primary", 
                  plotOutput("plot2", height = 250)
                  ),
              box(
                title = "Controles do histograma.", 
                status = "primary",
                collapsible = TRUE,
                sliderInput("slider2", "número de classes:", 1, 50, 20, 1),
                textInput("text", "limite (máx.) do tamanho das áreas (ha):", value = "10000")
              )
            )
    ),
    # Second tab content
    tabItem(tabName = "widgets",
            h2("Área de mini-aplicativos (widgets)."),
            h2("Não há mini-aplicativos disponíveis no momento.")
    )
  )
)

####
## Interface de usuário (UI)
#
ui <- dashboardPage( dashHeader, dashSidebar, dashBody)



histdata <- data.frame(q_df$ANO)
histdata2 <- q_df[q_df$AREA < 100000,] #|> filter(AREA > 10000) |> select(AREA)
data3 <- q_df |>
  group_by(ANO) |>
  summarise(N = n(),
            TOT = sum(AREA),
            MAX = max(AREA),
            MED = mean(AREA),
            MIN = min(AREA))

###
## Funções do lado do servidor
#
server <- function(input, output) {
  # plot1
  output$plot1 <- renderPlot({
    #hist(histdata, breaks = input$slider, main = "histograma do nº de queimadas\n por ano")
    ggplot(data = q_df, aes(x=ANO)) +
      geom_histogram( bins=input$slider, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
      #ggtitle("Bin size = 3") +
      #theme_ipsum() +
      theme(
        plot.title = element_text(size=15)
      )
  })
  # plot2
  output$plot2 <- renderPlot({
    #hist(histdata2, breaks = input$slider2, main = "histograma das áreas queimadas")
    ggplot(data = histdata2[histdata2$AREA < as.numeric(gsub(",", "", input$text)),], aes(x=AREA)) +
      geom_histogram( bins=input$slider2, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
      #ggtitle("Bin size = 3") +
      #theme_ipsum() +
      theme(
        plot.title = element_text(size=15)
      )
  })
  # plot3
  output$plot3 <- renderPlotly({
    plot_ly(
      x = data3$ANO,
      y = data3$TOT,
      name = "Total de área queimada a cada ano",
      type = 'scatter', 
      mode = 'lines+markers', # type = "bar",
      line = list(color = '#69b3a2', width = 2)
    )
  })
  # plot4
  output$plot4 <- renderPlotly({
    fig <- plot_ly(data3, 
                   x = data3$ANO,
                   y = data3$MED,
                   name = "área média",
                   type = 'scatter', 
                   mode = 'lines',
                   line = list(color = 'orange', width = 2)) 
    fig <- fig %>% add_trace(y = ~MIN, 
                             name = 'menor área', 
                             line = list(color = 'yellow', width = 2, dash = 'dot')) 
    fig <- fig %>% add_trace(y = ~MAX, 
                             name = 'maior área', 
                             line = list(color = 'red', width = 2, dash = 'dot')) 
    fig
  })
}

#########
# Chamada da função que gera o painel

shinyApp(ui, server)


## Referencias
# https://rstudio.github.io/shinydashboard/structure.html
# https://pkgs.rstudio.com/flexdashboard/articles/flexdashboard.html - FlexBoard - o basico
# https://pkgs.rstudio.com/flexdashboard/articles/examples.html - FlexBoard - exemplos

# https://tilburgsciencehub.com/topics/visualization/data-visualization/dashboarding/shinydashboard/#:~:text=Overview,specialised%20package%20for%20creating%20dashboards.

# https://rstudio.github.io/shinydashboard/examples.html - exemplos de uso do shinyDashboard
# https://rpubs.com/nickstambaugh/RforBizIntel - um exemplo para testar depois

