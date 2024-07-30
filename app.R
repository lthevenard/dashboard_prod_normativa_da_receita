library(shiny)
library(tidyverse)
library(bslib)
library(plotly)

app_theme <- bs_theme(
  bootswatch = "journal", 
  primary = deep_blue, 
  secondary = light_blue, 
  info = purple,
  success = pale_green
)

ui <- navbarPage(
  theme = app_theme,
  titlePanel(HTML(bold_with_color("Produção Normativa\nda Receita Federal&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;", color = deep_blue)), 
             windowTitle = paste(emo::ji("page"), "Normas Receita")),
  tabPanel(
    HTML(bold_with_color("Apresentação da Pesquisa", color = deep_blue)),
    br(), br(),
    mod_apresentacao_UI("apresentacao_da_pesquisa")
  ),
  navbarMenu(
    HTML(bold_with_color("Resultados", color = deep_blue)),
    tabPanel(
      "Tamanho dos atos normativos",
      br(), br(),
      mod_tamanho_UI("normas_receita_tamanho")
    ),
    tabPanel(
      "Interações entre os atos normativos",
      br(), br(),
      mod_interacao_UI("normas_receita_interacao")
    ),
    tabPanel(
      "Conteúdo dos atos normativos",
      br(), br(),
      mod_conteudo_UI("normas_receita_conteudo")
    )
  )
)

server <- function(input, output, session) {
  mod_apresentacao_server("apresentacao_da_pesquisa")
  mod_tamanho_server("normas_receita_tamanho")
  mod_interacao_server("normas_receita_interacao")
  mod_conteudo_server("normas_receita_conteudo")
}

shinyApp(ui, server)
