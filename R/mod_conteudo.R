mod_conteudo_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        HTML(htitle_with_color("Tamanho dos Atos Normativos", color = darker_blue, htitle = 2)),
        hr(),
        sliderInput(
          ns("ano_normas"), 
          HTML(htitle_with_color("Intervalo de tempo", color = darker_blue, htitle = 4)),
          value = c(ANO_CORTE_MIN, ANO_CORTE_MAX),
          min = ANO_CORTE_MIN, 
          max = ANO_CORTE_MAX,
          step = 1,
          sep = ""
        ),
        br(), br(),
        checkboxGroupInput(
          ns("tipos_normas"),
          label = HTML(paste0(htitle_with_color("Mostrar Tipos de Atos:", color = darker_blue, htitle = 4), "<br>")),
          choices = c("Portarias", "Instruções Normativas", "Resoluções", "Outros atos normativos"),
          selected = c("Portarias", "Instruções Normativas", "Resoluções", "Outros atos normativos")
        ),
        br(), br()
      ),
      mainPanel(
        fluidRow(
          width = 9,
          htmlOutput(ns("texto_principal_mod_conteudo")),
          br(), br(), hr(), br(), br()
        ),
        tabsetPanel(
          tabPanel(
            "Tab1",
            br(),
            #plotlyOutput(ns("UI_mainPanel_graph_evolucao_atos")),
            br(), br()
          ),
          tabPanel(
            "Tab2",
            br(),
            #plotlyOutput(ns("UI_mainPanel_scatter_evolucao_media_palavras")),
            br(), br(), hr(), br(), br(),
            #plotlyOutput(ns("UI_mainPanel_box_evolucao_media_palavras")),
            br(), br()
          ),
          tabPanel(
            "Tab3",
            br(),
            #plotlyOutput(ns("UI_mainPanel_scatter_evolucao_media_palavras")),
            br(), br(), hr(), br(), br(),
            #plotlyOutput(ns("UI_mainPanel_box_evolucao_media_palavras")),
            br(), br()
          ),
          tabPanel(
            "Tab4",
            br(),
            #plotlyOutput(ns("UI_mainPanel_scatter_evolucao_media_palavras")),
            br(), br(), hr(), br(), br(),
            #plotlyOutput(ns("UI_mainPanel_box_evolucao_media_palavras")),
            br(), br()
          )
        )
      )
    ),
    br(),
    fluidRow(style = paste0("background-color: ", deep_blue), column(12, HTML(UI_footer())))
  )
}

mod_conteudo_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$texto_principal_mod_conteudo <- renderUI({HTML(texto_principal_mod_conteudo())})
      
      
      
    }
  )
}