mod_interacao_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        HTML(htitle_with_color("Interação entre os Atos Normativos", color = darker_blue, htitle = 2)),
        hr(),
        sliderInput(
          ns("ano_normas_interacao"), 
          HTML(htitle_with_color("Intervalo de tempo", color = darker_blue, htitle = 4)),
          value = c(ANO_CORTE_MIN, ANO_CORTE_MAX),
          min = ANO_CORTE_MIN, 
          max = ANO_CORTE_MAX,
          step = 1,
          sep = ""
        ),
        br(), br(),
        checkboxGroupInput(
          ns("opcoes_scores"),
          label = HTML(paste0(htitle_with_color("Mostrar Scores Parciais:", color = darker_blue, htitle = 4), "<br>")),
          choices = c("Score de Modificações Passivas", "Score de Modificações Ativas")
        ),
        br(), br()
      ),
      mainPanel(
        fluidRow(
          width = 9,
          htmlOutput(ns("texto_principal_mod_interacao")),
          br(), br(), hr(), br(), br(),
          plotlyOutput(ns("UI_mainPanel_graph_interacoes")),
          br(), br(), hr(), br(), br(),
          htmlOutput(ns("texto_footnote_mod_interacao")),
          br(), br(), br(), br()
        )
      )
    ),
    br(),
    fluidRow(style = paste0("background-color: ", deep_blue), column(12, HTML(UI_footer())))
  )
}

mod_interacao_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$texto_principal_mod_interacao <- renderUI({HTML(texto_principal_mod_interacao())})
      output$texto_explicacao_scores_de_interacao <- renderUI({HTML(texto_footnote_mod_interacao())})
      
      ano_min_interacao <- reactive({input$ano_normas_interacao[1]})
      ano_max_interacao <- reactive({input$ano_normas_interacao[2]})
      
      df_interacoes_periodo <- reactive({
        mod_relacional %>%
          filter(ano >= ano_min_interacao() & ano <= ano_max_interacao())
      })
      
      output$UI_mainPanel_graph_interacoes <- renderPlotly({
        fig <- plot_ly(
          data = df_interacoes_periodo(),
          x = ~ano,
          y = ~interacoes_total,
          type = 'scatter',
          mode = 'lines+markers',
          name = "Score de Interações (Totais)",
          line = list(color = palheta_tamanho$universo, width = 4),
          marker = list(color = palheta_tamanho$universo, size = 8),
          showlegend = TRUE
        )
        if ("Score de Modificações Passivas" %in% input$opcoes_scores) {
          fig <- fig %>%
            add_trace(
              data = df_interacoes_periodo(),
              x = ~ano,
              y = ~mod_passiva,
              name = "Score de Modificações Passivas",
              line = list(color = palheta_tamanho$portarias, width = 2.5, dash = 'dot'),
              marker = list(color = palheta_tamanho$portarias, size = 3)
            )
        }
        if ("Score de Modificações Ativas" %in% input$opcoes_scores) {
          fig <- fig %>%
            add_trace(
              data = df_interacoes_periodo(),
              x = ~ano,
              y = ~mod_ativa,
              name = "Score de Modificações Ativas",
              line = list(color = palheta_tamanho$outros_atos, width = 2.5, dash = 'dot'),
              marker = list(color = palheta_tamanho$outros_atos, size = 3)
            )
        }
        fig <- fig %>% layout(title = "Evolução dos Níveis de Interação entre os Atos Normativos",
                              yaxis = list(title = "Score de Interação (Log)"),
                              xaxis = list (title = "Ano"))
        return(fig)
      })
    }
  )
}