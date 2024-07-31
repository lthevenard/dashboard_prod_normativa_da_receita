mod_conteudo_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        HTML(htitle_with_color("Conteúdo dos Atos Normativos", color = darker_blue, htitle = 2)),
        hr(),
        sliderInput(
          ns("ano_normas_conteudo"), 
          HTML(htitle_with_color("Intervalo de tempo", color = darker_blue, htitle = 4)),
          value = c(ANO_CORTE_MIN, ANO_CORTE_MAX),
          min = ANO_CORTE_MIN, 
          max = ANO_CORTE_MAX,
          step = 1,
          sep = ""
        ),
        br(), br(),
        selectInput(
          ns("tema_conteudo"),
          label = HTML(paste0(htitle_with_color("Tema de análise:", color = darker_blue, htitle = 4), "<br>")),
          choices = c("Deveres / obrigações", "Multas e penalidades", "Alteração de prazos", "Comparação dos temas"),
          selected = "Comparação dos temas"
        ),
        br(), br()
      ),
      mainPanel(
        htmlOutput(ns("texto_principal_mod_conteudo")),
        br(),
        plotlyOutput(ns("UI_mainPanel_graph_conteudo")),
        br(),
        htmlOutput(ns("texto_footnote_mod_conteudo")),
        br()
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
      output$texto_footnote_mod_conteudo <- renderUI({HTML(texto_footnote_mod_conteudo())})
      
      ano_min_conteudo <- reactive({input$ano_normas_conteudo[1]})
      ano_max_conteudo <- reactive({input$ano_normas_conteudo[2]})
      
      join_table <- reactive({
        normas_select %>%
          filter(ano >= ano_min_conteudo() & ano <= ano_max_conteudo())
      })
      
      ca_table_period <- reactive({
        ca_table %>%
          filter(ano >= ano_min_conteudo() & ano <= ano_max_conteudo())
      })
      
      output$UI_mainPanel_graph_conteudo <- renderPlotly({
        
        if (input$tema_conteudo == "Comparação dos temas") {
          p <- ca_table_period() %>% 
            plot_comparacao_temas()
        } else if (input$tema_conteudo == "Deveres / obrigações") {
          p <- plot_content_analysis(
            content_plots_info$dever$tipo,
            content_plots_info$dever$titulo,
            content_plots_info$dever$cor,
            content_table,
            join_table()
          )
        } else if (input$tema_conteudo == "Multas e penalidades") {
          p <- plot_content_analysis(
            content_plots_info$pena$tipo,
            content_plots_info$pena$titulo,
            content_plots_info$pena$cor,
            content_table,
            join_table()
          )
        } else if (input$tema_conteudo == "Alteração de prazos") {
          p <- plot_content_analysis(
              content_plots_info$prazo$tipo,
              content_plots_info$prazo$titulo,
              content_plots_info$prazo$cor,
              content_table,
              join_table()
            )
        }
        return(ggplotly(p))
      })
      
    }
  )
}