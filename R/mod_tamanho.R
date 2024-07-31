mod_tamanho_UI <- function(id) {
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
          htmlOutput(ns("texto_principal_mod_tamanho"))
        ),
        tabsetPanel(
          tabPanel(
            "Número de Atos Normativos",
            br(), br(),
            plotlyOutput(ns("UI_mainPanel_graph_evolucao_atos")),
            br()
          ),
          tabPanel(
            "Extensão Média dos Atos Normativos",
            br(), br(),
            plotlyOutput(ns("UI_mainPanel_scatter_evolucao_media_palavras")),
            br(),
            plotlyOutput(ns("UI_mainPanel_box_evolucao_media_palavras")),
            br()
          )
        )
      )
    ),
    br(), br(),
    fluidRow(style = paste0("background-color: ", deep_blue), column(12, HTML(UI_footer())))
  )
}

mod_tamanho_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$texto_principal_mod_tamanho <- renderUI({HTML(texto_principal_mod_tamanho())})
      
      ano_min <- reactive({input$ano_normas[1]})
      ano_max <- reactive({input$ano_normas[2]})
      
      df_tamanho <- reactive({
        normas_select %>%
          filter(ano >= ano_min() & ano <= ano_max())
      })
      
      df_tipos <- reactive({
        df_tamanho() %>%
          mutate(tipo_resumido = case_when(
            str_detect(str_to_lower(tipo), "portaria") ~ "Portarias",
            str_detect(str_to_lower(tipo), "instrução.+normativa") ~ "Instruções Normativas",
            str_detect(str_to_lower(tipo), "resolução") ~ "Resoluções",
            TRUE ~ "Outros atos normativos"
          )) %>%
          count(ano, tipo_resumido)
      })
      
      q3_ref <- reactive({
        df_tamanho() %>% 
          group_by(ano) %>% 
          summarise(q3 = quantile(palavras, na.rm = TRUE)[4]) %>% 
          .$q3 %>% 
          max()
      })
      
      df_medias_palavras <- reactive({
        df_tamanho() %>% 
          mutate(ano = as.character(ano)) %>%
          group_by(ano) %>% 
          summarise(media_palavras = mean(palavras, na.rm = TRUE))
      })
      
      df_tipos_medias_palavras <- reactive({
        df_tamanho() %>%
          mutate(tipo_resumido = case_when(
            str_detect(str_to_lower(tipo), "portaria") ~ "Portarias",
            str_detect(str_to_lower(tipo), "instrução.+normativa") ~ "Instruções Normativas",
            str_detect(str_to_lower(tipo), "resolução") ~ "Resoluções",
            TRUE ~ "Outros atos normativos"
          )) %>%
          group_by(ano, tipo_resumido) %>% 
          summarise(media_palavras = mean(palavras, na.rm = TRUE))
      })
      
      output$UI_mainPanel_graph_evolucao_atos <- renderPlotly({
        fig <- plot_ly(
          data = count(df_tamanho(), ano),
          x = ~ano,
          y = ~n,
          type = 'scatter',
          mode = 'lines+markers',
          name = "Total de atos",
          line = list(color = palheta_tamanho$universo, width = 4),
          marker = list(color = palheta_tamanho$universo, size = 8)
        )
        if ("Portarias" %in% input$tipos_normas) {
          fig <- fig %>%
            add_trace(
              data = filter(df_tipos(), tipo_resumido == "Portarias"),
              x = ~ano,
              y = ~n,
              name = "Portarias",
              line = list(color = palheta_tamanho$portarias, width = 2.5, dash = 'dot'),
              marker = list(color = palheta_tamanho$portarias, size = 3)
            )
        }
        if ("Instruções Normativas" %in% input$tipos_normas) {
          fig <- fig %>%
            add_trace(
              data = filter(df_tipos(), tipo_resumido == "Instruções Normativas"),
              x = ~ano,
              y = ~n,
              name = "Instruções Normativas",
              line = list(color = palheta_tamanho$instrucoes, width = 2.5, dash = 'dot'),
              marker = list(color = palheta_tamanho$instrucoes, size = 3)
            )
        }
        if ("Resoluções" %in% input$tipos_normas) {
          fig <- fig %>%
            add_trace(
              data = filter(df_tipos(), tipo_resumido == "Resoluções"),
              x = ~ano,
              y = ~n,
              name = "Resoluções",
              line = list(color = palheta_tamanho$resolucoes, width = 2.5, dash = 'dot'),
              marker = list(color = palheta_tamanho$resolucoes, size = 3)
            )
        }
        if ("Outros atos normativos" %in% input$tipos_normas) {
          fig <- fig %>%
            add_trace(
              data = filter(df_tipos(), tipo_resumido == "Outros atos normativos"),
              x = ~ano,
              y = ~n,
              name = "Outros atos normativos",
              line = list(color = palheta_tamanho$outros_atos, width = 2.5, dash = 'dot'),
              marker = list(color = palheta_tamanho$outros_atos, size = 3)
            )
        }
        fig <- fig %>% layout(title = "Evolução do número de atos normativos editados pela Receita Federal",
                              yaxis = list(title = "Número de Atos Normativos Editados"),
                              xaxis = list (title = "Ano"))
        return(fig)
      })
      
      output$UI_mainPanel_box_evolucao_media_palavras <- renderPlotly({
        fig <- plot_ly(
          data = df_tamanho() %>% mutate(ano = as.character(ano)), 
          x = ~ano,
          y = ~palavras,
          type = "box",
          marker = list(color = palheta_tamanho$universo),
          line = list(color = palheta_tamanho$universo, fillcolor = palheta_tamanho$universo_fill),
          name = "Distribuição",
          showlegend = FALSE
        ) %>% 
          add_trace(
            data = df_medias_palavras(),
            y = ~media_palavras,
            type = "scatter",
            mode = "markers",
            name = 'Média Anual',
            marker = list(color = 'rgb(222, 0, 36)', size = 9, opacity = 0.7, line = list(color = "115, 1, 20")),
            line = list(width = 0, opacity = 0)
        )  %>%
          layout(title = "Extensão dos atos normativos editados pela Receita Federal",
                              yaxis = list(title = "Número de palavras dos atos normativos", range = c(0, q3_ref() * 2)),
                              xaxis = list (title = "Ano"))
        return(fig)
      })
      
      output$UI_mainPanel_scatter_evolucao_media_palavras <- renderPlotly({
        fig <- plot_ly(
          data = df_medias_palavras() %>% mutate(ano = as.numeric(ano)),
          x = ~ano,
          y = ~media_palavras,
          type = 'scatter',
          mode = 'lines+markers',
          name = "Todos (universo)",
          line = list(color = palheta_tamanho$universo, width = 4),
          marker = list(color = palheta_tamanho$universo, size = 8)
        )
        if ("Portarias" %in% input$tipos_normas) {
          fig <- fig %>%
            add_trace(
              data = filter(df_tipos_medias_palavras(), tipo_resumido == "Portarias"),
              x = ~ano,
              y = ~media_palavras,
              type = "scatter",
              mode = 'markers',
              name = "Portarias",
              marker = list(color = palheta_tamanho$portarias, size = 4, opacity = 0.75),
              line = list(color = palheta_tamanho$portarias, width = 2, opacity = 0.75, dash = "dot")
            )
        }
        if ("Instruções Normativas" %in% input$tipos_normas) {
          fig <- fig %>%
            add_trace(
              data = filter(df_tipos_medias_palavras(), tipo_resumido == "Instruções Normativas"),
              x = ~ano,
              y = ~media_palavras,
              type = "scatter",
              mode = 'markers',
              name = "Instruções Normativas",
              marker = list(color = palheta_tamanho$instrucoes, size = 4, opacity = 0.75),
              line = list(color = palheta_tamanho$instrucoes, width = 2, opacity = 0.75, dash = "dot")
              
            )
        }
        if ("Resoluções" %in% input$tipos_normas) {
          fig <- fig %>%
            add_trace(
              data = filter(df_tipos_medias_palavras(), tipo_resumido == "Resoluções"),
              x = ~ano,
              y = ~media_palavras,
              type = "scatter",
              mode = 'markers',
              name = "Resoluções",
              marker = list(color = palheta_tamanho$resolucoes, size = 4, opacity = 0.75),
              line = list(color = palheta_tamanho$resolucoes, width = 2, opacity = 0.75, dash = "dot")
              
            )
        }
        if ("Outros atos normativos" %in% input$tipos_normas) {
          fig <- fig %>%
            add_trace(
              data = filter(df_tipos_medias_palavras(), tipo_resumido == "Outros atos normativos"),
              x = ~ano,
              y = ~media_palavras,
              type = "scatter",
              mode = 'markers',
              name = "Outros atos normativos",
              marker = list(color = palheta_tamanho$outros_atos, size = 4, opacity = 0.75),
              line = list(color = palheta_tamanho$outros_atos, width = 2, opacity = 0.75, dash = "dot")
            )
        }
        fig <- fig %>% layout(title = "Extensão média dos atos normativos editados pela Receita Federal, por tipo",
                              yaxis = list(title = "Número de palavras dos atos normativos"),
                              xaxis = list (title = "Ano"))
        return(fig)
      })
      
    }
  )
}