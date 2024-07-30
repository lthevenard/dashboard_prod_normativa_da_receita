mod_apresentacao_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      style = paste0("background-color: ", ultra_light_blue),
      column(2),
      column(
        8,
        br(), br(), br(),
        htmlOutput(ns("apresentacao_da_pesquisa")),
        br(), br(), hr(), br(), br(),
        htmlOutput(ns("apresentacao_footnote")),
        br(), br(), br(), br()
      ),
      column(2)
    ),
    fluidRow(style = paste0("background-color: ", deep_blue), column(12, HTML(UI_footer())))
  )
}

mod_apresentacao_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$apresentacao_da_pesquisa <- renderUI({HTML(texto_apresentacao_da_pesquisa())})
      output$apresentacao_footnote <- renderUI({HTML(texto_apresentacao_footnote())})
    }
  )
}
