texto_apresentacao_da_pesquisa <- function() {
  paste0(
    htitle_with_color(
      "A evolução da produção normativa da Receita Federal do Brasil (1988-2020): análise empírica e implicações regulatórias",
      color = darker_blue,
      htitle = 2
    ), 
    "\n<hr><br>\n<p>",
    read_file("text_input/texto_apresentacao_da_pesquisa.txt") %>%
      str_replace_all("\\n+", "</p>\n<p>"),
    "</p>"
  ) %>% 
    str_replace(
      "(?<=deu origem a uma )publicação acadêmica",
      "publicação acadêmica" %>% 
        add_link_tag("https://www.scielo.br/j/rdgv/a/fq5sqBqSVYMMzdSJxgdGBts/") %>% 
        bold_with_color()
    )
}

texto_apresentacao_footnote <- function() {
  paste0(
    "<div style = 'color: gray; font-size: 80%;'>",
    read_file("text_input/texto_apresentacao_footnote.txt"),
    "</div>"
  ) %>%
    str_replace(
      "repositório público desta pesquisa no Github",
      "repositório público desta pesquisa no Github" %>% 
        add_link_tag("https://github.com/lthevenard/receita_scraper") %>% 
        bold_with_color()
    ) %>% 
    str_replace(
      "repositório próprio",
      "repositório próprio" %>% 
        add_link_tag("https://www.google.com") %>% 
        bold_with_color()
    ) %>% 
    str_replace(
      "Observação",
      "Observação" %>% 
        bold_with_color(color = "#4f4f4f")
    )
}

texto_principal_mod_tamanho <- function() {
  texto <- read_file("text_input/texto_principal_mod_tamanho.txt")
  titulo <- str_extract(texto, ".+")
  corpo <- str_remove(texto, ".+")
  paste0(
    htitle_with_color(
      titulo,
      color = darker_blue,
      htitle = 3
    ), 
    "\n<hr><br>\n<p>",
    str_replace_all(corpo, "\\n+", "</p>\n<p>"),
    "</p>"
  )
}

texto_principal_mod_interacao <- function() {
  texto <- read_file("text_input/texto_principal_mod_interacao.txt")
  titulo <- str_extract(texto, ".+")
  corpo <- str_remove(texto, ".+")
  paste0(
    htitle_with_color(
      titulo,
      color = darker_blue,
      htitle = 3
    ), 
    "\n<hr><br>\n<p>",
    str_replace_all(corpo, "\\n+", "</p>\n<p>"),
    "</p>"
  )
}

texto_footnote_mod_interacao <- function() {
  paste0(
    "<div style = 'color: gray;'><i><p>",
    read_file("text_input/texto_footnote_mod_interacao.txt") %>%
      str_replace_all("\\n+", "</p>\n<p>"),
    "</p></i>"
  )
}

texto_principal_mod_conteudo <- function() {
  texto <- read_file("text_input/texto_principal_mod_conteudo.txt")
  titulo <- str_extract(texto, ".+")
  corpo <- str_remove(texto, ".+")
  paste0(
    htitle_with_color(
      titulo,
      color = darker_blue,
      htitle = 2
    ), 
    "\n<hr><br>\n<p>",
    str_replace_all(corpo, "\\n+", "</p>\n<p>"),
    "</p>"
  )
}

UI_footer <- function() {
  paste0(
    "</br><p style = 'text-align: center; font-size:0.8em; color: white; line-height: 50%;'>",
    "<b>Criado por</b>: Lucas Thevenard | ",
    "<b>Última atualização</b>: 15/07/2024",
    "</p>",
    "<p style = 'text-align: center; font-size:0.8em; color: #b5d1e8;'>| ",
    "<a style = 'color: #b5d1e8;', href = 'https://github.com/lthevenard/receita_scraper'>Repositório da pesquisa</a> | ",
    "<a style = 'color: #b5d1e8;', href = 'https://github.com/lthevenard/'>Repositório deste dashboard</a> | ",
    "<a style = 'color: #b5d1e8;', href = 'https://www.linkedin.com/in/lthevenard/'>LinkedIn do autor</a> | ",
    "</p>"
  )
}