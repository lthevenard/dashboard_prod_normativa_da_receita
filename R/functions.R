prepare_evolucao_atos <- function(df, min_ano, max_ano) {
  df %>% 
    filter(ano <= max_ano & ano >= min_ano)
}

bold_with_color <- function(text, color = light_blue, opacity = 1) {
  return(
    paste0(
      "<b style = 'color: ", color, "; opacity: ", opacity, ";'>", text, "</b>"
    )
  )
}

htitle_with_color <- function(text, color = light_blue, htitle = 1) {
  return(
    paste0(
      "<h", htitle, " style = 'color: ", color, ";'>", text, "</h", htitle, ">"
    )
  )
}

add_link_tag <- function(text, href) {
  paste0(
    "<a href='", href, "'>", text, "</a>"
  )
}
