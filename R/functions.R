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

plot_content_analysis <- function(chosen_type, title, hue, df, df_join) {
  df %>% 
    filter(type == chosen_type) %>% 
    count(id) %>% 
    right_join(df_join) %>% 
    mutate(ano = str_extract(data, "\\d{4}")) %>% 
    mutate(n = ifelse(is.na(n), 0, n)) %>% 
    group_by(ano) %>% 
    summarise(`Incidência total` = sum(n, na.rm = TRUE), `Médias anuais` = mean(n, na.rm = TRUE)) %>% 
    mutate(ano = as.integer(ano)) %>% 
    pivot_longer(cols = c(`Incidência total`, `Médias anuais`)) %>% 
    ggplot(aes(x= ano, y = value, group = "")) +
    geom_point(color = hue) +
    labs(title = title,
         x = "", y = "") +
    geom_smooth(method = "lm", color = "#424242", size = 0.5, linetype = "dashed") +
    facet_wrap(~name, scales = "free_y") +
    theme_minimal() +
    theme(plot.title = element_text(hjust=0.5))
}

plot_comparacao_temas <- function(df) {
  df %>% 
    ggplot(aes(x = ano, y = N)) +
    geom_point(color = content_plots_info$cor_comparacao) +
    geom_smooth(method = "lm", color = "#424242", size = 0.5, linetype = "dashed") +
    labs(title = "Incidência de termos relacionados a obrigações, penalidades e prazos\n",
         y = "Incidência Total", x = "") +
    facet_wrap(~class) +
    theme_minimal() +
    theme(plot.title = element_text(hjust=0.5))
}
