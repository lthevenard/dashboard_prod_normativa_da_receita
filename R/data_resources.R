library(tidyverse)

ANO_CORTE_MAX <- 2023
ANO_CORTE_MIN <- 1988

normas_select <- readRDS("data_input/select_database.rds") %>% 
  mutate(ano = str_extract(data, "\\d{4}") %>% as.numeric()) %>% 
  filter(ano >= ANO_CORTE_MIN & ano <= ANO_CORTE_MAX)

word_mean_per_year <- readRDS("data_input/word_mean_per_year.rds") %>% 
  filter(as.numeric(ano) >= ANO_CORTE_MIN & ano <= as.numeric(ANO_CORTE_MAX))

ca_table <- readRDS("data_input/ca_table.rds") %>% 
  mutate(class = ifelse(
    type == "dever", "Deveres e Obrigações", ifelse(
      type == "pena", "Multas e Penalidades", "Prazos Legais"
    )),
    ano = as.integer(ano)
  ) %>% 
  filter(ano >= ANO_CORTE_MIN & ano <= ANO_CORTE_MAX)

mod_relacional <- readRDS("data_input/mod_relacional.rds") %>% 
  filter(ano >= ANO_CORTE_MIN & ano <= ANO_CORTE_MAX)

content_table <- readRDS("data_input/content_table.rds")

content_plots_info <- list(
  dever = list(
    tipo = "dever", 
    titulo = "Incidência de termos relacionados ao estabelecimento de deveres / obrigações\n",
    cor = "#bd6ae6"
  ),
  pena = list(
    tipo = "pena", 
    titulo = "Incidência de termos relacionados à imposição de multas e penalidades\n",
    cor = "#1400fa"
  ),
  prazo = list(
    tipo = "prazo", 
    titulo = "Incidência de termos relacionados à criação ou alteração de prazos\n",
    cor = "#fc6df9"
  ),
  cor_comparacao = "#032863"
)
