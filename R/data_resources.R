ANO_CORTE_MAX <- 2023
ANO_CORTE_MIN <- 1988

normas_select <- readRDS("data_input/select_database.rds") %>% 
  mutate(ano = str_extract(data, "\\d{4}") %>% as.numeric()) %>% 
  filter(ano >= ANO_CORTE_MIN & ano <= ANO_CORTE_MAX)
word_mean_per_year <- readRDS("data_input/word_mean_per_year.rds")
ca_table <- readRDS("data_input/ca_table.rds")
mod_relacional <- readRDS("data_input/mod_relacional.rds")