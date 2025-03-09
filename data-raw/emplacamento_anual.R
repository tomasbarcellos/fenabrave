# download do pdf
emplacamento_anual <- purrr::map_df(2003:2024, leitura_relatorio_dez)
# usethis::use_data(emplacamento_anual, overwrite = TRUE)
