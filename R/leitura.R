#' Leitura de relatorio de dezembro
#'
#' @param ano Ano do relatorio
#'
#' @return Uma tibble com os dados do acumulado do ano
leitura_relatorio_dez <- function(ano) {
  arquivo <- link_relatorio_dez(ano)

  texto <- pdftools::pdf_text(arquivo)[[7]]

  modelos <- stringr::str_split_1(texto, "\\d{1,2}\\u00ba") %>%
    stringr::str_squish() %>%
    utils::tail(-1) %>%
    stringr::str_remove(" www\\.fenabrave.+")

  tibble::tibble(
    texto = modelos
  ) %>%
    dplyr::mutate(tipo = rep(c("automovel", "utilitario"), 50),
                  montadora = stringr::str_extract(texto, ".+(?=/)"),
                  modelo = stringr::str_extract(texto, "(?<=/).+(?= \\d)"),
                  quantidade = texto %>%
                    stringr::str_remove_all("\\.") %>%
                    stringr::str_extract("\\d+$") %>%
                    as.numeric(),
                  ano = ano) %>%
    dplyr::select(-texto)
}
