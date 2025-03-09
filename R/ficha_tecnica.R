#' Recupera ficha tecncia do carro
#'
#' @param montadora Montadora
#' @param modelo Modelo
#' @param ano Ano do veiculo
#'
#' @return Uma tabela com caracteristicas do modelo
#' @export
#'
#' @examples
#' ficha_tecnica("fiat", "argo", 2022)
ficha_tecnica <- function(montadora, modelo, ano) {
  link <- glue::glue(
    "https://www.icarros.com.br/",
    "{stringr::str_to_lower(montadora)}/",
    "{stringr::str_to_lower(modelo)}/",
    "{ano}/ficha-tecnica"
  )
  html <- rvest::read_html(link)

  resultado <- rvest::html_table(html)

  simbolos <- html %>%
    rvest::html_elements(css = ".badge-icon.one-col") %>%
    rvest::html_children() %>%
    rvest::html_attr("class") %>%
    stringr::str_extract("check|times") %>%
    magrittr::equals("check") %>%
    as.character()

  resultado[[2]]$X2 <- as.character(resultado[[2]]$X2)
  resultado[[3]]$X2 <- simbolos[1:7]
  resultado[[4]]$X2 <- simbolos[8:13]
  resultado[[5]]$X2 <- simbolos[14:18]
  resultado[[6]]$X2 <- simbolos[19:21]
  resultado[[7]]$X2 <- simbolos[22:25]
  resultado[[8]]$X2 <- simbolos[26:28]

  alcool <- resultado[[1]][, 1:2] %>%
    dplyr::mutate(X1 = paste(X1, "alcool"))

  gasolina <- resultado[[1]][, c(1, 3)] %>%
    dplyr::rename(X2 = X3) %>%
    dplyr::mutate(X1 = paste(X1, "gasolina"))

  alcool %>%
    dplyr::bind_rows(
      gasolina, resultado[[2]], resultado[[3]], resultado[[4]],
      resultado[[5]], resultado[[6]], resultado[[7]], resultado[[8]]
    ) %>%
    tidyr::pivot_wider(names_from = X1, values_from = X2) %>%
    janitor::clean_names() %>%
    dplyr::mutate(ano, montadora, modelo, .before = dplyr::everything())
}
