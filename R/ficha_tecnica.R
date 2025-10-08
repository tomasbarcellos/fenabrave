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
ficha_tecnica <- function(montadora, modelo, ano, sleep = 1) {
  if (sleep != 0) {
    Sys.sleep(abs(rnorm(1, sleep)))
  }

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

link_valido_fichacompleta <- function(str) {
  idx <- stringr::str_detect(str, "^/carros") &
    stringr::str_detect(str, "\\d{4}") &
    stringr::str_detect(str, "/$", negate = TRUE)
  str[idx]
}

ficha_tecnica2 <- function(montadora, modelo, ano) {
  link <- glue::glue("https://www.fichacompleta.com.br/carros/{montadora}/{modelo}/")
  html <- rvest::read_html(link)


  links_modelo <- html %>%
    rvest::html_nodes("div.col-md-12") %>%
    rvest::html_nodes(".row") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    link_valido_fichacompleta() %>%
    stringr::str_subset(as.character(ano))

  links_completo <- glue::glue("https://www.fichacompleta.com.br{links_modelo}")

  raspar_link <- function(link) {
    pagina_modelo <- rvest::read_html(link)

    campos <- pagina_modelo %>%
      rvest::html_nodes(".colEsq") %>%
      rvest::html_text() %>%
      stringr::str_squish() %>%
      janitor::make_clean_names()

    valores <- pagina_modelo %>%
      rvest::html_nodes(".colDir") %>%
      rvest::html_text() %>%
      stringr::str_squish()

    tibble(montadora, modelo, campos, valores) %>%
      pivot_wider(names_from = campos, values_from = valores)
  }

  res <- map(links_completo, purrr::safely(raspar_link)) %>%
    map_df("result")

  saveRDS(res, glue::glue("ficha/{montadora}_{modelo}_{ano}.rds"))
  res
}
# ficha_tecnica2("audi", "a3", 2004)
dados_blp %>%
  filter(nas) %>%
  unique() %>%
  select(ano:modelo, hp, peso_kg, size, mpg) %>%
  write_csv("faltantes.csv")

