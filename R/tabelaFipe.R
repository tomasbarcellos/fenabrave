library(rvest)
link <- "https://veiculos.fipe.org.br/#carro-comum"
pag_live <- read_html_live(link)


get_referencia <- function(ano_) {
  tbl_ano %>%
    dplyr::filter(ano == ano_) %>%
    dplyr::pull(valor) %>%
    unique()
}

get_marcas <- function(referencia) {
  body <- list(codigoTabelaReferencia = referencia,
               codigoTipoVeiculo = 1)
  resp <- "https://veiculos.fipe.org.br/api/veiculos//ConsultarMarcas" %>%
    httr::POST(body = body) %>%
    httr::content()

  marcas <- resp %>%
    purrr::map_chr(1) %>%
    stringr::str_to_lower()

  codigo <- resp %>%
    purrr::map_chr(2)

  tibble::tibble(
    codigo = codigo,
    marcas = marcas
  )
}

get_modelos <- function(referencia, cod_marca) {

  body <- list(codigoTipoVeiculo = "1",
               codigoTabelaReferencia = referencia,
               codigoMarca = cod_marca)

  resp <- "https://veiculos.fipe.org.br/api/veiculos//ConsultarModelos" %>%
    httr::POST(body = body) %>%
    httr::content()

  list(
    modelos = tibble::tibble(
      modelos = resp[[1]] %>%
        purrr::map_chr(1),
      codigos = resp[[1]] %>%
        purrr::map(2) %>%
        purrr::map_chr(as.character),
    ),
    anos = tibble::tibble(
      combustivel = resp[[2]] %>%
        purrr::map_chr(1),
      codigos = resp[[2]] %>%
        purrr::map_chr(2),
    )
  )


}
tabela_fipe <- function(referencia, marca, modelo, ano) {
  referencia <- get_referencia(ano)
  cod_marca <- get_marcas(referencia) %>%
    dplyr::filter(marcas == marca) %>%
    dplyr::pull(codigo)

  tbl_modelos <- get_modelos(referencia, cod_marca)

  link2 <- "https://veiculos.fipe.org.br/api/veiculos/ConsultarValorComTodosParametros/"
  partes <- stringr::str_c("codigoTabelaReferencia={referencia}&",
                           "codigoMarca={marca}&codigoModelo={modelo}&",
                           "codigoTipoVeiculo=1&anoModelo={ano}&",
                           "codigoTipoCombustivel=1&tipoVeiculo=carro&",
                           "modeloCodigoExterno=&tipoConsulta=tradicional") %>%
    stringr::str_split_1("&") %>%
    stringr::str_split("=")
  body <- partes %>%
    purrr::map(2)
  names(body) <- partes %>%
    purrr::map_chr(1)

  rpost <- httr::POST(link2, body = body, encode = "json")
  rpost %>% httr::content()
}



