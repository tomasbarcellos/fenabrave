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


tabela_fipe <- function(ano, marca, modelo) {
  referencia <- get_referencia(ano)

  rg_marca <- stringr::regex(marca, ignore_case = TRUE)

  cod_marca <- get_marcas(referencia) %>%
    dplyr::filter(stringr::str_detect(marcas, rg_marca)) %>%
    dplyr::pull(codigo)

  tbl_modelos <- get_modelos(referencia, cod_marca)

  rg_busca <- stringr::regex(modelo, ignore_case = TRUE)

  cod_modelo <- tbl_modelos$modelos %>%
    dplyr::filter(stringr::str_detect(modelos, rg_busca)) %>%
    # ta quebrando
    # head(1) %>%
    dplyr::pull(codigos)

  links <- glue::glue("codigoTabelaReferencia={referencia}&",
                           "codigoMarca={cod_marca}&codigoModelo={cod_modelo}&",
                           "codigoTipoVeiculo=1&anoModelo=32000&",
                           "codigoTipoCombustivel=1&tipoVeiculo=carro&",
                      "modeloCodigoExterno=&tipoConsulta=tradicional")

  pegar_preco <- function(link) {
    partes <- link %>%
      stringr::str_split_1("&") %>%
      stringr::str_split("=")
    body <- partes %>%
      purrr::map(2)
    names(body) <- partes %>%
      purrr::map_chr(1)

    if (sample(c(TRUE, FALSE), 1, prob = c(0.1, 0.9))) {
      Sys.sleep(abs(rnorm(1, 5, 0.5)))
    }

    if (sample(c(TRUE, FALSE), 1, prob = c(0.3, 0.7))) {
      Sys.sleep(abs(rnorm(1, 2, 0.5)))
    }

    link_post <- "https://veiculos.fipe.org.br/api/veiculos/ConsultarValorComTodosParametros/"

    httr::POST(link_post, body = body, encode = "json") %>%
      httr::content() %>%
      tibble::as_tibble()
  }
  purrr::map(links, purrr::safely(pegar_preco)) %>%
    purrr::map_df("result")
}
teste <- purrr::pmap(
  list(caracteristicas$ano,
       caracteristicas$montadora,
       caracteristicas$modelo),
  purrr::safely(tabela_fipe))

teste %>%
  purrr::map_df("result")

teste %>%
  purrr::map(c("result", "result"))
  dplyr::filter(!is.na(Valor))

