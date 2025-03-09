#' Download de relatorio de emplacamento de dezembro
#'
#' @param ano Ano do relatorio
#'
#' @return O link para o relatorio
link_relatorio_dez <- function(ano) {
  if (ano == 2024) {
    return("https://www.fenabrave.org.br/portal/files/2024_12_02.pdf")
  }

  if (ano >= 2017) {
    return(
      glue::glue(
        "https://www.fenabrave.org.br/portal/files/{ano}_12_2.pdf"
      )
    )
  }

  prefixo <- ifelse(
    ano %in% c(2003, 2015, 2016),
    2, ifelse(
      ano %in% c(2004, 2005), 3, ifelse(
        ano %in% c(2006, 2014), 4, ifelse(
          ano %in% c(2007:2011), 5, ifelse(
            ano %in% c(2012, 2013), 6, NA_character_
          )))))

  glue::glue(
    "https://www.fenabrave.org.br/portal/files/",
    "{prefixo}_{ano}_12_2.pdf"
  )
}
