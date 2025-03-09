#' Download de relatorio de emplacamento de dezembro
#'
#' @param ano Ano do relatorio
#'
#' @return NULL
#' @export
#'
#' @examples
#' download_relatorio_dez(2024)
download_relatorio_dez <- function(ano) {
  if (ano == 2024) {
    link <- "https://www.fenabrave.org.br/portal/files/2024_12_02.pdf"
  } else {
    link <- glue::glue(
      "https://www.fenabrave.org.br/portal/files/{ano}_12_2.pdf"
    )
  }

  tf <- glue::glue(tempdir(), "/fenabrave_dez_{ano}.pdf")
  download.file(link, tf)
}
