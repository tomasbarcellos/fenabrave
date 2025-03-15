df_tentativas <- emplacamento_anual %>%
  dplyr::mutate(
    modelo = tolower(modelo) %>%
      stringr::str_replace(" ", "-"),
    montadora = tolower(montadora),
    montadora = dplyr::case_when(
      montadora == "gm" ~ "chevrolet",
      montadora == "vw" ~ "volkswagen",
      montadora == "vw/fox" ~ "volkswagen",
      montadora == "vw/man" ~ "volkswagen",
      montadora == "vw-trucks-e-bus" ~ "volkswagen",
      montadora == "m.benz" ~ "mercedes-benz",
      montadora == "caoa chery" ~ "caoa-chery",
      TRUE ~ montadora
    ),
    modelo = dplyr::case_when(
      modelo == "fox/cross-fox" ~ "fox",
      modelo == "etios-hb" ~ "etios",
      modelo == "yaris-hb" ~ "yaris",
      modelo == "cruze-sedan" ~ "cruze",
      modelo == "cruze-hb" ~ "cruze-sport6",
      modelo == "hilux-sw4" ~ "sw4",
      modelo == "space-fox" ~ "spacefox",
      modelo == "crv" ~ "cr-v",
      modelo == "206ws" ~ "206",
      modelo == "city-hatch" ~ "city-hatchback",
      modelo == "320i" ~ "serie-3",
      montadora == "kia" & stringr::str_detect(modelo, "k\\d+") ~ "bongo",
      stringr::str_detect(modelo, "sprinter-") ~ "sprinter",
      montadora == "ford" & stringr::str_detect(modelo, "f(?=\\d+)") ~
        stringr::str_replace(modelo, "f", "f-"),
      montadora == "iveco" & stringr::str_detect(modelo, "daily-") ~ "daily",
      TRUE ~ modelo
    ))

total_anos <- df_tentativas %>%
  dplyr::group_by(ano) %>%
  dplyr::summarise(total = sum(quantidade))

resp <- df_tentativas %>%
  dplyr::anti_join(dados) %>%
  dplyr::mutate(resp = purrr::pmap(list(montadora = montadora,
                                        modelo = modelo,
                                        ano = ano),
                                   purrr::safely(ficha_tecnica)))
caracteristicas <- resp %>%
  tidyr::unnest(resp) %>%
  dplyr::filter(!purrr::map_lgl(resp, is.null),
                purrr::map_lgl(resp, tibble::is_tibble)) %>%
  dplyr::mutate(resp = purrr::map(resp, dplyr::select,
                                  -c(montadora, modelo, ano))) %>%
  tidyr::unnest(resp) %>%
  dplyr::group_by(tipo, montadora, modelo, ano) %>%
  dplyr::summarise(across(-quantidade, dplyr::first),
                   quantidade = sum(quantidade))

# caracteristicas %>%
#   dplyr::transmute(ano, quantidade, potencia_cv_gasolina = ifelse(potencia_cv_gasolina != "N/D",
#                                                  potencia_cv_gasolina,
#                                                  potencia_cv_alcool),
#                    hp_wt = as.numeric(potencia_cv_gasolina) /
#                      as.numeric(peso_kg),
#                    potencia_cv_gasolina = NULL,
#                    size = as.numeric(largura_mm) * as.numeric(comprimento_mm),
#                    ar_condicionado = as.numeric(as.logical(ar_condicionado)),
#                    mpg = as.numeric(ifelse(consumo_cidade_km_l_gasolina != "N/D",
#                                            consumo_cidade_km_l_gasolina,
#                                            consumo_cidade_km_l_alcool))) %>%
#   dplyr::group_by(ano) %>%
#   dplyr::summarise(dplyr::across(-quantidade, weighted.mean, w = quantidade,
#                                  na.rm = TRUE),
#                    quantidade = sum(quantidade))

usethis::use_data(caracteristicas, overwrite = TRUE)
