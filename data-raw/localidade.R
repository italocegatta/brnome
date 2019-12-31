library(magrittr)

municipios <- "https://servicodados.ibge.gov.br/api/v1/localidades/municipios" %>%
  xml2::read_html() %>%
  rvest::html_text() %>%
  jsonlite::fromJSON() %>%
  dplyr::select(
    localidade = id,
    localidade_nome = nome
  ) %>%
  dplyr::mutate(tipo = "Município") %>%
  dplyr::as_tibble()

estados <- "https://servicodados.ibge.gov.br/api/v1/localidades/estados" %>%
  xml2::read_html() %>%
  rvest::html_text() %>%
  jsonlite::fromJSON() %>%
  dplyr::select(
    localidade = id,
    localidade_nome = nome
  ) %>%
  dplyr::mutate(tipo = "Estado") %>%
  dplyr::as_tibble()

localidades <- dplyr::bind_rows(municipios, estados) %>%
  dplyr::mutate(localidade = as.numeric(localidade))

usethis::use_data(
  localidades,
  overwrite = TRUE
)
