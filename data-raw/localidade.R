library(magrittr)

municipios <- "https://servicodados.ibge.gov.br/api/v1/localidades/municipios" %>%
  xml2::read_html() %>%
  rvest::html_text() %>%
  jsonlite::fromJSON() %>%
  dplyr::select(
    localidade = id,
    localidade_nome = nome
  ) %>%
  dplyr::as_tibble()

estados <- "https://servicodados.ibge.gov.br/api/v1/localidades/estados" %>%
  xml2::read_html() %>%
  rvest::html_text() %>%
  jsonlite::fromJSON() %>%
  dplyr::select(
    localidade = id,
    localidade_nome = nome
  ) %>%
  dplyr::as_tibble()

localidades <- dplyr::bind_rows(municipios, estados) %>%
  dplyr::mutate(localidade = as.character(localidade))

usethis::use_data(
  localidades,
  overwrite = TRUE
)
