
# auxiliares ---------------------------------------------------------------

pega_tabela <- function(x) {

  x %>%
    xml2::read_html() %>%
    rvest::html_text() %>%
    jsonlite::fromJSON() %>%
    dplyr::as_tibble() %>%
    tidyr::unnest()
}

corrige_periodo <- function(x) {

  stringr::str_extract_all(x, "[:digit:]{4}", simplify = F) %>%
    purrr::map_chr(
      ~dplyr::case_when(
        length(.x) == 1 ~ stringr::str_glue("[    , {.x}]"),
        length(.x) == 2 ~ stringr::str_glue("[{.x[1]}, {.x[2]}]"),
      )[1]
    )
}

verifica_localidade <- function(x) {

  if (is.null(x)) {

    return(data.frame(localidade = "BR", localidade_nome = "Brasil", stringsAsFactors = FALSE))
  }

  if (length(x) != 1) {

    stop("Insira apenas uma localidade", call. = FALSE)
  }

  if (!is.numeric(x)) {

    stop("Localidade prace ser um valor numerico", call. = FALSE)
  }

  loc <- df_localidade %>%
    dplyr::filter(localidade== as.character(x))

  if (nrow(loc) == 0) {
    stop("Localidade incorreta", call. = FALSE)
  }

  loc
}

verifica_sexo <- function(x) {

  if (is.null(x)) {

    return(x)
  }

  if (length(x) != 1) {

    stop("Indique apenas um sexo", call. = FALSE)
  }

  if (!x %in% c("F", "f", "M", "m")) {

    stop('Indique o sexo apenas como "F" ou "M"', call. = FALSE)
  }

  stringr::str_to_lower(x)
}

verifica_decada <- function(x) {

  if (is.null(x)) {

    return(x)
  }

  if (length(x) != 1 & x %in% seq(1930, 2010, 10)) {

    stop("Decada incorreta")
  }

  x
}