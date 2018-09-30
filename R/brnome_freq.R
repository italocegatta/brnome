#' Frenquencia por decada.
#'
#' Retorna a frequencia de nascimentos por decada para o nome consultado.
#'
#' @param nome Vetor com um ou mais nomes a serem consultados.
#' @param sexo Se `NULL` (default), a consulta pelo nome eh unissex. Valores podem ser "M", para o sexo masculino, ou "F", para o feminino.
#' @param localidade_cod Se `NULL` (default), a consulta eh para todo territorio. Para especificar uma localidade, utilize o identificador de um município ou de uma Unidade da Federação.
#'
#' @examples
#' brnome_freq("italo", "m")
#'
#' @export
brnome_freq <- function(nome, sexo = NULL, localidade_cod = NULL) {

  if (!is.null(sexo) & !is.null(localidade_cod)) {
    stop("Sexo e localidade nao podem ser indicados em conjunto, especifique apenas um dos dois", call. = FALSE)
  }

  nome <- stringr::str_c(nome, collapse = "%7C")

  sexo <- verifica_sexo(sexo)

  localidade <- verifica_localidade(localidade_cod)

  consulta <- stringr::str_glue("https://servicodados.ibge.gov.br/api/v2/censos/nomes/{nome}")

  if (!is.null(sexo)) {
    consulta <- stringr::str_glue("{consulta}?sexo={sexo}")
  }

  if (!is.null(localidade)) {
    consulta <- stringr::str_glue("{consulta}?localidade={localidade$localidade}")
  }

  consulta %>%
    xml2::read_html() %>%
    rvest::html_text() %>%
    jsonlite::fromJSON() %>%
    dplyr::as_tibble() %>%
    tidyr::unnest() %>%
    dplyr::mutate(
      nascimento_periodo = corrige_periodo(periodo),
      nascimento_decada = as.integer(stringr::str_extract(periodo, "[:digit:]{4}"))
    ) %>%
    dplyr::left_join(localidade, by = c("localidade")) %>%
    dplyr::select(
      nome, sexo, localidade_cod = localidade, localidade_nome,
      nascimento_periodo, nascimento_decada, frequencia
    )
}


# auxiliares ---------------------------------------------------------------

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

  if (length(x) != 1 & x %in% c("F", "f", "M", "m")) {

    stop('Indique o sexo apenas como "F" ou "M"')
  }

  stringr::str_to_lower(x)
}
