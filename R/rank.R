#' Rankin por sexo, decada ou localidade.
#'
#' Retorna ranking dos nomes segundo a frequencia de nascimentos por sexo, decada ou localidade.
#'
#' @param sexo Se `NULL` (default), a consulta pelo nome eh unissex. Valores podem ser "M", para o sexo masculino, ou "F", para o feminino.
#' @param decada Se `NULL` (default), a consulta eh para todas as decadas. Para especificar a decada, utilise um numero inteiro.
#' @param localidade_cod Se `NULL` (default), a consulta eh para todo territorio. Para especificar uma localidade, utilize o identificador de um município ou de uma Unidade da Federação.
#'
#' @examples
#' brnome_rank()
#'
#' @export
brnome_rank <- function(sexo = NULL, decada = NULL, localidade_cod = NULL) {

  # if (!is.null(sexo) & !is.null(localidade_cod)) {
  #   stop("Sexo e localidade nao podem ser indicados em conjunto, especifique apenas um dos dois", call. = FALSE)
  # }
  #
  if (sum(!c(is.null(sexo), is.null(localidade_cod), is.null(decada))) > 1) {
    stop("Utilize apenas uma das opcoes de consulta.", call. = FALSE)
  }

  sexo <- verifica_sexo(sexo)

  localidade <- verifica_localidade(localidade_cod)

  decada <- verifica_decada(decada)

  consulta <- stringr::str_glue("https://servicodados.ibge.gov.br/api/v2/censos/nomes/ranking")

  if (!is.null(sexo)) {
    consulta <- stringr::str_glue("{consulta}/?sexo={sexo}")
  }

  if (!is.null(localidade_cod)) {
    consulta <- stringr::str_glue("{consulta}/?localidade={localidade$localidade}")
  }

  if (!is.null(decada)) {
    consulta <- stringr::str_glue("{consulta}/?decada={decada}")
  }

  tab <- pega_tabela(consulta) %>%
    dplyr::mutate(
      decada = ifelse(is.null(decada), NA, decada)
    ) %>%
    dplyr::left_join(localidades, by = c("localidade")) %>%
    dplyr::select(
      nome, sexo, decada, localidade_cod = localidade, localidade_nome,
      frequencia, ranking
    )

  # if (!is.null(sexo)) {
  #   tab$sexo <- toupper(sexo)
  # }

  if (is.null(localidade_cod)) {
    tab$localidade_cod <- localidade$localidade
    tab$localidade_nome <- localidade$localidade_nome
  }

  if (!is.null(decada)) {
    tab$decada <- decada
  }

  dplyr::mutate(
    tab,
    localidade_cod = ifelse(
      localidade_cod == "BR",
      localidade_cod,
      as.integer(localidade_cod)
    )
  )
}

# brnome_rank <- function(sexo = NULL, localidade_cod = NULL, decada_nascimento = NULL) {
#
#   sexo <- verifica_sexo(sexo)
#
#   localidade <- verifica_localidade(localidade_cod)
#
#   decada <- verifica_decada(decada_nascimento)
#
#   consulta <- "https://servicodados.ibge.gov.br/api/v1/censos/nomes/faixa?qtd=20"
#
#   if (!is.null(sexo)) {
#     consulta <- stringr::str_glue("{consulta}&sexo={sexo}")
#   } else {
#     consulta <- stringr::str_glue("{consulta}&sexo=")
#   }
#
#   if (!is.null(localidade_cod)) {
#     consulta <- stringr::str_glue("{consulta}&regiao={localidade$localidade}")
#   } else {
#     consulta <- stringr::str_glue("{consulta}&regiao=0")
#   }
#
#   stringr::str_glue("{consulta}&faixa={seq(1930, 2010, 10)}") %>%
#     '['(., stringr::str_detect(., paste0("faixa=", decada + 10))) %>%
#     purrr::map_dfr(pega_tabela) %>%
#     dplyr::group_by(faixa) %>%
#     dplyr::mutate(
#       decada_nascimento = as.integer(stringr::str_extract(faixa, "[:digit:]{4}")) - 10L,
#       regiao = ifelse(regiao == 0, NA_character_, regiao),
#       sexo = ifelse(sexo == "", NA_character_, stringr::str_to_upper(sexo)),
#       rank = dplyr::row_number(-freq)
#     ) %>%
#     dplyr::ungroup() %>%
#     dplyr::select(nome, sexo, regiao, decada_nascimento, freq, rank) %>%
#     dplyr::arrange(-decada_nascimento, rank)
# }

