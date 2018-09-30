#' Frenquencia por decada.
#'
#' Retorna a frequencia de nascimentos por decada de nascimento para o nome consultado.
#'
#' @param nome Vetor com um ou mais nomes a serem consultados.
#' @param sexo Se `NULL` (default), a consulta pelo nome eh unissex. Valores podem ser "M", para o sexo masculino, ou "F", para o feminino.
#' @param localidade_cod Se `NULL` (default), a consulta eh para todo territorio. Para especificar uma localidade, utilize o identificador de uma Unidade da Federação.
#'
#' @examples
#' brnome_freq("italo", "m")
#'
#' @export
brnome_freq <- function(nome, sexo = NULL, localidade_cod = NULL) {

  #nome <- stringr::str_c(nome, collapse = "%7C") # corrigir para aceitar so um nome

  sexo <- verifica_sexo(sexo)

  localidade <- verifica_localidade(localidade_cod)

  consulta <- stringr::str_glue("https://servicodados.ibge.gov.br/api/v1/censos/nomes/faixa?nome={nome}")

  if (!is.null(sexo)) {
    consulta <- stringr::str_glue("{consulta}&sexo={sexo}")
  }

  if (!is.null(localidade_cod)) {
    consulta <- stringr::str_glue("{consulta}&regiao={localidade$localidade}")
  }

  pega_tabela(consulta) %>%
    dplyr::mutate(
      decada_nascimento = as.integer(stringr::str_extract(faixa, "[:digit:]{4}")) - 10L,
      regiao = ifelse(regiao == 0, NA_character_, regiao),
      sexo = ifelse(sexo == "", NA_character_, stringr::str_to_upper(sexo))
    ) %>%
    dplyr::select(nome, sexo, regiao, decada_nascimento, freq)
}

# angita
# brnome_freq <- function(nome, sexo = NULL, localidade_cod = NULL) {
#
#   if (!is.null(sexo) & !is.null(localidade_cod)) {
#     stop("Sexo e localidade nao podem ser indicados em conjunto, especifique apenas um dos dois", call. = FALSE)
#   }
#
#   nome <- stringr::str_c(nome, collapse = "%7C")
#
#   sexo <- verifica_sexo(sexo)
#
#   localidade <- verifica_localidade(localidade_cod)
#
#   consulta <- stringr::str_glue("https://servicodados.ibge.gov.br/api/v2/censos/nomes/{nome}")
#
#   if (!is.null(sexo)) {
#     consulta <- stringr::str_glue("{consulta}?sexo={sexo}")
#   }
#
#   if (!is.null(localidade)) {
#     consulta <- stringr::str_glue("{consulta}?localidade={localidade$localidade}")
#   }
#
#   pega_tabela(consulta) %>%
#     dplyr::mutate(
#       nascimento_periodo = corrige_periodo(periodo),
#       nascimento_decada = as.integer(stringr::str_extract(periodo, "[:digit:]{4}"))
#     ) %>%
#     dplyr::left_join(localidade, by = c("localidade")) %>%
#     dplyr::select(
#       nome, sexo, localidade_cod = localidade, localidade_nome,
#       nascimento_periodo, nascimento_decada, frequencia
#     )
# }
