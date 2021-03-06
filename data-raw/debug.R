library(brnome)

# frequencia --------------------------------------------------------------

library(magrittr)
data("localidades")

nome = "eliza"
sexo = "f"
localidade_cod = 33
localidade_cod = 1100023



brnome_freq("Eliza")
brnome_freq("italo", "f")
brnome_freq("italo", "f", 33)
brnome_freq("eliza", localidade_cod = 33)
brnome_freq("eliza", localidade_cod =  1100023)
brnome_freq("italo", localidade_cod =  3300100)

# rank --------------------------------------------------------------------

library(magrittr)
sexo = "M"
localidade_cod = 3300100
localidade_cod = 33
decada = 2000

brnome_rank()
brnome_rank(sexo = "M")
brnome_rank(localidade_cod = 33)
brnome_rank(localidade_cod = 3300100)
brnome_rank(decada = 2000)

brnome_rank(sexo = "M", localidade_cod = 33)
brnome_rank(sexo = "M", decada = 2000, localidade_cod = 3300100)


"https://servicodados.ibge.gov.br/api/v2/censos/nomes/ranking" %>%
  xml2::read_html() %>%
  rvest::html_text() %>%
  jsonlite::fromJSON() %>%
  dplyr::as_tibble() %>%
  tidyr::unnest()

consulta <- "https://servicodados.ibge.gov.br/api/v1/censos/nomes/faixa?qtd=20&sexo=&regiao=0"

# 2.75
system.time({
  future::plan(future::multiprocess)
  stringr::str_glue("{consulta}&faixa={seq(1930, 2010, 10)}") %>%
    furrr::future_map_dfr(brnome:::pega_tabela)
})

# 2.75
system.time({
  future::plan(future::sequential)
  stringr::str_glue("{consulta}&faixa={seq(1930, 2010, 10)}") %>%
    furrr::future_map_dfr(brnome:::pega_tabela)
})

# 2.75
system.time({
  stringr::str_glue("{consulta}&faixa={seq(1930, 2010, 10)}") %>%
    map_dfr(brnome:::pega_tabela)
})

