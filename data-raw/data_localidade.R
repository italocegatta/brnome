
l_estado <- dplyr::select(
  sf::st_set_geometry(brmap::brmap_estado, NULL),
  localidade = cod_estado,
  localidade_nome = estado_nome
)

l_municipio <- dplyr::select(
  sf::st_set_geometry(brmap::brmap_municipio, NULL),
  localidade = cod_municipio,
  localidade_nome = municipio
)

df_localidade <- dplyr::mutate(
  dplyr::bind_rows(l_estado, l_municipio),
  localidade = as.character(localidade)
)

usethis::use_data(
  df_localidade,
  internal = TRUE, overwrite = TRUE
)
