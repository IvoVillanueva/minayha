


# librerias y csv con logos y colores -------------------------------------


library(tidyverse)
library(rvest)
library(gt)
library(ggtext)
library(janitor)
library(gtExtras)

logo <- read.csv("logos2023.csv")



# lugar donde se guarda ---------------------------------------------------


here::here("matchup", "matchup.R")


# enfrentamientos jornda --------------------------------------------------



url <- "https://www.acb.com/calendario/index/temporada_id/2022/edicion_id/967" %>%
  read_html()

jor4 <- tibble(
  local = url %>% html_elements("div:nth-of-type(54) .local span.nombre_largo") %>%
    html_text(),
  visitante = url %>% html_elements("div:nth-of-type(54) .visitante span.nombre_largo") %>%
    html_text()
)



# valoracion recibida por los equipos -------------------------------------



pos <- c("Bases", "Aleros", "Pívots")

recPosloc <- function(pos) {
  url <- "https://www.rincondelmanager.com/smgr/valoracion_rec.php"
  pgsession <- session(url)
  pgform <- html_form(pgsession)[[5]]
  filled_form <- html_form_set(pgform,
    "de_jor" = "1",
    "a_jor" = "25",
    "cancha" = "1"
  )

  df <- submit_form(session = pgsession, form = filled_form, POST = url)
  df <- df %>%
    html_element(glue::glue("table#t{pos}")) %>%
    html_table()
  df$pos <- pos
  df$tipo <- "local"

  return(df)
}

recPosvis <- function(pos) {
  url <- "https://www.rincondelmanager.com/smgr/valoracion_rec.php"
  pgsession <- session(url)
  pgform <- html_form(pgsession)[[5]]
  filled_form <- html_form_set(pgform,
    "de_jor" = "1",
    "a_jor" = "25",
    "cancha" = "0"
  )

  df <- submit_form(session = pgsession, form = filled_form, POST = url)
  df <- df %>%
    html_element(glue::glue("table#t{pos}")) %>%
    html_table()
  df$pos <- pos
  df$tipo <- "visitante"

  return(df)
}
recPosloc_df <- map_df(pos, recPosloc)
recPosvis_df <- map_df(pos, recPosvis)



# Data wrangler de la extracion de datos ----------------------------------



loc_df <- recPosloc_df %>%
  select(Equipo, Media, pos, tipo) %>%
  clean_names() %>%
  separate(media, c("media_total", "media_player"), sep = "( )") %>%
  mutate_at(vars("media_total", "media_player"), list(~ str_replace(., ",", "."))) %>%
  mutate(
    media_player = str_remove_all(media_player, "[( )]"),
    across(c("media_total", "media_player"), as.numeric)
  ) %>%
  pivot_wider(id_cols = c(equipo, tipo), names_from = pos, values_from = contains("media")) %>%
  select(abb = equipo, contains("player"), tipo) %>%
  mutate(abb = case_when(
    abb == "CBG" ~ "COV",
    abb == "OBR" ~ "MOB",
    abb == "RMA" ~ "RMB",
    abb == "BAS" ~ "BKN",
    abb == "FCB" ~ "BAR",
    abb == "CAN" ~ "LNT",
    abb == "ZAR" ~ "CAZ",
    abb == "BLB" ~ "SBB",
    abb == "MUR" ~ "UCM",
    abb == "MAN" ~ "BAX",
    TRUE ~ abb
  )) %>%
  left_join(logo) |>
  left_join(x = jor4 |> select(local), by = c("local" = "equipo")) %>%
  select(local, logo, contains("player_"))


vis_df <- recPosvis_df %>%
  select(Equipo, Media, pos, tipo) %>%
  clean_names() %>%
  separate(media, c("media_total", "media_player1"), sep = "( )") %>%
  mutate_at(vars("media_total", "media_player1"), list(~ str_replace(., ",", "."))) %>%
  mutate(
    media_player1 = str_remove_all(media_player1, "[( )]"),
    across(c("media_total", "media_player1"), as.numeric)
  ) %>%
  pivot_wider(id_cols = c(equipo, tipo), names_from = pos, values_from = contains("media")) %>%
  select(abb1 = equipo, contains("player1"), tipo_vis = tipo) %>%
  mutate(abb1 = case_when(
    abb1 == "CBG" ~ "COV",
    abb1 == "OBR" ~ "MOB",
    abb1 == "RMA" ~ "RMB",
    abb1 == "BAS" ~ "BKN",
    abb1 == "FCB" ~ "BAR",
    abb1 == "CAN" ~ "LNT",
    abb1 == "ZAR" ~ "CAZ",
    abb1 == "BLB" ~ "SBB",
    abb1 == "MUR" ~ "UCM",
    abb1 == "MAN" ~ "BAX",
    TRUE ~ abb1
  )) %>%
  left_join(logo, by = c("abb1" = "abb")) |>
  left_join(x = jor4 |> select(visitante), by = c("visitante" = "equipo")) %>%
  select(visitante, logo2vis = logo, contains("player1_"))

jorDfgt <- cbind(loc_df, vis_df)


# tabla -------------------------------------------------------------------

path <- here::here("matchup", "recjor23")

jorDfgt %>%
  select(local, logo, contains("player_"), contains("player1_"), logo2vis, visitante) %>%
  mutate(
    local = ifelse(local == "Joventut Badalona", "Joventut", local),
    visitante = ifelse(visitante == "Joventut Badalona", "Joventut", visitante)
  ) %>%
  gt() %>%
  gt_img_rows(logo) %>%
  gt_img_rows(logo2vis) %>%
  # tab_spanner(
  #   label =  gt::html("<span style='font-weight:bold;font-size:12px'>Como Local</span>"),
  #   columns = c(local:media_player_Pívots)
  # ) %>%
  # tab_spanner(
  #   label =  gt::html("<span style='font-weight:bold;font-size:12px'>Como Visitante</span>"),
  #   columns = c(media_player1_Bases:visitante)
  # ) |>
  tab_source_note(
    source_note = md("**Datos**: *@elrincondelsm* &nbsp;&nbsp; <img src='https://www.rincondelmanager.com/smgr/images/logo.png'
                     style='height:12px;'>,  *@ACBCOM*&nbsp;&nbsp;<img src='https://upload.wikimedia.org/wikipedia/commons/3/3f/Acb_2019_logo.svg'
                     style='height:12.5px;'><br>
                     **Gráfico**: *Ivo Villanueva* &bull;  <span style='color:#00acee;font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span> <b>@elcheff</b>")
  ) %>%
  cols_label(
    local = gt::html("<span style='font-weight:bold;font-size:14px'>Como Local</span>"),
    logo = "",
    media_player_Bases = "Bases",
    media_player_Aleros = "Aleros",
    media_player_Pívots = "Pivots",
    visitante = gt::html("<span style='font-weight:bold;font-size:14px'> Como Visitante</span>"),
    logo2vis = "",
    media_player1_Bases = "Bases",
    media_player1_Aleros = "Aleros",
    media_player1_Pívots = "Pivots"
  ) %>%
  cols_align(
    align = "center",
    columns = c(media_player_Bases:media_player_Pívots, media_player1_Bases:media_player_Pívots, logo, logo2vis)
  ) %>%
  cols_align(
    align = "right",
    columns = local
  ) %>%
  data_color(
    columns = contains("player_"),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "Redmonder::dPBIRdGn",
        direction = 1
      ) %>% as.character(),
      domain = c(0, 16),
      na.color = "#005C55FF"
    )
  ) %>%
  data_color(
    columns = contains("player1_"),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "Redmonder::dPBIRdGn",
        direction = 1
      ) %>% as.character(),
      domain = c(0, 16),
      na.color = "#005C55FF"
    )
  ) |>
  tab_header(
    title = md("<img src='https://raw.githubusercontent.com/IvoVillanueva/acb_logo/main/Logo%20SM%20mosca%20340x340.png'
               style='height:40px;'> <img src='https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Liga_Endesa_2019_logo.svg/800px-Liga_Endesa_2019_logo.svg.png'
               style='height:40px;'><br><span style='font-weight:bold;font-dystopian:small-caps;font-size:24px'>Valoración Recibida Hasta J25</span>"),#ojo que esta con el logo de navidad
    subtitle = md(str_to_title("<span style='font-weight:400;color:#8C8C8C;font-size:12px'>Enfrentamientos para la J26<br><br></span>"))
  ) |>
  tab_options(
    heading.title.font.size = 18,
    heading.subtitle.font.size = 10,
    heading.title.font.weight = "bold",
    column_labels.font.size = 11,
    column_labels.font.weight = "bold",
    table.font.size = 11,
    table.font.names = "Roboto Condensed",
    source_notes.font.size = 7,
    data_row.padding = px(.4)
  ) |>
  gtsave("recjor26.png", expand =50)
