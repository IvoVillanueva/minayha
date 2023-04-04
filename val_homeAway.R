


#load libraries
library(rvest)
library(tidyverse)
library(gt)
library(gtExtras)


# datos -------------------------------------------------------------------

# logos equipos -----------------------------------------------------------

tm_acb <- read.csv("https://raw.githubusercontent.com/IvoVillanueva/csv2023/main/logos2023.csv") %>% #data of logos colors and names
  select(tm = abb, logo, color) |>
  drop_na()


caras <- read.csv("jugadores_2023.csv") |>
  select(jugador = nombre, foto)|>
  mutate( foto =ifelse(!str_detect(foto, "https:"), paste0("https:", foto), foto))


# play by play para averiguar el 20% ------------------------------------------------------------



iddf_df <- read.csv("PlayByPlay/iddf_df23.csv") |>
  filter(id_phase == 107) |>
  select(id_match = id, id_phase, team_home = local_team_team_abbrev_name, team_away = visitor_team_team_abbrev_name, score_home = score_local, score_away = score_visitor, matchweek_number) %>%
pivot_longer(cols = starts_with("team"), names_to = "tipo", names_prefix = "team_", values_to = "team") %>%
  mutate( win = case_when(tipo == "home" & score_home > score_away ~ 1,
                          tipo == "away" & score_away >score_home ~ 1,
                          TRUE ~ 0)) %>%
  select(id_match, matchweek_number:win)


iddf_df2 <- read.csv(here::here("PlayByPlay", "iddf_df23.csv")) |>
  select(id_match = id, week_description, matchweek_number) #para unir con el play by play y así poder filtrar por jornadas


pbp_acb_23 <- read_csv( here::here("PlayByPlay","pbp_Df2023.csv"),
                        show_col_types = FALSE
)|>
  left_join(iddf_df2)

tm <- pbp_acb_23 %>%
  select(license_licenseAbbrev, team_team_abbrev_name, team_team_actual_name) %>%
  distinct()


tm1 <- pbp_acb_23 %>%
  select( team_team_abbrev_name, team_team_actual_name) %>%
  distinct() %>%
  drop_na()

partidos <- pbp_acb_23  %>%
  filter(matchweek_number>17) %>%
  group_by(license_licenseAbbrev) %>%
  distinct(id_match) %>%
  count(license_licenseAbbrev)

face <- pbp_acb_23 %>%
  select(license_licenseStr15, license_licenseAbbrev) %>%
  left_join(caras, by =c("license_licenseStr15" = "jugador")) %>%
  unique()


# calendario --------------------------------------------------------------



url <- "https://www.acb.com/calendario/index/temporada_id/2022/edicion_id/967" %>%
  read_html()

jor <- tibble(
  local = url %>% html_elements("div:nth-of-type(54) .local span.nombre_largo") %>%
    html_text(),
  visitante = url %>% html_elements("div:nth-of-type(54) .visitante span.nombre_largo") %>%
    html_text()
)


# posiciones y broker -----------------------------------------------------


pos <- c("Bases", "Aleros", "Pívots")

recPosjug <- function(pos) {
  url <- "https://www.rincondelmanager.com/smgr/valoracion.php"
  pgsession <- session(url)
  pgform <- html_form(pgsession)[[5]]
  filled_form <- html_form_set(pgform,
                               "de_jor" =  "18",
                               "a_jor" = "25",
                               "cancha"  =  "2"
  )

  df <- session_submit(pgsession, form = filled_form, POST = url)
  df <- df %>%
    html_element(glue::glue("table#t{pos}")) %>%
    html_table(dec = ",") |>
    mutate(Broker = as.numeric(str_replace_all(Broker, "\\.", ""))) |>
    janitor::clean_names() |>
    mutate(eq = case_when(
      eq == "CBG" ~ "COV",
      eq == "OBR" ~ "MOB",
      eq == "RMA" ~ "RMB",
      eq == "BAS" ~ "BKN",
      eq == "FCB" ~ "BAR",
      eq == "CAN" ~ "LNT",
      eq == "ZAR" ~ "CAZ",
      eq == "BLB" ~ "SBB",
      eq == "MUR" ~ "UCM",
      eq == "MAN" ~ "BAX",
      TRUE ~ eq
    ), media_sm = as.numeric(media_sm),
    pos = paste0(pos))


  return(df)
}

recPosjug_df <- map_df(pos, recPosjug)

posicion <- recPosjug_df %>% select(license_licenseAbbrev = jug,eq, broker, pos) %>%
  left_join(tm1, by = c("eq"="team_team_abbrev_name")) %>%
  filter(team_team_actual_name  %in%  jor$local)



# valoracion recibida por posición ----------------------------------------

#tabla local para unir con la tabla visitante

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


recPosloc_df <- map_df(pos, recPosloc)

#tabla visitante para unir con la tabla local

local <- jor %>% select(local) %>%
  left_join(tm1 %>% select(team_team_actual_name, team_team_abbrev_name ), by =c("local" ="team_team_actual_name"))

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

recPosvis_df <- map_df(pos, recPosvis)


vis_df <- recPosvis_df %>%
  select(Equipo, Media, pos, tipo) %>%
  janitor::clean_names() %>%
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
  )) %>%left_join(tm1, by = c("abb1"="team_team_abbrev_name")) %>%
  filter( team_team_actual_name  %in% jor$visitante)



# data wrangling ----------------------------------------------------------



df <- pbp_acb_23 %>%
  select(id_match, matchweek_number,
         abb = team_team_abbrev_name, license_licenseStr15, license_licenseAbbrev, license_licenseNick, id_play = id_playbyplaytype, type_description,
         type_normalized_description, local, period, minute, second, crono, score_local, score_visitor, wall_clock
  ) %>%
  mutate(
    player_asist = ifelse(str_detect(type_description, "Asistencia"), license_licenseStr15, "0"),
    player_shoot = ifelse(str_detect(type_description, "Asistencia"), lag(license_licenseStr15), "0"),
    asist_type = ifelse(str_detect(type_description, "Asistencia"), lag(type_description), "0"),
    player_points = case_when(
      type_normalized_description == "2-Point Shot Made" ~ 2,
      type_normalized_description == "Dunk" ~ 2,
      type_normalized_description == "3-Point Shot Made" ~ 3,
      type_normalized_description == "Free Throw Made" ~ 1,
      TRUE ~ 0
    ),
    msg_type = case_when(
      type_description %in% c("Intento fallado de 2", "Intento fallado de 3", "Mate fuera") ~ 1,
      type_description %in% c("Canasta de 2", "Canasta de 3", "Mate") ~ 2,
      type_description %in% c("Intento fallado de 1", "Canasta de 1") ~ 3,
      TRUE ~ 0
    ),
    three_make = ifelse(type_normalized_description == "3-Point Shot Made", 1, 0),
    three_misses = ifelse(type_normalized_description == "3-Point Shot Missed", 1, 0),
    two_make = ifelse(type_normalized_description == "2-Point Shot Made", 1, 0),
    two_misses = ifelse(type_normalized_description == "2-Point Shot Missed", 1, 0),
    free_make = ifelse(type_normalized_description == "Free Throw Made", 1, 0),
    free_misses = ifelse(type_normalized_description == "Free Throw Missed", 1, 0),
    dunk = ifelse(type_normalized_description == "Dunk", 1, 0),
    dunk_misses = ifelse(type_normalized_description == "Missed Dunk", 1, 0),
    valoracion = case_when(
      type_normalized_description == "2-Point Shot Made" ~ 2,
      type_normalized_description == "3-Point Shot Made" ~ 3,
      type_normalized_description == "Free Throw Made" ~ 1,
      type_normalized_description == "Free Throw Missed" ~ -1,
      type_normalized_description == "2-Point Shot Missed" ~ -1,
      type_normalized_description == "3-Point Shot Missed" ~ -1,
      type_normalized_description == "Defensive Rebound" ~ 1,
      type_normalized_description == "Block Received" ~ -1,
      type_normalized_description == "Assist 3-Point Shot" ~ 1,
      type_normalized_description == "Block" ~ 1,
      type_normalized_description == "Unsportsmanlike 2FT" ~ -1,
      type_normalized_description == "Double Foul - No FT" ~ -1,
      type_normalized_description == "Foul No FT" ~ -1,
      type_normalized_description == "Offensive Rebound" ~ 1,
      type_normalized_description == "Assist 2-Point Shot" ~ 1,
      type_normalized_description == "Offensive Foul" ~ -1,
      type_normalized_description == "Foul Received" ~ 1,
      type_normalized_description == "Foul 2FT" ~ -1,
      type_normalized_description == "Turnover" ~ -1,
      type_normalized_description == "Steal" ~ 1,
      type_normalized_description == "Foul 1FT" ~ -1,
      type_normalized_description == "Foul 3FT" ~ -1,
      type_normalized_description == "Technical Foul 1FT" ~ -1,
      type_normalized_description == "Double Unsportsmanlike - No FT" ~ -1,
      type_normalized_description == "Assist Foul Received" ~ 1,
      type_normalized_description == "Missed Dunk" ~ -1,
      type_normalized_description == "Dunk" ~ 2,
      TRUE ~ 0
    ),
    reboundOff = ifelse(type_normalized_description == "Offensive Rebound", 1, 0),
    reboundDef = ifelse(type_normalized_description == "Defensive Rebound", 1, 0),
    rebounds = ifelse(type_normalized_description  %in%  c("Defensive Rebound","Offensive Rebound"), 1,0),
    asistencias =ifelse(str_detect(type_normalized_description, "Assist"),1,0),
    pts_diff = abs(c(score_local - score_visitor))
  )

c("Monbus Obradoiro", "Coviran Granada", "Río Breogán", "Casademont Zaragoza", "Bàsquet Girona", "Real Betis Baloncesto", "Surne Bilbao Basket", "Joventut Badalona", "Cazoo Baskonia")


dfval <- df|>
  filter (!license_licenseStr15  %in%  c("Juan Fernández", "Dragan Bender", "Shannon Evans", "Jean Montero", "BJ Johnson III", "Volodymyr Gerun", "Jeff Withey", "Ludde Hakanson")) |>
  rename(tm =abb) |>
  group_by(id_match, tm,license_licenseAbbrev ) |>
  summarise(val = sum(valoracion),
            .groups = "drop") |>
  ungroup() %>%
  left_join(iddf_df) %>%
  filter(tm==team, tipo =="home", matchweek_number >17)|>
  mutate(val_sm = ifelse(win == 1, abs(val*.20) + val,  val)) %>%
  group_by(license_licenseAbbrev) %>%
  summarise(local_media=sum(val_sm)) %>%
  left_join(partidos) %>%
  left_join(tm) %>%
  drop_na() %>%
  mutate(val = local_media/n) %>%
  filter(team_team_actual_name  %in%  jor$local) %>%
  arrange(desc(val)) %>%
  left_join(posicion)|>
  group_by(pos)  %>%
  drop_na() %>%
  arrange(desc(val)) |>
  slice(1:5) |>
  ungroup() |>
  arrange(match(pos, c("Bases", "Aleros", "Pívots"))) |>
  left_join(jor, by =c("team_team_actual_name" = "local")) %>%
  left_join(vis_df, by =c("visitante" = "team_team_actual_name")) %>%
  pivot_longer(contains("media_player1_"), names_prefix = "media_player1_", values_to = "val2") %>%
  filter(name ==pos) %>%
  select(1,2,3,4, 6,8,9,11,13,14) %>%
  left_join(tm_acb, by =c("team_team_abbrev_name" ="tm")) %>%
  left_join(tm_acb, by =c("abb1" ="tm")) %>%
  left_join(face) %>%
  mutate(foto = ifelse(license_licenseAbbrev == "S. Bamforth", "https://static.acb.com/media/PRO/00/00/46/58/87/0000465887_5-6_03.jpg", foto))



dfval1 <- df|>
  rename(tm =abb) |>
  group_by(id_match, tm,license_licenseAbbrev ) |>
  summarise(val = sum(valoracion),
            .groups = "drop") |>
  ungroup() %>%
  left_join(iddf_df) %>%
  filter(tm==team, tipo =="away", matchweek_number >17)|>
  mutate(val_sm = ifelse(win == 1, abs(val*.20) + val,  val)) %>%
  arrange(matchweek_number) %>%
  group_by(license_licenseAbbrev) %>%
  summarise(local_media=mean(val_sm)) %>%
  arrange(desc(local_media)) %>%
  left_join(partidos)



