
library(tidyverse)
library(rvest)
library(janitor)
library(ggtext)
library(gt)
library(gtExtras)
library(sysfonts)
library(showtext)

"F8B229"

font_add('fs', '/Users/ivo/Library/Fonts/Font Awesome 6 Free-Solid-900.otf')
sysfonts::font_add('fb', 'Users/ivo/Library/Fonts/Font Awesome 6 Free-Regular-400.otf')

font_files() |> tibble() |> filter (str_detect(family, "Roboto Condensed"))

tm_acb <- read.csv("https://raw.githubusercontent.com/IvoVillanueva/csv2023/main/logos2023.csv") %>% #data of logos colors and names
  select(tm = abb, logo, color, equipo) |>
  drop_na()
font <- "Roboto Condensed"

caras <- read.csv("jugadores_2023.csv") |>
  select(jugador = nombre, foto)|>
  mutate( foto =ifelse(!str_detect(foto, "https:"), paste0("https:", foto), foto))

clasificacion <- "https://www.acb.com/resultados-clasificacion/ver/temporada_id/2022/competicion_id/1/jornada_numero/24" |>
  read_html() |>
  html_element("table") |>
  html_table() |>
  janitor::clean_names() |>
  mutate(equipo = substr(equipo, 1, 3)) |>
  select(tm = equipo, v, d) |>
  mutate(wl = glue::glue("{v}-{d}")) |>
  select(tm, wl)


iddf_df <- read.csv("PlayByPlay/iddf_df23.csv") |>
  filter(id_phase == 107) |>
  select(id_match = id, id_phase, local_team = local_team_team_abbrev_name, visitor_team = visitor_team_team_abbrev_name, score_local, score_visitor, matchweek_number)


# extraer calendario por equipo -------------------------------------------


url <- "https://www.acb.com/calendario/index/temporada_id/2022/edicion_id/967" %>%
  read_html()

rival1 <- tibble(
  local = url %>% html_elements("div:nth-of-type(52) .local span.nombre_largo") %>%
    html_text(),
  visitante = url %>% html_elements("div:nth-of-type(52) .visitante span.nombre_largo") %>%
    html_text()
) |> pivot_longer(cols = c(local, visitante)) |>
  mutate(rival = "rival1")

rival2 <- tibble(
  local = url %>% html_elements("div:nth-of-type(54) .local span.nombre_largo") %>%
    html_text(),
  visitante = url %>% html_elements("div:nth-of-type(54) .visitante span.nombre_largo") %>%
    html_text()
)|> pivot_longer(cols = c(local, visitante)) |>
  mutate(rival = "rival2")

rival3 <- tibble(
  local = url %>% html_elements("div:nth-of-type(56) .local span.nombre_largo") %>%
    html_text(),
  visitante = url %>% html_elements("div:nth-of-type(56) .visitante span.nombre_largo") %>%
    html_text()
) |> pivot_longer(cols = c(local, visitante)) |>
  mutate(rival = "rival3")

rival4 <- tibble(
  local = url %>% html_elements("div:nth-of-type(58) .local span.nombre_largo") %>%
    html_text(),
  visitante = url %>% html_elements("div:nth-of-type(58) .visitante span.nombre_largo") %>%
    html_text()
)|> pivot_longer(cols = c(local, visitante)) |>
  mutate(rival = "rival4")

rival5 <- tibble(
  local = url %>% html_elements("div:nth-of-type(60) .local span.nombre_largo") %>%
    html_text(),
  visitante = url %>% html_elements("div:nth-of-type(60) .visitante span.nombre_largo") %>%
    html_text()
)|> pivot_longer(cols = c(local, visitante)) |>
  mutate(rival = "rival5")

calen <- rbind(rival1, rival2, rival3, rival4, rival5) |>
  left_join(tm_acb, by= c(value = "equipo")) |>
  select(name, tm, rival) |>
  filter(rival == "rival1")

calen2 <- rbind(rival1, rival2, rival3, rival4, rival5) |>
  left_join(tm_acb, by= c(value = "equipo")) |>
  select(name2 = name, tm2 = tm, rival_2 = rival) |>
  filter(rival_2 == "rival2")

calen3 <- rbind(rival1, rival2, rival3, rival4, rival5) |>
  left_join(tm_acb, by= c(value = "equipo")) |>
  select(name3 = name, tm3 = tm, rival_3 = rival) |>
  filter(rival_3 == "rival3")

calen4 <- rbind(rival1, rival2, rival3, rival4, rival5) |>
  left_join(tm_acb, by= c(value = "equipo")) |>
  select(name4 = name, tm4 = tm, rival_4 = rival) |>
  filter(rival_4 == "rival4")

calen5 <- rbind(rival1, rival2, rival3, rival4, rival5) |>
  left_join(tm_acb, by= c(value = "equipo")) |>
  select(name5 = name, tm5 = tm, rival_5 = rival) |>
  filter(rival_5 == "rival5")



url <- "https://www.proballers.com/es/baloncesto/liga/30/spain-liga-endesa/equipos" |>
  read_html()


cal <- tibble(
  calendario = url |> html_elements("a[title='Calendário']") |>
    html_attr("href")
) |>
  mutate(
    calendario = paste0("https://www.proballers.com", calendario),
    cod = str_extract(calendario, "[0-9]+"),
    eq = case_when(
      cod == 155 ~ "BKN",
      cod == 13356 ~ "GIR",
      cod == 405 ~ "BAX",
      cod == 671 ~ "CAZ",
      cod == 158 ~ "BET",
      cod == 218 ~ "COV",
      cod == 148 ~ "BAR",
      cod == 159 ~ "FUE",
      cod == 157 ~ "GCA",
      cod == 154 ~ "JOV",
      cod == 917 ~ "LNT",
      cod == 1986 ~ "MOB",
      cod == 160 ~ "RMB",
      cod == 149 ~ "BRE",
      cod == 844 ~ "SBB",
      cod == 457 ~ "UCM",
      cod == 161 ~ "UNI",
      cod == 153 ~ "VBC",
      TRUE ~ cod
    )
  )



p <- cal$calendario

rival <- function(p) {
  url <- p |>
    read_html()
  df <- tibble(
    logo = url |> html_elements(".show img") |>
      html_attr("src"),
    m = url |> html_elements(".show a[itemprop='name']") |>
      html_text("img")
  ) |>
    mutate(equip = str_extract(logo, "[0-9]+"),
           m =str_squish(m),
           m=ifelse(str_detect(m,"vs"), "home", "away"))
  df$equip2 <- p
  return(df)
}

pdf <- map_df(p, rival)



calen_futur1 <- pdf |>
  mutate(equip2 = str_extract(equip2, "[0-9]+")) |>
  group_by(equip2) |>
  slice(25) |>
  left_join(cal |> select(cod, tm = eq), by = c("equip2" = "cod")) |>
  left_join(cal |> select(cod, tm1=eq), by = c("equip" = "cod")) |>
  ungroup() |>
 select(m, tm, tm1)

calen_futur2 <- pdf |>
  mutate(equip2 = str_extract(equip2, "[0-9]+")) |>
  group_by(equip2) |>
  slice(26) |>
  left_join(cal |> select(cod, tm = eq), by = c("equip2" = "cod")) |>
  left_join(cal |> select(cod, tm1=eq), by = c("equip" = "cod")) |>
  ungroup() |>
  select(m, tm, tm2=tm1)

calen_futur3 <- pdf |>
  mutate(equip2 = str_extract(equip2, "[0-9]+")) |>
  group_by(equip2) |>
  slice(27) |>
  left_join(cal |> select(cod, tm = eq), by = c("equip2" = "cod")) |>
  left_join(cal |> select(cod, tm1=eq), by = c("equip" = "cod")) |>
  ungroup() |>
  select(tm, tm3=tm1)

calen_futur4 <- pdf |>
  mutate(equip2 = str_extract(equip2, "[0-9]+")) |>
  group_by(equip2) |>
  slice(28) |>
  left_join(cal |> select(cod, tm = eq), by = c("equip2" = "cod")) |>
  left_join(cal |> select(cod, tm1=eq), by = c("equip" = "cod")) |>
  ungroup() |>
  select(tm, tm4=tm1)

calen_futur5 <- pdf |>
  mutate(equip2 = str_extract(equip2, "[0-9]+")) |>
  group_by(equip2) |>
  slice(29) |>
  left_join(cal |> select(cod, tm = eq), by = c("equip2" = "cod")) |>
  left_join(cal |> select(cod, tm1=eq), by = c("equip" = "cod")) |>
  ungroup() |>
  select(tm, tm5=tm1)

# valoracion media por jugador ------------------------------


pos <- c("Bases", "Aleros", "Pívots")

recPosjug <- function(pos) {
  url <- "https://www.rincondelmanager.com/smgr/valoracion.php"
  pgsession <- session(url)
  pgform <- html_form(pgsession)[[5]]
  filled_form <- html_form_set(pgform,
                               "de_jor" =  "20",
                               "a_jor" = "24",
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


# valoracion de equipo recibida como local --------------------------------


pos <- c("Bases", "Aleros", "Pívots")

recPosloc <- function(pos) {
  url <- "https://www.rincondelmanager.com/smgr/valoracion_rec.php"
  pgsession <- session(url)
  pgform <- html_form(pgsession)[[5]]
  filled_form <- html_form_set(pgform,
                               "de_jor" = "20",
                               "a_jor" = "24",
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


# valoracion de equipo recibida como visitante ----------------------------


recPosvis <- function(pos) {
  url <- "https://www.rincondelmanager.com/smgr/valoracion_rec.php"
  pgsession <- session(url)
  pgform <- html_form(pgsession)[[5]]
  filled_form <- html_form_set(pgform,
                               "de_jor" = "20",
                               "a_jor" = "24",
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

#union local y visitante

recPosloc_df <- map_df(pos, recPosloc)
recPosvis_df <- map_df(pos, recPosvis)

rec <- rbind(recPosloc_df,recPosvis_df ) |>
select(abb = Equipo, Media, pos, tipo) %>%
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
  ))



df <-  recPosjug_df  |>
  filter (!jugador  %in%  c("Juan Fernández", "Dragan Bender", "Shannon Evans", "Jean Montero", "BJ Johnson III", "Volodymyr Gerun", "Jeff Withey" )) |>
  group_by(pos)  %>%
  replace(is.na(.), 0) %>%
  arrange(desc(media_sm)) |>
  slice(1:5) |>
  ungroup() |>
  arrange(match(pos, c("Bases", "Aleros", "Pívots"))) |>
  select(jugador, jug:media_sm, ef:reg,pos) |>
  rename(tm =eq) |>
  left_join(clasificacion) |>
  left_join(caras) |>
  left_join(calen)|>
  left_join(calen_futur1) |>
  left_join(rec, by = c("tm1" = "abb")) |>
  filter(pos.x == pos.y , tipo != name) |>
  select( -name, -pos.y, -rival) |>
  left_join(calen_futur2) |>
  left_join(calen2) |>
  left_join(rec, by = c("tm2" = "abb")) |>
  # left_join(calen_futur3) |>
  # left_join(calen3)  |>
  # # left_join(rec, by = c("tm3" = "abb")) |>
  filter(pos == pos.x , tipo.y != name2) |>
  select(jugador:name2,Media.y) |>
  left_join(tm_acb) |>
  left_join(tm_acb |> select( tm1 = tm, logo1 = logo)) |>
  left_join(tm_acb |> select( tm2 = tm, logo2 = logo)) %>%
  separate(Media.x, c("media_total", "media_player1"), sep = "( )") %>%
  mutate_at(vars("media_total", "media_player1"), list(~ str_replace(., ",", "."))) %>%
  mutate(
    media_player1 = as.numeric(str_remove_all(media_player1, "[( )]"))) %>%
  separate(Media.y, c("media_total2", "media_player2"), sep = "( )") %>%
  mutate_at(vars("media_total2", "media_player2"), list(~ str_replace(., ",", "."))) %>%
  mutate(
    media_player2 = as.numeric(str_remove_all(media_player2, "[( )]"))) |>
  select(-contains("_total"))



combine_word <- function(jug, tm, logo, wl) {
  glue::glue(
    "<div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;horizontal-align:left;font-size:13px'>{jug}</span></div>
        <div style='font-size:12px;line-height:18px;vertical-align:middle;horizontal-align:left'><span style ='font-weight:bold;color:grey;font-size:10px'><img src='{logo}'
    style='width:11px; height:13px;vertical-align:middle;horizontal-align:left'> {wl} {tm}</span></div>"
  )
}

path <- here::here("acb2023","especial_navidad", "afavorencontra_23_gt")

df %>%

  mutate( combo = combine_word(jug, tm, logo, wl),
          combo = map(combo, gt::html),
          foto = ifelse(jugador == "Jasiel Rivero", "https://static.acb.com/media/PRO/00/00/46/89/57/0000468957_5-6_03.jpg", foto),
          foto = ifelse(jugador == "Guerschon Yabusele", "https://static.acb.com/media/PRO/00/00/46/77/41/0000467741_5-6_03.jpg", foto),
          foto = ifelse(jugador == "BJ Johnson III", "https://static.acb.com/media/PRO/00/00/46/73/47/0000467347_5-6_03.jpg", foto),
          foto = ifelse(jugador == "Jean Montero", "https://static.acb.com/media/PRO/00/00/48/74/11/0000487411_5-6_03.jpg", foto),
          tipo.x = ifelse(tipo.x=="local",  emoji::emoji("house"),emoji::emoji("flight_departure")),
          name2 = ifelse(name2=="visitante", emoji::emoji("flight_departure"), emoji::emoji("house"))
  ) %>%
  select(foto, combo, pos=pos.x, broker, media_sm, ef, rent, reg, logo1, media_player1, tipo.x, logo2, media_player2, name2 ) %>%
  gt(groupname_col = "pos") |>
  gt_img_circle(foto,  border_color = df$color, border_weight = .86) |>
  fmt_currency(columns = c(broker), placement = "right", currency = "EUR", use_subunits = FALSE
  ) |>
  gt_img_rows(columns = c(logo1)) |>
  gt_img_rows(columns = c(logo2)) |>
  data_color(
    columns = contains("_player"),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "Redmonder::dPBIRdGn",
        direction = 1
      ) %>% as.character(),
      domain = c(0, 14),
      na.color = "#005C55FF"
    )
  ) |>
  cols_label(
    foto = "",
    combo = "",
    broker = gt::html("<span style='font-weight:bold;font-size:12px'>💰br🌟ker💰</span>"),
    media_sm= gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:12px'>MED</span>"),
    ef= gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:12px'>EF</span>"),
    rent = gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:12px'>RENT</span>"),
    reg = gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:12px'>REG</span>"),
    logo1 = gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:12px'>j25</span>"),
    media_player1 = gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:12px'>Rec</span>"),
    logo2 = gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:12px'>j26</span>"),
    media_player2 = gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:12px'>Rec</span>"),
    tipo.x ="🏠o🛫",
    name2 ="🏠o🛫",
  ) %>%
  tab_spanner(gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:14px'>VAL. REC. Rivales j25 y j26</span>"),
              columns = c(logo1:name2)
  ) |>
  cols_align(
    align = "left",
    columns = c(combo)
  )  |>
  cols_align(
    align = "center",
    columns = c( -combo)
  ) %>%
  cols_width(
    c(foto) ~ px(36)
  )  |>
  data_color(
    columns = c(media_sm, ef), # summary(p1$defrating)
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "Redmonder::dPBIPuOr",
        direction = -1
      ) %>% as.character(),
      domain = c(0, 30),
      na.color = "#005C55FF"
    )
  )|>
  data_color(
    columns = c(rent), # summary(p1$defrating)
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "Redmonder::dPBIPuOr",
        direction = -1
      ) %>% as.character(),
      domain = c(0, 2.5),
      na.color = "#005C55FF"
    )
  )|>
  data_color(
    columns = c(reg), # summary(p1$defrating)
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "Redmonder::dPBIPuOr",
        direction = -1
      ) %>% as.character(),
      domain = c(0, 16),
      na.color = "#005C55FF"
    )
  ) |>
  gt_add_divider( columns = c(broker, reg,tipo.x), sides = "right",include_labels = FALSE ) |>


  espnscrapeR::gt_theme_espn() |>
  opt_row_striping(row_striping = FALSE) %>%
   tab_options(heading.align  = "center",

    table.font.names = font,
    table.background.color = "white",
    table.font.size = 12,
    data_row.padding = px(1),
    source_notes.font.size = 10,
    table.additional_css = ".gt_table {
                margin-bottom: 40px;
              }"
  ) |> tab_header(
    title = md("<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Liga_Endesa_2019_logo.svg/800px-Liga_Endesa_2019_logo.svg.png'
               style='height:40px;'><img src='https://raw.githubusercontent.com/IvoVillanueva/acb_logo/main/Logo%20SM%20mosca%20340x340.png'
               style='height:40px;'><br><span style='font-weight:bold;font-dystopian:small-caps;font-size:30px'>Top 15 Supermanager últimos 5 Partidos</span>"),
    subtitle = md("<span style='font-weight:400;color:#8C8C8C;font-size:15px'>Desde la J20 Hasta la j24 y sus enfrentamientos Para la J25 y la J26 <br><br><br></span>"))%>%
  tab_source_note(
    source_note = md(" **Datos**: *@elrincondelsm* &nbsp;&nbsp; <img src='https://www.rincondelmanager.com/smgr/images/logo.png'
                     style='height:12px;'>,  *@ACBCOM*&nbsp;&nbsp;<img src='https://upload.wikimedia.org/wikipedia/commons/3/3f/Acb_2019_logo.svg'
                     style='height:12.5px;'><br>
                     **Gráfico**: *Ivo Villanueva* &bull;  <span style='color:#00acee;font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span> <b>@elcheff</b>")) %>%
  gtsave("j25.png", expand = 50)
  gtsave(glue::glue("{path}.png", vwidth = 512, vheight = 512*1.5))


mosca3 <- magick::image_read("https://raw.githubusercontent.com/IvoVillanueva/acb_logo/main/Supermanager_Navidad400x400%20(1).png")
image_write(mosca3, path = "superManagerNavidad.png", format = "png")
image_convert(mosca3)

