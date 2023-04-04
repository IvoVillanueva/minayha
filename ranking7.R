


# Librerias ---------------------------------------------------------------




library(rvest)
library(tidyverse)
library(httr)
library(gt)
library(gtExtras)

# datos -------------------------------------------------------------------



logos <- read.csv("https://raw.githubusercontent.com/IvoVillanueva/csv2023/main/logos2023.csv") |>
  select (eq = abb, color, logo)


caras <- read.csv(here::here("PlayByPlay", "faces.csv")) |>
  filter(!str_detect(foto,".gif")) |>
  mutate( abb =ifelse(nombre == "Shannon Evans", "VBC", abb),
          nombre_corto =ifelse(nombre_corto == "J. Fernández" & abb == "FUE", "J. Fernández.", nombre_corto)) |>
  rename(jugador = nombre)



path <- here::here("acb2023/jornada18/jornada25")

# scrap data --------------------------------------------------------------


clasificacion <- "https://www.acb.com/resultados-clasificacion/ver/temporada_id/2022/competicion_id/1/jornada_numero/25" |>
  read_html() |>
  html_element("table") |>
  html_table() |>
  janitor::clean_names() |>
  mutate(equipo = substr(equipo, 1, 3)) |>
  select(eq = equipo, v, d)



jor <- c(1:25)

jornada <- function(jor) {
  url <- "https://www.rincondelmanager.com/smgr/valoracion.php"
  pgsession <- session(url)
  pgform <- html_form(pgsession)[[5]]
  filled_form <- html_form_set(pgform,
                               "de_jor" = "1",
                               "a_jor" = jor,
                               "cancha" = "2"
  )

  df <- session_submit(pgsession, form = filled_form, POST = url)
  df <- df %>%
    html_element(glue::glue("table#tTodos")) %>%
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
      TRUE ~ eq))|>
    pivot_longer(names_to = "jornada",
                 values_to = "val",
                 cols =c(paste0("j",jor):paste0("j",jor))) |>
    arrange(desc(jornada))


  return(df)
}




jor_df <- map_df(jor, jornada)

parj <- "https://www.rincondelmanager.com/smgr/avanzadas.php" |>
  read_html() |>
  html_element("table#tTodos") |>
  html_table() |>
  janitor::clean_names() |>
  select(jugador, pj)



# Wrangling datos ---------------------------------------------------------



jor_df1 <- jor_df |>
  filter(jornada == "j25") |>
  left_join(parj) |>
  select(jugador, jug, eq,media_sm, broker, ef, rent, reg, pj)

jdf <- jor_df|>
  select(jugador:val) %>%
  filter(jugador %in% unique(jor_df$jugador)) %>%
  group_by(jugador, eq) %>%
  summarise(val_wsem = list(val), .groups = "drop") |>
  left_join(clasificacion) |>
  left_join(logos)

jordf <- jor_df %>%
  group_by(jugador) %>%
left_join(parj) |>
  mutate(val= ifelse(is.na(val),0,val)) |>
  filter(!jugador  %in% c(  "Dragan Bender" )) |>
  mutate(mean_val = sum(val)/pj) %>%
  ungroup() %>%
  select(jornada,  jugador, val, mean_val, pj) %>%
  filter(jornada != "j25") %>%
  filter(jugador %in% jdf$jugador) %>%
  group_by(jugador) %>%
  summarise(prev_val =sum(val), mean_val = unique(mean_val)) %>%
  left_join(parj) |>
  drop_na() |>
  mutate(
    prev_val = prev_val/pj,
    prev_week = rank(-prev_val),
    rank = rank(-mean_val)
  ) %>%
  mutate(rank_chg = prev_week-rank) %>%
  ungroup() %>%
  arrange(desc(mean_val)) %>%
  select(jugador, val = mean_val, rank_chg, rank) |>
  slice(1:15) |>
  left_join(jdf) |>
  left_join(caras) |>
  left_join(jor_df1) |>
  distinct() |>
  mutate(wl = glue::glue("{v}-{d}"))

font <- "Roboto Condensed"
# gt table ----------------------------------------------------------------


combine_word <- function(jug, eq, logo, wl){
  glue::glue(
    "<div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;font-size:14px'>{jug}</div>
        <div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>{eq}&nbsp;&nbsp;{wl}</span>
    <img src='{logo}'style='width:12px; height:11px;vertical-align:middle;horizontal-align:left'</span></div></div>"
  )
}

jordf %>%
  mutate(
    val = round(val, 2),
    rank = row_number(),
    combo = combine_word(jug, eq, logo, wl),
    combo = map(combo, gt::html),
    foto = ifelse(foto == "//static.acb.com/img/www/jugadores2022/TomicCara.jpg", "https://static.acb.com/img/www/jugadores2022/TomicCara.jpg",foto ),
    foto = ifelse(jugador == "Jasiel Rivero", "https://static.acb.com/media/PRO/00/00/46/89/57/0000468957_5-6_03.jpg", foto),
    foto = ifelse(jugador == "Guerschon Yabusele", "https://static.acb.com/media/PRO/00/00/46/77/41/0000467741_5-6_03.jpg", foto)
  ) %>%
  select(rank, rank_chg, foto, combo, val, val_wsem, broker, ef, rent, reg) |>
  gt() %>%
  cols_align(
    align = "left",
    columns = c(combo)
  ) %>%
  tab_options(
    data_row.padding = px(2)
  ) %>%
  gt_img_circle(foto, height = 33, border_color = jordf$color, border_weight = .86) %>%
  text_transform(
    locations = cells_body(columns = c(rank_chg)),
    fn = function(x){

      rank_chg <- as.integer(x)

      choose_logo <-function(x){
        if (x == 0){
          gt::html(fontawesome::fa("equals", fill = "grey"))
        } else if (x > 0){
          gt::html(glue::glue("<span style='color:#1134A6;font-face:bold;font-size:10px;'>{x}</span>"), fontawesome::fa("arrow-up", fill = "#1134A6"))
        } else if (x < 0) {
          gt::html(glue::glue("<span style='color:#DA2A2A;font-face:bold;font-size:10px;'>{x}</span>"), fontawesome::fa("arrow-down", fill = "#DA2A2A"))
        }
      }

      map(rank_chg, choose_logo)

    }
  ) %>%
  gt_plt_sparkline(palette = c("black", "black", "#ff0000", "#00bb2d", "#ffa031"), val_wsem, type = "shaded") |>
  cols_label(
    rank = "RK",
    rank_chg = "",
    foto ="",
    combo = "Jugador",
    val = "Val M.",
    val_wsem = "historico",
    broker = "broker",
    ef = "ef",
    rent = "rent",
    reg = "reg"
  ) %>%
  espnscrapeR::gt_theme_espn() |>
  tab_options(heading.align  = "center",
              table.font.names = font,
              table.background.color = "white",
              table.font.size = 12,
              data_row.padding = px(1),
              source_notes.font.size = 10
  ) |> tab_header(
    title = md("<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Liga_Endesa_2019_logo.svg/800px-Liga_Endesa_2019_logo.svg.png'
               style='height:40px;'><img src='https://raw.githubusercontent.com/IvoVillanueva/acb_logo/main/Logo%20SM%20mosca%20340x340.png'
               style='height:40px;'><br><span style='font-weight:bold;font-dystopian:small-caps;font-size:24px'>Top 15 en Valoración Media Supermanager</span>"),
    subtitle = md("<span style='font-weight:400;color:#8C8C8C;font-size:12px'>Ranking de la j25 con respecto a la j24<br><br><br></span>")
  )  %>%
  fmt_currency(columns = c(broker),  currency = "EUR", use_subunits = FALSE
  ) %>%
  tab_source_note( source_note = gt::html("<b>Gráfico</b>: <i>Ivo Villanueva<i> &bull;  <span style='color:#00acee;font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span> <b>@elcheff</b><br><b>
  Datos por</b> : \n<i>@elrincondelsm<i>&nbsp;&nbsp; <img src='https://www.rincondelmanager.com/smgr/images/logo.png'
                     style='height:12px;', *@ACBCOM*&nbsp;&nbsp;<img src='https://upload.wikimedia.org/wikipedia/commons/3/3f/Acb_2019_logo.svg'
                     style='height:12.5px;'><br>"

  ) )%>%
  cols_align(
    "center",
    columns = c(val, broker:reg)
  ) %>%
  cols_width(
    colums = c(rank) ~ px(25),
    colums = c(foto, val) ~ px(40),
    colums = c(combo) ~ (110),
    colums = c(rank_chg,ef, rent, reg) ~ px(35)

  )|>
  gtsave("jor25Rk.png", expand =50)
