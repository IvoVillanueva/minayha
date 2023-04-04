


# librerias ---------------------------------------------------------------


library(tidyverse)
library(rvest)
library(ggtext)
library(janitor)
library(magick)
library(grid)


# theme ivo
theme_ivo <- function() {
  library(ggtext)
  theme_minimal(base_size = 9, base_family = "Roboto Condensed") %+replace%
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
      plot.caption = element_markdown(size = 5,  color = "#626060")
      #margin = unit(c(.5,0,0.5,0), "cm"))
    )
}



# datos -------------------------------------------------------------------

# tabla de antes de la jornada(no tocar)
url <- "https://www.rincondelmanager.com/smgr/valoracion.php"
pgsession <- session(url)
pgform <- html_form(pgsession)[[5]]
filled_form <- html_form_set(pgform,
  "de_jor" = "1",
  "a_jor" = "25",
  "cancha" = "2"
)

df1 <- session_submit(
  x = pgsession,
  form =  filled_form,
  POST = url
) %>%
  html_element("table#tTodos") %>%
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
  ))

write_csv(df1, "tablajornada4abril.csv")
dfcsv <- read.csv("tablajornada4abril.csv") |>
  select(jug, eq, broker)



# tabla del lunes ---------------------------------------------------------

# tabla de despues de la jornada(no tocar)
url <- "https://www.rincondelmanager.com/smgr/valoracion.php"
pgsession <- session(url)
pgform <- html_form(pgsession)[[5]]
filled_form <- html_form_set(pgform,
  "de_jor" = "1",
  "a_jor" = "22",
  "cancha" = "2"
)

df3 <- submit_form(
  session = pgsession,
  form = filled_form,
  POST = url
) %>%
  html_element("table#tTodos") %>%
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
  ))
write_csv(df3, "tablajornada14.csv")



dfcsv <- read.csv("tablajornada28marzo.csv") |>
  select(jug, eq, broker) |>
 mutate(broker=ifelse(broker == 7605, 275000,broker),
        broker = ifelse(jug == "Y. Ndoye" & broker == 590750, 382500, broker))

dfcsv1 <- read.csv("tablajornada4abril.csv") |>
  select(jug, eq, broker1 = broker) |>
  left_join(dfcsv) |>
  mutate(
    broker = ifelse(is.na(broker), 0, broker),
    dif = ifelse( broker == 0,0, broker1-broker)
  ) |>
  group_by(eq) |>
  summarise(tot = sum(dif)) |>
  arrange(desc(tot))

dfcsv2 <- read.csv("tablajornada4abril.csv") |>
  select(jug, eq, broker1 = broker) |>
  left_join(dfcsv) |>
  mutate(
    broker = ifelse(is.na(broker), 0, broker),
    dif = ifelse( broker == 0,0, broker1-broker),
    jug = ifelse(jug == "J. Fernández" & eq == "FUE", "Juan Fernández", jug)
  ) |>
  filter(!is.na(dif)) |>
  left_join(dfcsv1)

options(scipen = 999)
library(cowplot)

mosca <- magick::image_read("https://raw.githubusercontent.com/IvoVillanueva/acb_logo/main/Logo%20SM%20mosca%20340x340.png")
image_write(mosca, path = "mosca.png", format = "png")
image_convert(mosca)
logo <- read.csv("logos2023.csv") |>
  select(eq = abb, equipo, logo)

path <- here::here("valdirecto", "broker_25" )




link_to_img <- function(x, width = 20) {
  # Define the logo link as src attribute to
  # html img element
  glue::glue("<img src='{x}' width='{width}'/>")
}

p23 <- dfcsv2 |>
  left_join(logo) |>
  mutate( team = paste0(link_to_img(logo),'<br>', eq)) |>
  group_by(eq) |>
  ggplot(aes(y = reorder(jug, dif), x = dif, fill = dif)) +
  geom_col() +
  # ggimage::geom_image(aes(
  #   x = 75000, y = 3,
  #   image = logo
  # ),
  # size = .2,
  # by = "width", asp = 1.618
  # ) +
   scale_fill_gradient2(high = "#05CE1C", mid = "#F4CB25", low = "#F43725") +
  scale_x_continuous(limits = c(min(dfcsv2$dif), max(dfcsv2$dif))) +
  facet_wrap(~ fct_reorder(paste0(team, " ", tot, "€"), -tot), scales = "free", nrow = 6) +
  theme_minimal() +
  coord_cartesian(clip = "off") +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    legend.position = "none",
    strip.text.x = element_markdown(face = "bold", size = 4.5, hjust = 0.5),
    panel.spacing.x = unit(1, "lines"),
    plot.title.position = "plot",
    plot.title = element_markdown(face = "bold", size = 20, hjust = .5),
    plot.subtitle = element_markdown(size = 20 / 1.618, hjust = .5, color = "#8C8C8C"),
    axis.title.y = element_text(size = 10, face = "bold"),
    axis.title.x = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 3.5),
    axis.text.y = element_text(size = 3.5),
    panel.grid.major.x = element_line(size = .1),
    panel.grid.major.y = element_line(size = .1),
     plot.caption = element_markdown(size = 5, hjust = .5, color = "#626060"),
     panel.grid.minor = element_blank(),
    plot.margin = unit(c(2, 2, 2, 2), "lines")
  ) +
  labs(
    title = "Subidas y Bajadas BrokerBasket Por Equipos",
    subtitle = "En la J25\n\n",
    x = "Broker",
    y = "",
    caption = "<br> **Datos** *@elrincondelsm* **Gráfico** *Ivo Villanueva* &bull;  <span style='color:#00acee; font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span> **@elcheff**"
  )
#
# image <- image_fill(mosca, 'none')
# raster <- as.raster(image)
# p23 + annotation_raster(raster, -0.47, -.30, 0.47, .30)

p24 <- ggdraw(p23) +
  theme(
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  draw_image(mosca, x = -0.45, y = 0.45, scale = .08)


  ggsave(p24, filename = glue::glue("{path}.png"), w = 8, h = 8 * 1.23, dpi = 320)


# todos los jugadores-----------
#
# p25 <-  p2|>
#   left_join(logo) |>
#   ggplot(aes(y = reorder(jug, broker), x= broker, fill=broker )) +
#
#   geom_col()+
#   geom_text(aes(label = broker, hjust = if_else(broker > 0, -0.1, 1.1)), size = 1)+
#   scale_fill_gradient2(high = "#05CE1C",mid = "#F4CB25", low ="#F43725") +
#   scale_x_continuous(limits = c(min(p2$broker), max(p2$broker)))+
#   theme_ivo() +
#   coord_cartesian(clip = "off") +
#   theme(
#     legend.position = "none",
#     strip.text.x = element_text(face = "bold", size = 4.5, hjust = 0.5),
#     panel.spacing.x = unit(1, "lines"),
#     plot.title.position = "plot",
#     plot.title = element_markdown(face = 'bold', size = 16, hjust = .5),
#     plot.subtitle = element_markdown( size = 16/1.618, hjust = .5),
#     axis.title.y = element_text(size = 12, face = "bold"),
#     axis.title.x = element_text(size = 10, face = "bold"),
#     axis.text.x = element_text(size = 3.5),
#     axis.text.y = element_text(size = 3.5),
#     panel.grid.major.x = element_line(size = .1),
#     panel.grid.major.y = element_line(size = .1),
#     plot.margin = unit(c(.5, .5, 1, .5), "lines")
#   ) +
#   labs(title = "Ranking por jugador BrokerBasket",
#        subtitle = "En la J1",
#        x = "Broker",
#        y = "",
#        caption = "<br>**Datos** @elrincondelsm &nbsp;&nbsp; **Gráfico**  *Ivo Villanueva* &bull;  <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span> **@elcheff**")
#
#
#   image <- image_fill(mosca, 'none')
#   raster <- as.raster(image)
#   p + annotation_raster(raster, 106,115,  20.7, 22.5)
#
#
# ggsave(p25, filename = "broker25.png", w = 6, h = 6*1.618, dpi = "retina")
#
