library(rvest)
library(tidyverse)
library(janitor)
library(cropcircles)
library(ggimage)
library(ggtext)
# Diccionario jugadores euroliga ------------------------------------------

logos <- read.csv("https://raw.githubusercontent.com/IvoVillanueva/csv2023/main/logos2023.csv") |>
  select(team = equipo,logo, color,abb) |>
  drop_na()


jg <- c("153/valencia", "155/baskonia", "148/f-c-barcelona", "160/real-madrid",
        "917/lenovo-tenerife", "405/baxi-manresa", "844/surne-bilbao", "161/unicaja-malaga",
        "457/ucam-murcia", "154/joventut-badalona", "157/herbalife-gran-canaria")

jgLigas <- function(jg) {
  url <- paste0("https://www.proballers.com/es/baloncesto/equipo/", jg) |>
    read_html()

  euroliga <- tibble(
    link = url |> html_elements(".select-tab-average-30 a") |>
      html_attr("href"),
    nombre1 = url |> html_elements(".select-tab-average-30 a") |>
      html_text("title"),
    foto = url |> html_elements(".identity__picture img[itemprop='image']") |>
      html_attr("src")
  ) |>
    mutate(
      nombre1 = str_squish(nombre1),
      team = jg,
      team = case_when(
        team == "153/valencia" ~ "Valencia Basket",
        team == "155/baskonia" ~ "Cazoo Baskonia",
        team == "148/f-c-barcelona" ~ "Barça",
        team == "160/real-madrid" ~ "Real Madrid",
        team == "917/lenovo-tenerife" ~ "Lenovo Tenerife",
        team == "405/baxi-manresa" ~ "BAXI Manresa",
        team == "844/surne-bilbao" ~ "Surne Bilbao Basket",
        team == "161/unicaja-malaga" ~ "Unicaja",
        team == "457/ucam-murcia" ~ "UCAM Murcia",
        team == "154/joventut-badalona" ~ "Joventut Badalona",
        team == "157/herbalife-gran-canaria" ~ "Gran Canaria",
        TRUE ~ team
      ),
      nombre = str_extract(link, "([a-z]+-)\\w+"),
      codigo = str_extract(link, "[0-9]+"),
      link = str_remove(link, "/es/baloncesto/jugador/")
    )



  return(euroliga)
}


jgligas_df <- map_df(jg, jgLigas) |>
  select(nombre1, nombre, foto,team, codigo, link)

player <- jgligas_df$link



# función que extrae los ultimos 5 partidos en valoracion -----------------
# "https://www.proballers.com/es/baloncesto/jugador/60919/matt-costello" |>
#   read_html() |>



stats <- function(player) {
  jug <- paste0("https://www.proballers.com/es/baloncesto/jugador/", player) |>
    read_html()
  name <- paste0("https://www.proballers.com/es/baloncesto/jugador/", player)

  u5 <- tibble(

    fecha = jug |> html_elements("td.hidden:nth-of-type(1) a") |>
      html_text("href"),
    logoRival = jug |> html_elements(".first__left img") |>
      html_attr("src"),
    val = jug |> html_elements("td:nth-of-type(25)") |>
      html_text("class"),
    masmen = jug |> html_elements("td:nth-of-type(23)") |>
      html_text("class"),
    min = jug |> html_elements("td.switch:nth-of-type(9)") |>
      html_text("class"),
    v_l =  jug |> html_elements(".left span") |>
      html_text("span")
  ) |>
    mutate(
      nombre = str_extract(name, "([a-z]+-)\\w+"),
      codigo = str_extract(name, "[0-9]+")

    )
  return(u5)
}

playerStatsDf <- map_df(player, stats)

playerStats_Df <- playerStatsDf|>
  left_join(jgligas_df) |>
  group_by(nombre1) |>
  slice(3:1) |>
  separate(fecha,c("dia", "mes", "año"), " ") |>
  mutate(across(c(dia, año, val, masmen, min, codigo),as.numeric)) |>
  mutate( liga = case_when(
    team %in% c("Valencia Basket", "Cazoo Baskonia", "Barça", "Real Madrid") ~ "europa1",
    team %in% c("Lenovo Tenerife", "BAXI Manresa", "Surne Bilbao Basket", "UCAM Murcia") ~ "europa2",
    TRUE~"europa3"
  )) |>
  ungroup()

marzo <- playerStats_Df |>
  select(-logoRival, -link, -nombre, -nombre) |>
  filter(mes == "mar", dia >13) |>
  group_by(nombre1,team,foto) |>
  summarise(val =mean(val),
            masmen =mean(masmen),
            min = mean(min)) |>
  ungroup()|>
  arrange(desc(val)) |>
filter(val>=18) %>%
  left_join(logos)|>
   mutate(rk=row_number(),
          foto =ifelse(nombre1 == "Daulton Hommes", "https://static.acb.com/media/PRO/00/00/46/86/63/0000468663_5-6_03.jpg", foto),
         foto= circle_crop(foto, border_colour = color, border_size =10),
          logo = cropcircles::square_crop(logo, border_colour = color, border_size =10))

#
# marzo <- playerStats_Df |>
#   select(-logoRival, -link, -nombre, -nombre) |>
#   filter(mes == "mar", dia >5)
#   arrange(desc(val)) |>
#   slice(1:20) |>
#   mutate(rk=row_number()) |>
#   mutate(foto= circle_crop(foto, border_colour = color, border_size =10),
#          logo = cropcircles::square_crop(logo, border_colour = color, border_size =10))


 pl <- ggplot(marzo) +
  geom_text(mapping = aes(y = rk, x = 1.03, label = rk), hjust = 0.5, size = 4.2, fontface = "bold", family = "Roboto Condensed", color = "black")+
  geom_text(mapping = aes(y = -.05, x = 1, label = "RK"), hjust = 0, size = 6, family = "Roboto Condensed", color = "black")+
  geom_image( aes(y = rk, x = 1.1, image = foto), hjust =0, size = 0.04) +
  geom_text(mapping = aes(y = rk, x = 1.2, label = nombre1), hjust = 0, size =  4.2, family = "Roboto Condensed")+
  geom_text(mapping = aes(y = -.05, x = 1.12, label = "JUGADOR"), hjust = 0, size = 6, family = "Roboto Condensed", color = "black")+
  geom_image( aes(y = rk, x = 1.7, image = logo), size = 0.041)+
  geom_text(mapping = aes(y = rk, x = 1.8, label = abb), hjust = 0, size =  4.2, family = "Roboto Condensed", color = "black")+
  geom_text(mapping = aes(y = -.05, x = 1.78, label = "EQUIPO"), size = 6, family = "Roboto Condensed", color = "black")+
  geom_text(mapping = aes(y = rk, x = 2.1, label = min), hjust = 0, size =  4.2, family = "Roboto Condensed", color = "black")+
  geom_text(mapping = aes(y = -.05, x = 2.1, label = "MIN"), size = 6, hjust = 0.3, family = "Roboto Condensed", color = "black")+
  geom_text(mapping = aes(y = rk, x = 2.3, label = masmen), hjust = 0.5, size = 4.2, family = "Roboto Condensed", color = "black")+
  geom_text(mapping = aes(y = -.05, x = 2.3, label ="+/-"), size = 6, hjust = 0.5, family = "Roboto Condensed", color = "black")+
  geom_text(mapping = aes(y = rk, x = 2.5, label = val), hjust = 0, size =  4.2, family = "Roboto Condensed", color = "black")+
  annotate("rect", ymin =  .5, ymax  = 22.5, xmin = 2.45, xmax = 2.6, fill = "gray75", alpha =.25, color = "transparent") +
  geom_text(mapping = aes(y = -.05, x = 2.5, label ="VAL"), size = 6, hjust = 0.25, family = "Roboto Condensed", color = "black")+
  geom_segment(mapping = aes(y = rk + 0.5, yend = rk + 0.5, x = 1, xend = 2.6), size = 0.3, color ="grey") +
  geom_segment(mapping = aes(y = 1-.5, yend = 1-.5, x = 1, xend = 2.6), size = 0.5, color = "black") +
  coord_cartesian(clip = "off", expand = T)+
  scale_x_continuous(limits = c(1, 2.6)) +
  scale_y_reverse(limits = c(22.6, -2), expand = c(0, 0)) +
  theme_minimal() +
   labs(
     title = "Journey Europeo",
     subtitle = "<span style='font-weight:500;color:#8C8C8C'>Top en valoración >18 de los jugadores de la ACB  en Europa para la J23<br></span>",
     caption = "<br> **Datos**:   &nbsp;&nbsp;*@Proballers_com*
                     **Gráfico**: *Ivo Villanueva* &bull;  <span style='color:#00acee;font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span> <b>@elcheff</b>"
   ) +
   # geom_curve(aes(x = 2.42, y = -1.6, xend = 2.5, yend = -.5), colour= "#8C8C8C", size=.3, curvature = -0.2, arrow = arrow(length = unit(0.01, "npc"))) +
   # geom_curve(aes(x = 2.185, y = -1.6, xend = 2.12, yend = -.5), colour= "#8C8C8C", size=.3,curvature = 0.2, arrow = arrow(length = unit(0.01, "npc"))) +
   # geom_curve(aes(x = 2.3, y = -1.5, xend = 2.3, yend = -.5), colour= "#8C8C8C",size=.3, curvature = 0, arrow = arrow(length = unit(0.01, "npc"))) +
   # annotate(geom = 'text', x = 2.3, y = -1.8, hjust = .5,fontface ="italic", label = "Son medias porque algunos jugadores\nhan jugado dos partidos", family = "Roboto Condensed",  size = 2.8)+
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 45, hjust = .1, family = "Roboto Condensed"),
    plot.subtitle = element_markdown( size = 15, hjust = .15, family = "Roboto Condensed"),
    plot.background = element_rect("white"),
    panel.grid = element_blank(),
    plot.margin   = unit(c(2,2,3,2), "cm"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.caption = element_markdown(hjust =.925, size = 7)
  )

library(magick)
library(grid)



ggsave("eurolig_2.png", pl, w = 10 , h = 10*1.1, dpi = "retina", type ="cairo")
