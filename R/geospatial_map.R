library(tidyverse)
library(bggjphd)
library(metill)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(glue)

theme_set(theme_bggj())

station1 <- 10000
station1_col <- "#99000d"

station2 <- 15000
station2_col <- "#0570b0"


station3 <- 11800
station3_col <- "#ff7f00"

uk <- ne_countries(
    scale = "medium", 
    returnclass = "sf",
    country =  c("United Kingdom", "Ireland")
) 

d <- stations |> 
    stations_to_sf() |> 
    points_to_grid() |> 
    filter(
        station %in% c(station1, station2, station3)
    ) |> 
    mutate(
        col = case_match(
            station,
            station1 ~ station1_col,
            station2 ~ station2_col,
            station3 ~ station3_col
        )
    )


p <- uk |> 
    ggplot() +
    geom_sf() +
    geom_sf(
        data = d, 
        size = 3,
        linewidth = 6,
        aes(fill = col, col = col)
    ) +
    scale_colour_identity() +
    scale_fill_identity() +
    coord_sf(expand = FALSE) +
    theme(
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none"
    ) 


p

ggsave(
    plot = p,
    filename = "images/stations.png",
    width = 8, height = 1.5 * 8, scale = 1
)
