library(tidyverse)
library(bggjphd)
library(metill)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(glue)

theme_set(theme_bggj())

uk <- ne_countries(
    scale = "medium", 
    returnclass = "sf",
    country =  c("United Kingdom", "Ireland")
) 

d <- precip |> 
    mutate(
        year = 5 * floor(year/5)
    ) |> 
    summarise(
        precip = max(precip),
        .by = c(year, station)
    )


years <- c(1981, 2023, 2080)
lims <- c(0, 110)

plot_fun <- function(yr) {
    
    plot_dat <- precip |> 
        filter(
            year == yr
        ) |> 
        inner_join(stations) |> 
        stations_to_sf() |> 
        points_to_grid()
    
    
    uk |> 
        ggplot() +
        geom_sf() +
        geom_sf(
            data = plot_dat,
            alpha = 0.6,
            linewidth = 0.001,
            size = 0,
            aes(fill = precip, color = after_scale(fill))
        ) +
        scale_color_viridis_c(
            limits = lims
        ) +
        scale_fill_viridis_c(
            limits = lims
        ) +
        theme(
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "none"
        ) +
        labs(
            subtitle = glue("Árið {yr}"),
            fill =  NULL
        )
    
}



p_list <- map(years, plot_fun)



p <- wrap_plots(p_list) +
    plot_layout(
        nrow = 1
    ) +
    plot_annotation(
        title = "Landfræðileg dreifing hámarksúrkomu",
        subtitle = "Teiknað út frá UKCP gögnum fyrir RCP8.5",
        theme = theme(
            plot.margin = margin()
        )
    )



ggsave(
    plot = p,
    filename = "images/max_precip.png",
    width = 8, height = 0.5 * 8, scale = 1.3
)

p
