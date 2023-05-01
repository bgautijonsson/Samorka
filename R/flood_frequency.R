library(tidyverse)
library(bggjphd)
library(metill)
library(gganimate)
library(sf)
library(ggtext)
library(glue)
library(scales)
library(arrow)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
theme_set(theme_bggj())

uk <- ne_countries(
    scale = "medium", 
    returnclass = "sf",
    country =  c("United Kingdom", "Ireland")
) 

pgev <- function(y, loc, scale, shape) {
    out <- 1 + shape * (y - loc) / scale
    out <- out ^ (-1/shape)
    out <- exp(-out)
    out
} 


d <- read_parquet("data/station_results.parquet") |> 
    select(-ml_estimate, value = mcmc_mean) |> 
    pivot_wider(names_from = variable, values_from = value) |> 
    mutate(
        delta = link_trend_inverse(gamma),
        xi = link_shape_inverse(phi),
        sigma = exp(tau + psi),
        mu = exp(psi)
    ) |> 
    select(-(gamma:tau))


p <- d |> 
    slice_max(n = 300, order_by = mu) |> 
    slice_max(n = 10, order_by = delta) |> 
    crossing(
        year = seq(1981, 2080)
    ) |> 
    mutate(
        mut = mu * (1 + delta * (year - 1981))
    ) |> 
    crossing(
        y = c(20, 30, 40, 50)
    ) |> 
    mutate(
        p = 1 - pgev(y, mut, sigma, xi),
        t = 1 / p,
        y = glue("{y} mm/klst")
    ) |> 
    ggplot(aes(year, t)) +
    geom_line(aes(group = station)) +
    scale_x_continuous(
        breaks = c(1981, 2080, 2000, 2020, 2040, 2060)
    ) +
    scale_y_continuous(
        labels = label_number(suffix = " ára", accuracy = 1)
    ) +
    facet_wrap(vars(y), scales = "free_y") +
    theme(
        panel.spacing = unit(1, "cm")
    ) +
    labs(
        x = NULL,
        y = NULL,
        title = "Tíðni mismunandi hámarksúrkoma fer lækkandi",
        subtitle = "Reiknað út frá UKCP gögnum"
    )


ggsave(
    plot = p,
    filename = "images/flood_freq.png",
    width = 8, height = 0.621 * 8, scale = 1.3
)



