library(tidyverse)
library(bggjphd)
library(metill)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(glue)
library(arrow)

uk <- ne_countries(
    scale = "medium", 
    returnclass = "sf",
    country =  c("United Kingdom", "Ireland")
) 




plot_param <- function(param, title = NULL, subtitle = NULL) {
    uk |> 
        ggplot() +
        geom_sf() +
        geom_sf(
            data = d,
            alpha = 0.5,
            # color = NA,
            linewidth = 0.01,
            aes(fill = {{ param }}, color = after_stat(fill))
        ) +
        scale_color_viridis_c(
        ) +
        scale_fill_viridis_c(
        ) +
        theme(
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "none"
        ) +
        labs(
            title = title,
            subtitle = subtitle
        )
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
    select(-(gamma:tau)) |> 
    stations_to_sf() |> 
    points_to_grid()

p <- plot_param(
    mu, 
    subtitle = "Staðsetning (μ)"
) +
    plot_param(
        sigma,
        subtitle = "Dreifni (σ)"
    ) +
    plot_param(
        xi,
        subtitle = "Lögun (ξ)"
    ) +
    plot_param(
        delta,
        subtitle = "Leitni (Δ)"
    ) +
    plot_layout(
        nrow = 1
    ) +
    plot_annotation(
        title = "Niðurstöður úr landfræðilegu líkani",
        subtitle = "Myndin sýnir meðaltal fyrir hverja stöð úr eftirádreifingu Bayesísks líkansins"
    )


ggsave(
    plot = p,
    filename = "images/param_results_mcmc.png",
    width = 8, height = 0.4 * 8, scale = 1.3
)


d <- read_parquet("data/station_results.parquet") |> 
    select(-mcmc_mean, value = ml_estimate) |> 
    pivot_wider(names_from = variable, values_from = value) |> 
    mutate(
        delta = link_trend_inverse(gamma),
        xi = link_shape_inverse(phi),
        sigma = exp(tau + psi),
        mu = exp(psi)
    ) |> 
    select(-(gamma:tau)) |> 
    stations_to_sf() |> 
    points_to_grid()

p <- plot_param(
    mu, 
    subtitle = "Staðsetning (μ)"
) +
    plot_param(
        sigma,
        subtitle = "Dreifni (σ)"
    ) +
    plot_param(
        xi,
        subtitle = "Lögun (ξ)"
    ) +
    plot_param(
        delta,
        subtitle = "Leitni (Δ)"
    ) +
    plot_layout(
        nrow = 1
    ) +
    plot_annotation(
        title = "Niðurstöður úr líkani án landfræðilegra stika",
        subtitle = "Myndin sýnir meðaltal fyrir hverja stöð úr eftirádreifingu líkansins"
    )


ggsave(
    plot = p,
    filename = "images/param_results_ml.png",
    width = 8, height = 0.4 * 8, scale = 1.3
)
