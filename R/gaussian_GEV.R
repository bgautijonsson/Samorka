library(tidyverse)
library(bggjphd)
library(metill)
library(gganimate)
library(sf)
library(ggtext)
library(glue)
library(scales)
theme_set(theme_bggj())


d <- crossing(
    n_samp = c(10, 33, 100, 333, 1000, 3333, 10000, 33333, 100000),
    iter = 1:3000
) |> 
    mutate(
        samps = map_dbl(n_samp, ~ rnorm(.x) |> max())
    ) |> 
    arrange(n_samp, samps) |> 
    mutate(
        q = row_number() / (n() + 1),
        .by = n_samp
    )
    

d |> 
    ggplot(aes(samps, q)) +
    geom_line(aes(group = n_samp, col = n_samp))

