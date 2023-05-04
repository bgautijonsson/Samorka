plot_dat <- d |> 
    # slice_max(n = 100, order_by = mu) |> 
    slice_max(n = 30, order_by = delta) |> 
    crossing(
        year = seq(1981, 2080)
    ) |> 
    mutate(
        mut = mu * (1 + delta * (year - 1981))
    ) |> 
    crossing(
        y = seq(10, 50, length.out = 10)
    ) |> 
    mutate(
        p = 1 - pgev(y, mut, sigma, xi),
        t = 1 / p
    )



p <- plot_dat |> 
    ggplot(aes(y, p)) +
    geom_line(
        aes(group = station),
        alpha = 0.5
        ) +
    scale_x_tufte(
        labels = label_number(suffix = " mm/klst")
    ) +
    scale_y_tufte(
        trans = "log10",
        expand = expansion(),
        labels = label_hlutf(accuracy = 0.001),
        limits = c(NA, NA)
    ) +
    labs(
        x = NULL,
        y = NULL,
        title = "Líkur á klukkustundarúrkomu yfir ákveðnu marki",
        subtitle = "Árið {frame_time}"
    ) +
    transition_time(as.integer(year)) +
    ease_aes("cubic-in-out")


p_anim <- animate(
    plot = p,
    nframes = 80,
    fps = 10,
    width = 8, height = 0.621 * 8, unit = "in",
    res = 200
)


anim_save(
    animation = p_anim,
    filename = "images/flood_freq_change.gif"
)
