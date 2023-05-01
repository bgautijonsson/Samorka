library(tidyverse)
library(bggjphd)
library(metill)
library(gganimate)
library(sf)
library(ggtext)
library(glue)
library(scales)
theme_set(theme_bggj())

max_col <- "#cb181d"

norm_col <- "#969696"

n_frame <- 20
n_samp <- 365 * 24

d <- map_dfr(
    seq_len(n_frame), 
    function(iter) {
        tibble(
            iter = iter,
            x = rexp(n = n_samp, rate = 1),
            col = ifelse(
                x == max(x),
                max_col,
                norm_col
            )
        )
    }
)

# d <- map_dfr(
#     seq_len(n_frame),
#         function(i) {
#             d |> 
#                 filter(iter <= i) |> 
#                 filter(
#                     (col == max_col) | (runif(n()) < 1 / (i * 24))
#                 ) |> 
#                 mutate(frame = i)
#         }
# )



p <- d |> 
    ggplot(aes(x)) +
    geom_histogram(aes(fill = col, y = after_stat(ndensity), group = col)) +
    scale_x_continuous(
        expand = expansion()
    ) +
    scale_y_continuous(
        limits = c(0, 1.2),
        expand = expansion(),
        labels = label_number(suffix = " (mm/klst)")
    ) +
    scale_fill_identity() +
    theme(
        plot.title = element_markdown(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
    ) +
    labs(
       x = "Hámarks klukkustundarúrkoma yfir eitt ár",
       y = NULL,
       title = str_c(
           "Dreifing á <b style='color:{norm_col}'>venjulegri úrkomu </b>",
           "og <b style='color:{max_col}'>hámarksúrkomu"
           ) |> glue(),
    ) +
    transition_states(iter, transition_length = 2, state_length = 2) +
    ease_aes("cubic-in-out")


p_anim <- animate(
    plot = p,
    nframes = n_frame * 8,
    fps = 10,
    width = 8, height = 0.621 * 8, unit = "in",
    res = 200
)


anim_save(
    animation = p_anim,
    filename = "images/dreifing_maximum.gif"
)
