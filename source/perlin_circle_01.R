
# load packages -----------------------------------------------------------

library(tidyverse)
library(scales)
library(ambient)
library(here)
source(here("source", "common.R"))


# the thing I want to play with -------------------------------------------

perlin_circle <- function(cx = 0, cy = 0, n = 100, noise_max = 0.5,
                          octaves = 2, r_min = 0.5, r_max = 1) {
  tibble(
    angle = seq(0, 2*pi, length.out = n),
    xoff = cos(angle) %>% rescale(from = c(-1, 1), to = c(0, noise_max)),
    yoff = sin(angle) %>% rescale(from = c(-1, 1), to = c(0, noise_max)),
    r = gen_simplex %>%
      fracture(fractal = fbm, x = xoff, y = yoff, octaves = octaves) %>%
      rescale(from = c(-0.5, 0.5), to = c(r_min, r_max)),
    x = r * cos(angle) + cx,
    y = r * sin(angle) + cy
  )
}


# plot parameters ---------------------------------------------------------

seed <- 1
sys_id <- "perlincircle"
sys_version <- 1

bg <- "black"
xlim <- c(-2, 2)
ylim <- c(-2, 2)


# generate image ----------------------------------------------------------

set.seed(seed)

dat <- perlin_circle()

pic <- dat %>%
  ggplot(aes(x, y)) +
  geom_path(size = 2, colour = "white", show.legend = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill = bg)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed(xlim = xlim, ylim = ylim) +
  NULL

ggsave(
  filename = save_path(sys_id, sys_version, seed),
  plot = pic,
  width = 10,
  height = 10,
  dpi = 300
)

