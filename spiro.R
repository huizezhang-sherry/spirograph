library(tidyverse)
gcd <- function(x,y) {
  r <- x%%y;
  return(ifelse(r, gcd(y, r), y))
}

find_reduced_form <- function(a, b){
  d <- gcd(a, b)
  return(c(a/d, b/d))
}

spiro_curve <- function(R, r, h, delta = pi/360, inside = TRUE){
  reduced_form <- find_reduced_form(R, r)
  if (inside){
    # hypotrochoid
    tibble(
      theta = seq(0, 2*pi*reduced_form[2], delta),
      x = (R - r) * cos(theta) + h * cos((R - r) / r * theta),
      y = (R - r) * sin(theta) - h * sin((R - r) / r * theta)
    ) 
    
  } else{
    # epitrochoid
    tibble(
      theta = seq(0, 2*pi * reduced_form[2], delta),
      x = (R + r) * cos(theta) - h * cos((R + r) / r * theta),
      y = (R + r) * sin(theta) - h * sin((R + r) / r * theta)
    ) 
  }
  
}

r_vec <- c(24, 30, 32, 40, 42, 
           45, 48, 52, 56, 60, 
           63, 72, 75, 80, 84)

spiro_data <- crossing(
  inside = TRUE, R = c(96, 105), r = r_vec, h = seq(20, 80, 5)) |> 
  bind_rows(crossing(inside = FALSE, R = c(144, 150), r = r_vec, h = seq(20, 80, 5))) |> 
  filter(r > h) |> 
  rowwise() |> 
  mutate(dt = list(spiro_curve(R = R, r = r, h = h, inside = inside))) |> 
  unnest(dt)


p1 <- spiro_data |> filter(inside == TRUE, R == 96) |> 
  ggplot(aes(x = x, y = y)) + 
  geom_path() + 
  ggh4x::facet_nested(h ~ r) + 
  theme_void() + 
  theme(aspect.ratio = 1) 

p2 <- p1 %+% filter(spiro_data, inside == TRUE, R == 105)
p3 <- p1 %+% filter(spiro_data, inside == FALSE, R == 144)
p4 <- p1 %+% filter(spiro_data, inside == FALSE, R == 150)

library(patchwork)
ggsave(p1, filename = "figures/spiro-inside-96.png", bg = "white")
ggsave(p2, filename = "figures/spiro-inside-105.png", bg = "white")
ggsave(p3, filename = "figures/spiro-outside-144.png", bg = "white")
ggsave(p4, filename = "figures/spiro-outside-150.png", bg = "white")

96/56
map_dfr(seq(21, 40, 3), ~spiro_curve(R = 96, r = 80, h = .x, inside = TRUE), .id = "id") |> 
  ggplot(aes(x = x, y = y, group = id, color = id)) + 
  geom_path() + 
  scale_color_brewer(palette = "OrRd") + 
  theme_void() + 
  theme(aspect.ratio = 1, legend.position = "none")

map_dfr(seq(21, 90, 5), ~spiro_curve(R = 96, r = 56, h = .x, inside = FALSE), .id = "id") |>
  mutate(id = as.numeric(id)) |>
  ggplot(aes(x = x, y = y, group = id, color = id)) + 
  geom_path() + 
  colorspace::scale_color_continuous_sequential("Grays") + 
  theme_void() + 
  theme(aspect.ratio = 1, legend.position = "none")


spiro_curve(R = 96, r = 63, h = 40, inside = TRUE) |> 
  ggplot(aes(x = x, y = y)) + 
  geom_path() + 
  colorspace::scale_color_continuous_sequential("Grays") + 
  theme_void() + 
  theme(aspect.ratio = 1, legend.position = "none")
