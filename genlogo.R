#install.packages("hexSticker")
library(hexSticker)
#install.packages("emojifont")
library(emojifont)
r <- ggplot() + geom_fontawesome("fa-book", color='orangered', size = 20) + theme_void()


sticker(
  r,
  package = "ModStatR",
  p_size = 8,
  s_x = 0.95,
  s_y = 0.7,
  s_width = 1.7,
  s_height = 1.3,
  p_x = 1,
  p_y = 1.3,
  url = "https://cran.r-project.org/package=ModStatR",
  u_color = "white",
  u_size = 1.1,
  h_fill = "black",
  h_color = "grey",
  filename = "man/figures/logo.png"
)
