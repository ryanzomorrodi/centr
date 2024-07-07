library(hexSticker)
library(magick)

sticker("hexsticker/centr.png", 
  
  s_x = 0.6, 
  s_y = 1.2, 
  s_width = 0.76, 
  s_height = 0.6,

  package = "centr", 
  p_y = 0.55,
  p_size = 20, 
  p_color = "#cecece",

  h_fill = "#453737",
  h_color = "#e14901",
  
  filename="hexsticker/hexSticker.png")

image <- image_read("hexsticker/hexSticker.png")
plot(image)
