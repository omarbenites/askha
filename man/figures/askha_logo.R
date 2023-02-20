library(hexSticker)
library(magick)
library(showtext)
library(dplyr)

# open https://fonts.google.com/
# font poma

#descarga IMAGEN DE
### https://docs.google.com/presentation/d/1KjFWZUBvNHuYFDRcMUkRlgWg6zfyQwBubGvYpIHPnr8/edit#slide=id.g20f5d2342d8_0_46
##
# -------------------------------------------------------------------------
# inkaverse ---------------------------------------------------------------
# -------------------------------------------------------------------------
font_add_google("Righteous")
#font_add_google(name="Montserrat")

logo <- list.files("man/figures/"
                   , full.names = T
                   , pattern = "raw_askha_logo2.png"
) %>% 
  image_read() 

sticker(logo, package="askha",white_around_sticker = TRUE,
        p_size=18, s_x=1, s_y=.8, s_width=1.8, s_height=2, 
        h_fill="#d1f4eeff",h_color = "#000000", p_family = "Righteous",p_color = "black",
        filename = "man/figures/askha_sticker_logo.png"
)


fuzz <- 50
p <- image_read("man/figures/askha_sticker_logo.png")
pp <- p %>%
  image_fill(color = "transparent", refcolor = "white", fuzz = fuzz, point = "+1+1") %>%
  image_fill(color = "transparent", refcolor = "white", fuzz = fuzz, point = paste0("+", image_info(p)$width-1, "+1")) %>%
  image_fill(color = "transparent", refcolor = "white", fuzz = fuzz, point = paste0("+1", "+", image_info(p)$height-1)) %>%
  image_fill(color = "transparent", refcolor = "white", fuzz = fuzz, point = paste0("+", image_info(p)$width-1, "+", image_info(p)$height-1))
image_write(image = pp, path = "man/figures/askha_sticker_logo.png")

