install.packages("hexSticker")
library(hexSticker)
library(showtext)

font_add_google("Knewave")

showtext_auto()

imgurl <- system.file("lamp (1).png", package="GenieBPC")
sticker(imgurl,
        package="GenieBPC",
        p_size=20,
        s_x=1, s_y=.75, s_width=.5,
        h_color = "#000000",
        h_fill = "#7318a8",
        p_family = "Knewave",
        p_color = "#fad46b",
        spotlight = FALSE,
        filename="imgfile.png")
