library(tidyverse)
library(ggplot2)
library(tibble)
library(lubridate)
library(readr)
banco_final <- read_csv("Entrega 5 (26.05)/banco_final.csv")

#Padronização da ESTAT.
estat_colors <- c(
  "#A11D21", "#003366", "#CC9900",
  "#663333", "#FF6600", "#CC9966",
  "#999966", "#006606", "#008091", 
  "#041835", "#666666" )

estat_theme <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = estat_colors),
      scale_colour_manual(values = estat_colors)
    )
  )
}

#Transforma em Tibble:
D <- as_tibble(banco_final)
D <- D[ , c(9,20,21,22,23,24,41,42)]
D <- na.omit()
D <- rename(D, "Fred" = caught_fred)
D <- rename(D, "Daphnie" = caught_daphnie)
D <- rename(D, "Velma" = caught_velma)
D <- rename(D, "Salsicha" = caught_shaggy)
D <- rename(D, "Scooby" = caught_scooby)
D <- rename(D, "Outro" = caught_other)
D <- rename(D, "Não_capturado" = caught_not)
D <- rename(D, "Engajamento" = engagement)


#Gerar gráficos BloxSpot.
