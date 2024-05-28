library(tidyverse)
library(ggplot2)
library(tibble)
library(lubridate)
library(readr)

#Importar os dados.
banco_final <- read_csv("Entrega 4 (19.05)/banco_final.csv")

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

#Transfgormar em Tibble:
D <- as_tibble(banco_final)
D <- D[ , c(8:9)]

#Gráfico:
ggplot(D) +
  aes(x = engagement, y = imdb) +
  geom_point(colour = "#A11D21", size = 3, alpha = 0.3) +
  labs(
    x = "Engajamento",
    y = "Nota IMDB"
  ) +
  estat_theme()
ggsave("Entrega 4 (19.05)/IMDBxEngajamentpo.pdf", width = 158, height = 93, units = "mm")

# Coeficiente de Pearson:
correlation <- cor(D$imdb, D$engagement)
correlation

#Quadro de medidas resumo: 

#IMDB:
round(mean((D$imdb)),2)
round(sd((D$imdb)),2)
round(var((D$imdb)),2)
round(min((D$imdb)),2)
round(quantile((D$imdb), probs = 0.25),2)
round(median((D$imdb)),2)
round(quantile((D$imdb), probs = 0.75),2)
round(max((D$imdb)),2)

#Engajamento:
round(mean((D$engagement)),2)
round(sd((D$engagement)),2)
round(var((D$engagement)),2)
round(min((D$engagement)),2)
round(quantile((D$engagement), probs = 0.25),2)
round(median((D$engagement)),2)
round(quantile((D$engagement), probs = 0.75),2)
round(max((D$engagement)),2)


