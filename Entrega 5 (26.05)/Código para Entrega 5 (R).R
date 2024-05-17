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
D <- rename(D, "Fred" = caught_fred)
D <- rename(D, "Daphnie" = caught_daphnie)
D <- rename(D, "Velma" = caught_velma)
D <- rename(D, "Salsicha" = caught_shaggy)
D <- rename(D, "Scooby" = caught_scooby)
D <- rename(D, "Outro" = caught_other)
D <- rename(D, "Não_capturado" = caught_not)
D <- rename(D, "Engajamento" = engagement)


#Separar os dados:
#Fred:
X <- D[ ,c("Engajamento",'Fred')]
FX <- X$Fred == FALSE
X$Fred[FX] <- NA
Fred <- na.omit(X)
Fred$Fred <- "Fred"


#Daphnie:
X <- D[ ,c("Engajamento",'Daphnie')]
FX <- X$Daphnie == FALSE
X$Daphnie[FX] <- NA
Daphnie <- na.omit(X)
Daphnie$Daphnie <- "Daphnie"

#Velma:
X <- D[ ,c("Engajamento",'Velma')]
FX <- X$Velma == FALSE
X$Velma[FX] <- NA
Velma <- na.omit(X)
Velma$Velma <- "Velma"

#Salsicha:
X <- D[ ,c("Engajamento",'Salsicha')]
FX <- X$Salsicha == FALSE
X$Salsicha[FX] <- NA
Salsicha <- na.omit(X)
Salsicha$Salsicha <- "Salsicha"

#Scooby:
X <- D[ ,c("Engajamento",'Scooby')]
FX <- X$Scooby == FALSE
X$Scooby[FX] <- NA
Scooby <- na.omit(X)
Scooby$Scooby <- "Scooby"

#Outro:
X <- D[ ,c("Engajamento",'Outro')]
FX <- X$Outro == FALSE
X$Outro[FX] <- NA
Outro <- na.omit(X)
Outro$Outro <- "Outro"

K <- ggplot() +
  geom_boxplot(aes(x = Fred, y = Engajamento), Fred, fill = "#A11D21", width = 0.5) +
  stat_summary(aes(Fred, Engajamento), Fred, fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  geom_boxplot(aes(Daphnie, Engajamento), Daphnie, fill = c("#A11D21"), width = 0.5)+
  stat_summary(aes(Daphnie, Engajamento), Daphnie, fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  geom_boxplot(aes(Velma, Engajamento), Velma, fill = c("#A11D21"), width = 0.5)+
  stat_summary(aes(Velma, Engajamento), Velma, fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  geom_boxplot(aes(Salsicha, Engajamento), Salsicha, fill = c("#A11D21"), width = 0.5)+
  stat_summary(aes(Salsicha, Engajamento), Salsicha, fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  geom_boxplot(aes(Scooby, Engajamento), Scooby, fill = c("#A11D21"), width = 0.5)+
  stat_summary(aes(Scooby, Engajamento), Scooby, fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  geom_boxplot(aes(Outro, Engajamento), Outro, fill = c("#A11D21"), width = 0.5)+
  stat_summary(aes(Outro, Engajamento), Outro, fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Personagem", y = "Engajamento") +
  estat_theme()

ggplot() +
  geom_boxplot(aes(x = reorder(Fred, Engajamento, FUN = mean), y = Engajamento), Fred, fill = "#A11D21", width = 0.5) +
  stat_summary(aes(Fred, Engajamento), Fred, fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  geom_boxplot(aes(x = reorder(Daphnie, Engajamento, FUN = mean), Engajamento), Daphnie, fill = c("#A11D21"), width = 0.5)+
  stat_summary(aes(Daphnie, Engajamento), Daphnie, fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  geom_boxplot(aes(x = reorder(Velma, Engajamento, FUN = mean), Engajamento), Velma, fill = c("#A11D21"), width = 0.5)+
  stat_summary(aes(Velma, Engajamento), Velma, fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  geom_boxplot(aes(x = reorder(Salsicha, Engajamento, FUN = mean), Engajamento), Salsicha, fill = c("#A11D21"), width = 0.5)+
  stat_summary(aes(Salsicha, Engajamento), Salsicha, fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  geom_boxplot(aes(x = reorder(Scooby, Engajamento, FUN = mean), Engajamento), Scooby, fill = c("#A11D21"), width = 0.5)+
  stat_summary(aes(Scooby, Engajamento), Scooby, fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  geom_boxplot(aes(x = reorder(Outro, Engajamento, FUN = mean), Engajamento), Outro, fill = c("#A11D21"), width = 0.5)+
  stat_summary(aes(Outro, Engajamento), Outro, fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Personagem", y = "Engajamento") +
  estat_theme()

