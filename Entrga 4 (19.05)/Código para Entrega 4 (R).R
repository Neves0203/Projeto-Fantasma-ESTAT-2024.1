library(tidyverse)
library(ggplot2)
library(tibble)
library(lubridate)
library(readr)

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

ggplot(D) +
  aes(x = engagement, y = imdb) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Engajamento",
    y = "Nota IMDB"
  ) +
  estat_theme()
ggsave("IMDBxEngajamentpo.pdf", width = 158, height = 93, units = "mm")

ggplot(D) +
  aes(x=factor(""), y=engagement) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Engajamento")+
  estat_theme()

ggplot(D) +
  aes(x=factor(""), y=imdb) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Nota IMDB")+
  estat_theme()
D
C <- as.vector(D$imdb)
L <- as.vector(D$engagement)
P <- C/L*100
P <- as_tibble(P)

ggplot(P) +
  aes(x=factor(""), y=value) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Nota IMDB")+
  estat_theme()

quadro_resumo <- P %>% 
  summarize(Média = round(mean(value),2),
            `Desvio Padrão` = round(sd(value),2),
            `Variância` = round(var(value),2),
            `Mínimo` = round(min(value),2),
            `1º Quartil` = round(quantile(value, probs = .25),2),
            Mediana = round(quantile(value, probs = .5),2),
            `3º Quartil` = round(quantile(value, probs = .75),2),
            `Máximo` = round(max(value),2)) %>% t() %>% as.data.frame() %>% 
  mutate(V1 = str_replace(V1,"\\.",",")) # adicionar mais mutate(...) se tiver mais categorias

