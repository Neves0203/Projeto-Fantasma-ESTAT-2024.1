library(tidyverse)
library(ggplot2)
library(tibble)
library(readr)

#Importar os dados do PS.
dados <- read_csv("Entrega 1 (28.04)/dados.csv")

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

#Transformar os dados em data frame, afim de, no futuro, ter a coluna date_aired em formato de data.
D1 <- as.data.frame(dados)
D1$date_aired <- mdy(D1$date_aired)

#Transformar em Tibble, pois só aprendi a trabalhar com ele.
T1 <- as_tibble(D1)
T1 
#Retirar as colunas que não quero.
T1 <- T1[ , 10:13]
T1 <- T1[ , -2:-3]

#Retirar o formato de data completa.
T1 <- T1 %>%
  mutate(Data = floor_date(date_aired, unit = "year"))
T1 <- T1[ , -1]
T1$Data <- format(T1$Data, "%Y")

#Ordernar os valores em décadas.
T1 <- T1 %>%
  mutate(Data = case_when(
    Data  %in% c("1969") ~ "60",
    Data  %in% c("1970", "1971", "1972","1973","1974","1975","1976","1977","1978","1979") ~ "70",
    Data  %in% c("1980","1981","1982","1983","1984","1985","1986","1987","1988","1989") ~ "80",
    Data  %in% c("1990", "1991","1992","1993","1994","1995","1996","1997","1998","1999") ~ "90",
    Data  %in% c("2000", "2001","2002","2003","2004","2005","2006","2007","2008","2009") ~ "2000",
    Data  %in% c("2010", "2011","2012","2013","2014","2015","2016","2017","2018","2019") ~ "2010",
    Data  %in% c("2020", "2021") ~ "2020",
    TRUE ~ Data
  ))
T1$Data <- factor(T1$Data, levels = c("60","70","80","90","2000","2010","2020"))

#Transformar os dados em datas para conseguir as frequências.
C1 <- table(T1$format,T1$Data)
C1

#Transformar em Data Frame, para, de novo, transformar em Tibble.
D <- as.data.frame(C1)
T <- as_tibble(D)

#Organizar os nomes.
T <- rename(T, "Formato" = Var1)
T <- rename(T, "Década" = Var2)
T <- rename(T, "Frequência" = Freq)
T <- T %>%
  mutate(Formato = case_when(
    Formato == "Serie" ~ "Episódios (Série)",
    Formato == "Movie" ~ "Filme",
    TRUE ~ Formato
  ))

#Gráfico de linhas com as três variáveis.
ggplot(T) +
  aes(x = Década, y = Frequência, group = Formato, colour = Formato) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Década", y = "Frequência de Lançamentos") +
  estat_theme()
ggsave("Freq.CrossOvers,Filmes,EpisódeosxDécada.pdf", width = 158, height = 93, units = "mm")
