library(tidyverse)
library(ggplot2)
library(tibble)
library(lubridate)
library(readr)

#Importar os dados.
dados <- read_csv("Entrega 2 (05.05)/dados.csv")

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

#Transformar em Tibble.
D1 <- as.data.frame(dados)
T1 <- as_tibble(D1)

#Fazer a limpeza dos dados.
T1 <- T1[ , 4:13]
T1 <- T1[ , -2]
T1 <- T1[ , -3]
T1 <- T1[ , -4:-7]
T1 <- T1 %>%
  filter(T1$format == "Serie")
T1 <- T1 %>%
  filter(T1$season != "Special")
T1 <- T1[ , -4]
T1$imdb <- as.numeric(as.character (T1$imdb))
T1 <- T1 %>%
  mutate(season = case_when(
    season == "1" ~ "1ª Temporada",
    season == "2" ~ "2ª Temporada",
    season == "3" ~ "3ª Temporada",
    season == "4" ~ "4ª Temporada",
    TRUE ~ season
  ))
T1$season <- factor(T1$season, levels = c("1ª Temporada","2ª Temporada","3ª Temporada","4ª Temporada"))
T1 <- T1 %>%
  select(-series_name)

#Gerar gráficos BloxSpot.
ggplot(T1) +
  aes(x = reorder(season, imdb), y = imdb) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Temporada", y = "Nota IMDB") +
  estat_theme()
ggsave("Entrega 2 (05.05)/IMDBxTemporada.pdf", width = 158, height = 93, units = "mm")

#Quadro de medidas resumo.
D2 <- as.data.frame(T1)
print_quadro_resumo <- function(D2, title="Medidas resumo da(o) [nome da variável]", label="quad:quadro_resumo1")
{
  D2 <- T1 %>%
    group_by(season) %>%
    summarize(`Média` = round(mean(imdb),2),
              `Desvio Padrão` = round(sd(imdb),2),
              `Variância` = round(var(imdb),2),
              `Mínimo` = round(min(imdb),2),
              `1º Quartil` = round(quantile(imdb, probs = .25),2),
              `Mediana` = round(quantile(imdb, probs = .5),2),
              `3º Quartil` = round(quantile(imdb, probs = .75),2),
              `Máximo` = round(max(imdb),2)) %>%
    t() %>% 
    as.data.frame() %>%
    rownames_to_column()
  
  latex <- str_c("\\begin{quadro}[H]
\t\\caption{", title, "}
\t\\centering
\t\\begin{adjustbox}{max width=\\textwidth}
\t\\begin{tabular}{", sep="")
  
  col_count <- ncol(D2)
  row_count <- nrow(D2)
  latex <- str_c(latex, "| l", "|", sep=" ")
  for (i in seq(2, col_count))
  {
    latex <- str_c(latex, "S", sep=" ")
  }
  
  
  latex <- str_c(latex, "|}\n\t\\toprule\n\t\t", sep="")
  if (col_count > 2)
  {
    for (i in seq(1,col_count))
    {
      if (i == 1)
        latex <- str_c(latex, "\\textbf{Estatística}", sep="")
      else
        latex <- str_c(latex, " \\textbf{", D2[1, i], "}", sep="")
      
      if (i < col_count)
        latex <- str_c(latex, "&", sep=" ")
      else
        latex <- str_c(latex, "\\\\\n", sep=" ")
    }
  }
  else
  {
    latex <- str_c(latex, "\\textbf{Estatística} & \\textbf{Valor} \\\\\n", sep="")  
  }
  
  latex <- str_c(latex, "\t\t\\midrule\n", sep="")
  
  if (col_count > 2)
    starting_number <- 2
  else
    starting_number <- 1
  
  for (i in seq(starting_number, row_count))
  {
    latex <- str_c(latex, "\t\t", str_flatten(t(D2[i,]), collapse = " & "), " \\\\\n")
  }
  latex <- str_c(latex, "\t\\bottomrule
\t\\end{tabular}
\t\\label{", label, "}
\t\\end{adjustbox}
\\end{quadro}", sep="")
  
  writeLines(latex)
}

print_quadro_resumo()
