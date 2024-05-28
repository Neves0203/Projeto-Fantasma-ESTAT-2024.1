library(tidyverse)
library(ggplot2)
library(tibble)
library(lubridate)
library(readr)

#Importar os dados.
banco_final <- read_csv("Entrega 3 (12.05)/banco_final.csv")

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

#Transformar em Tibble:
D <- as.data.frame(banco_final)
D <- as_tibble(D)
D <- D[ , -1:-42]
D <- D[ , -3:-35]

#Tabela dos terrenos mais frequêntes:
D <- D %>%
  mutate(setting_terrain = case_when(
    setting_terrain %>% str_detect("Forest") ~ "Floresta",
    setting_terrain %>% str_detect("Urban") ~ "Urbano",
    setting_terrain %>% str_detect("Rural") ~ "Rural",
    setting_terrain %>% str_detect("Island") ~ "Ilha",
    setting_terrain %>% str_detect("Jungle") ~ "Selva",
    setting_terrain %>% str_detect("Mountain") ~ "Montanha",
    setting_terrain %>% str_detect("Ocean") ~ "Oceano",
    setting_terrain %>% str_detect("Snow") ~ "Neve",
    setting_terrain %>% str_detect("Space") ~ "Espaço",
    setting_terrain %>% str_detect("Swamp") ~ "Pantâno",
    setting_terrain %>% str_detect("Moon") ~ "Lua",
    setting_terrain %>% str_detect("Coast") ~ "Costa",
    setting_terrain %>% str_detect("Air") ~ "Ar",
    setting_terrain %>% str_detect("Cave") ~ "Caverna",
    setting_terrain %>% str_detect("Desert") ~ "Deserto",
    TRUE ~ setting_terrain
  ))

#Preparação para o gráfico:
D$trap_work_first <- as.character(D$trap_work_first)
D <- D %>%
  filter(D$trap_work_first != "NA")
D <- D %>%
  mutate(trap_work_first = case_when(
    trap_work_first == "FALSE" ~ "Não Funcionou",
    trap_work_first == "TRUE" ~ "Funcionou"))

#Tabela de Frequência:
C <- table(D)

#3 terrenos com atvação mais frequente:
D <- D %>%
  filter(setting_terrain %in% c("Urbano", "Rural", "Floresta","Urbano", "Rural", "Floresta"))

#Conseguir frequência relativa:
M <- D %>%
  group_by(setting_terrain,trap_work_first) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n))

M$freq <- round(M$freq,4)
M$freq <- M$freq*100

porcentagens <- str_c(M$freq, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(M$n, " (", porcentagens, ")"))

#Arrumar nome das variáveis:
M <- rename(M, "Armadilha" = trap_work_first)

#Gráfico:
K <- ggplot(M) +
  aes(
    x = fct_reorder(setting_terrain, n, .desc = T), y = n,
    fill = Armadilha, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Terreno", y = "Frequência") +
  estat_theme()
K + scale_y_continuous(limits = c(0, 70))
ggsave("Entrega 3 (12.05)/Funcionamento_armadilhaxTerreno.pdf", width = 158, height = 93, units = "mm")

#Tabela de frequência:
C
