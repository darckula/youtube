library(tidyverse)
library(extrafont)


theme_default <- theme_set(theme_light())
theme_update(
    text         = element_text(family = "Bahnschrift")
  , plot.title   = element_text(size = 20)
  , axis.text    = element_text(size = 15),
  , legend.text  = element_text(size = 12))


acc_trafico <- tribble(
  ~zona, ~victimas, ~fallecidos,
  "vias interurbanas", 900, 134,
  "travesias", 254, 19,
  "calles", 13248, 228
)


acc_trafico <- 
  acc_trafico %>%
  mutate(
    ratio        = 100*fallecidos/victimas,
    no_fallecidos = victimas - fallecidos)

acc_trafico <- 
  acc_trafico %>% 
    pivot_longer(
      cols = c(fallecidos, no_fallecidos),
      names_to = "tipo",
      values_to = "num_personas")

acc_trafico %>% 
  ggplot(aes(x=zona, y=num_personas, fill=tipo)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_brewer(palette = 3, type = "qual", direction = -1) +
  geom_text(aes(y = 0.75, label = paste(round(ratio,2), "%"))
            , colour = "blue4"
            , family = "Bahnschrift"
            , size = 8) +
  xlab(NULL) +
  ylab(NULL) +
  labs(
    title = "Proporción de la mortalidad en atropellos a peatones",
    subtitle = "Datos de la DGT 2019",
    caption = "Twitter: @darckula") +
  scale_y_continuous(labels = c("0%", "25%", "50%", "75%", "100%")) +
  theme(legend.position = "bottom", legend.title = element_blank())
