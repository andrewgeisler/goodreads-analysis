library(wesanderson)

# SET THEME FOR PLOTS -----------------------------------------------------
theme_set(
  theme_minimal() +
    theme(
      legend.title = element_blank(),
      axis.title = element_blank(),
      text = element_text(color = '#404040'),
      axis.text = element_text(color = '#404040', size = 8)
    )
)

color_pals <- c(
  "Moonrise2",
  "Moonrise1",
  "Moonrise3",
  "Royal1",
  "Royal2",
  "IsleofDogs1",
  "IsleofDogs2",
  "GrandBudapest1",
  "GrandBudapest2",
  "FantasticFox1",
  "Rushmore1",
  "Rushmore",
  "BottleRocket1",
  "BottleRocket2",
  "Zissou1",
  "Darjeeling1",
  "Darjeeling2",
  "Chevalier1",
  "Cavalcanti1"
)

color_pals <- map(color_pals, wes_palette) %>% unlist()

plot_color <- scale_color_manual(values = color_pals)
plot_fill <- scale_fill_manual(values = color_pals)