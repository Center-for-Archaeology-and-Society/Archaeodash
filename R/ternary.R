# library(plotly)
# library(tidyverse)
# fn  = "C:\\Users\\rjbischo\\Dropbox (ASU)\\Projects\\Shipley Collection\\aa complete collection sourced 5 29 2024 final vcheck 2.xlsx"
# data <- rio::import(fn)
# sources = data$Sources %>% unique() %>% sample(3)
# df = data %>%
#   filter(Sources %in% sources)
# scale01 <- function(x) {
#   (x - min(x)) / (max(x) - min(x))
# }
# df = df %>%
#   mutate_at(vars(`Rb/Zr`, `Y/Zr`, `Nb/Zr`), scale01) %>%
#   slice_sample(n = 100)
# fig <- plot_ly(
#   df,
#   a = ~`Rb/Zr`,
#   b = ~`Y/Zr`,
#   c = ~`Nb/Zr`,
#   color = ~ Sources,
#   type = 'scatterternary',
#   mode = 'markers'
# )
#
# fig <- fig %>%
#   layout(
#     ternary = list(
#       sum = 1,
#       aaxis = list(title = "Rb/Zr"),
#       baxis = list(title = "Y/Zr"),
#       caxis = list(title = "Nb/Zr")
#     ),
#     title = "Ternary Plot Example"
#   )
#
# fig
