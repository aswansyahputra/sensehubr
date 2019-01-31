plot_carto <- function(res_carto) {
  require(tidyverse)
  temp <- res.carto$nb.depasse/(ncol(res.carto$matrice) - 3)
  data_carto <- 
    temp %>% 
    `dimnames<-`(list(res_carto$f1, res_carto$f2)) %>% 
    reshape::melt() %>% 
    as_tibble() %>% 
    rename(
      x = X1,
      y = X2,
      z = value
    )
  rm(temp)
  
  data_sample <- 
    res.carto$matrice %>% 
    as_tibble() %>% 
    select(1:3) %>% 
    rename(sample = 1,
           x = 2,
           y = 3) %>% 
    mutate(z = 0)
  
  data_consumer <- 
    tibble(x = res.carto$abscis,
           y = res.carto$ordon,
           z = 0)
  
  p <- ggplot(mapping = aes(x = x, y = y, z = z)) +
    geom_tile(aes(fill = z), data = data_carto, show.legend = T) +
    geom_contour(data = data_carto, aes(col = z), colour = "white") +
    geom_point(data = data_sample, shape = 15, size = 3, col = "white") +
    geom_point(data = data_consumer, size = 1, col = "white") + 
    ggrepel::geom_text_repel(data = data_sample, aes(label = sample), col = "white") +
    scale_fill_gradient(low = "blue", high = "red") +
    labs(
      x = "Dim 1",
      y = "Dim 2",
      fill = ""
    ) +
    theme_minimal()
  p
}