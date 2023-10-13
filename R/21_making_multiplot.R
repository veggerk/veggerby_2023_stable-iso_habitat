library(patchwork)
library(ggplot2)



# walleye size plot

walleye_under500 <- readPNG("R/map-making-and-figure-merging-for-walleye/walleye_posterior_density_diet_p_below 500mm.png", native = TRUE)
walleye_over500 <- readPNG("R/map-making-and-figure-merging-for-walleye/walleye_posterior_density_diet_p_above 500mm.png", native = TRUE)


walleye_size<-wrap_elements(walleye_under500) + wrap_elements(walleye_over500) 

ggsave(plot = walleye_size, 
       filename = "figs/figure_3.pdf",width = 7,height = 2,units = "in")

