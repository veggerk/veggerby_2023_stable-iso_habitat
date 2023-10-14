# required packages
library(patchwork)
library(ggplot2)
library(png)


# load in PNG images of each posterior plot
shiner_perch <- readPNG("figures/shiner_perch.png", native = TRUE)
stickleback <- readPNG("figures/stickleback.png", native = TRUE)
flatfish <- readPNG("figures/flatfish.png", native = TRUE)
staghorn_sculpin <- readPNG("figures/staghorn_sculpin.png", native = TRUE)
dungeness_crab <- readPNG("figures/dungeness_crab.png", native = TRUE)
shore_crab <- readPNG("figures/shore_crab.png", native = TRUE)

# combine into one multiplot
posterior_plots<-(wrap_elements(shiner_perch) + wrap_elements(stickleback)) /
  
  (wrap_elements(flatfish) + wrap_elements(staghorn_sculpin)) /

(wrap_elements(dungeness_crab) + wrap_elements(shore_crab))

# save
ggsave(plot = posterior_plots, 
       filename = "figures/figure_2.pdf", width = 7, height = 7.5, units = "in")

