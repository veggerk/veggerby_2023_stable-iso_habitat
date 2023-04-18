#### ch1_3_draw_PS_map_fig_1.R ####

## load packages

library(here)
library(rgdal)
library(broom)
library(ggplot2)
library(cowplot)
library(ggspatial)
library(grid)
library(patchwork)
library(png)

## figure save dir
fig_dir <- here("figures")

## load habitat pictures for multiplot
eelgrass <- readPNG(here("data", "04_clean data", "map_files", "eelgrass_wth_border.png"),
                    native = TRUE)
flipbag <- readPNG(here("data", "04_clean data", "map_files", "flipbags_wth_border.png"),
                   native = TRUE)


# alt version of pics with no black border
# eelgrass <- readPNG(here("data", "map_files","eelgrass.png"),
#                     native = TRUE)
# flipbag <- readPNG(here("data", "map_files","flipbags.png"),
#                    native = TRUE)



#### base map of Puget Sound ####

## load USA shape file
usa_spdf <- readOGR(dsn = here("data", "04_clean data", "map_files", "USA_adm0.shp"))
## convert to df(ish)
usa_spdf_fort <- tidy(usa_spdf)


## draw puget sound map and add annotations etc
puget_sound<-ggplot(usa_spdf_fort, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "gray50", fill = "#d9d9d9") +
  ylab("Latitude") +
  xlab("Longitude") +
  coord_fixed(xlim = c(-123.35, -121.92), ylim = c(47.5, 49.0), ratio = 1.3)+
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill="#c6dbef", color = "black"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_x_continuous(breaks = c(-123, -122.5),
                     expand = c(0, 0),
                     labels=c(expression(paste(123*degree,"W")),
                              expression(paste(122.5*degree,"W")))) +
  scale_y_continuous(breaks = seq(48, 48.5, 0.5),
                     expand = c(0, 0),
                     labels=c(expression(paste(48*degree,"N")),
                              expression(paste(48.5*degree,"N")))) +
  annotate("point", 
           x = -122.77, 
           y = 48.98,
           size = 2,
           color = "#e41a1c")+ 
  annotate("text", 
           label = "Drayton Harbor site", 
           x = -122.46, 
           y = 48.97,
           size = 5,
           color = "black")+
  annotate("point", 
           x = -122.46, 
           y = 48.61,
           size = 2,
           color = "#e41a1c")+ 
  annotate("point", 
           x = -122.5, 
           y = 48.5,
           size = 2,
           color = "#e41a1c")+ 
  annotate("text", 
           label = "Samish Bay site", 
           x = -122.2, 
           y = 48.61,
           size = 5,
           color = "black")+
  annotate("text", 
           label = "Padilla Bay site", 
           x = -122.25, 
           y = 48.5,
           size = 5,
           color = "black")+
  annotate("text", 
           label = "Seattle, WA",
           x = -122.13, 
           y = 47.6,
           size = 5,
           color = "black")+ 
  annotate("point", 
           x = -122.32, 
           y = 47.6,
           size = 2,
           color = "black")+ 
  annotation_north_arrow(
    location = "tl", which_north = "grid",
    pad_x = unit(0.1, "in"), pad_y = unit(0.4, "in"),
    style = north_arrow_orienteering(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"))+ 
  theme(text = element_text(size = 14))+ 
  theme(axis.text = element_text(size = 12))  

## combine maps
## combine all into one plot (figure 1)
patch<-wrap_elements(puget_sound)+
  (wrap_elements(eelgrass)/wrap_elements(flipbag)) + 
  plot_layout(tag_level = 'new') +
  plot_annotation(tag_levels = list(c(' ','eelgrass meadow','rows of oyster flipbags'))) & 
  theme(plot.tag.position = c(0.35, 0) ,
        plot.tag = element_text(size = 14, hjust = 0, vjust = 0))




## save the combined file as figure 1
ggsave(filename = file.path(fig_dir, "01_fig_PS_map.png"), 
       plot = patch,
       width = 12, 
       height = 8,
       dpi = 300)

