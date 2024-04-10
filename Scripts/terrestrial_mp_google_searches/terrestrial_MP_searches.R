#setwd("C:/Users/lmills96/OneDrive - UBC/MSc Thesis Info/Global Analysis/Global Distribution/Figures")


search_data <- read.csv("C:/Users/lmills96/OneDrive - UBC/MSc Thesis Info/Global Analysis/terrestrial_MP_searches.csv")



library(khroma)
library(ggplot2)
library(mgcv)

bright <- color("bright")
plot_scheme(bright(7), colours = TRUE, names = TRUE, size = 0.9)



terrestrial_MP_searches <- 
  ggplot() +
  geom_line(data = search_data, aes(x = year, y = n_pub, col = keywords), 
            size = 1.5) +
  scale_color_manual(values = c("black", "#4477AA", "#924900")) + 
  ylab("Annual Publications") +
  xlab("Year") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=20, family = "sans", face = "bold"),
        axis.title.x = element_text(size=20, family = "sans", face = "bold"),
        axis.text.y = element_text(size=15, family = "sans"),
        axis.text.x  = element_text(size=15, family = "sans"),
        legend.key.size = (unit(1.5, "cm")),
        legend.title = element_blank(),
        legend.position = c(0.25,0.75),
        legend.text = element_text(size = 15, family = "sans", face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA))


ggsave("terrestrial_MP_searches.png", plot = terrestrial_MP_searches, width = 10,
       height = 6, units = "in", dpi = 300)

