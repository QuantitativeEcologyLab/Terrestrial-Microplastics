
# Load required packages
library(viridis)
library(ggplot2)
library(gridExtra)

# Load MPdf dataset 
MPdf <- read.csv(".Data/MPdf.csv")

#--------------------------------------------------------------------------
# Current trends in terrestrial microplastics   
#--------------------------------------------------------------------------   

#HFI 
size_limits <- c(0.75, 2.5)

HFI_current <- 
  ggplot(data = MPdf, 
         aes(y = Items_kg +1, x = HFI, size = Weights)) +
  geom_point(aes(col = Elevation_m), alpha = 0.8, pch = 16) +
  scale_size_continuous(range = size_limits) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=8, family = "sans", face = "bold"),
        axis.title.x = element_text(size=8, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        legend.key.size = (unit(0.5, "cm")),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm")) +
  guides(size = "none") +
  scale_x_continuous(limits = c(0,1),expand = c(0,0.02)) +
  scale_y_log10(breaks = c(1+1,10+1,100+1,1000+1,10000+1),
                labels = c(1,10,100,1000,10000)) +
  scale_colour_gradient(low = "blue", high = "red") +
  ylab("MP Concentrations (items/kg)") +
  xlab("HFI") +
  annotation_logticks(outside = TRUE, #should have tick marks outside
                      size = 0.3,
                      short = unit(0.05, "cm"),
                      mid = unit(0.05, "cm"),
                      long = unit(0.1, "cm")) +
  ggtitle("A)") +
  geom_smooth(method = "gam",
              color = "grey17",
              formula = y ~ s(x),
              method.args = list(family = tw),
              se = FALSE)


#Depth
depth_current <- 
  ggplot(data = MPdf, 
         aes(y = Items_kg + 1, x = log10(Max_Depth_cm), size = Weights)) +
  scale_size_continuous(range = size_limits) +
  geom_point(aes(col = Elevation_m), alpha = 0.8, pch = 16) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=8, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        legend.key.size = (unit(0.5, "cm")),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm")) +
  guides(size = "none") +
  scale_y_log10(breaks = c(1+1,10+1,100+1,1000+1,10000+1),
                labels = c(1,10,100,1000,10000)) +
  scale_colour_gradient(low = "blue", high = "red") +
  ylab("MP Concentrations (items/kg)") +
  xlab("Soil Depth (cm)") +
  annotation_logticks(outside = TRUE,
                      size = 0.3,
                      short = unit(0.05, "cm"),
                      mid = unit(0.05, "cm"),
                      long = unit(0.1, "cm")) +
  ggtitle("B)") +
  geom_smooth(method = "gam",
              color = "grey17",
              formula = y ~ s(x),
              method.args = list(family = tw),
              se = FALSE)


#Elevation
elev_current <- 
  ggplot(data = MPdf, 
         aes(y = Items_kg+1, x = log10(Elevation_m), size = Weights)) +
  scale_size_continuous(range = size_limits, name = NULL) +
  geom_point(col = "blue", alpha = 0.8, pch = 16) +
 # scale_color_viridis_d(option = "turbo") +
  scale_x_continuous(breaks = c(0,1,2,3),
                     labels = c(1, 10, 100, 1000)) +
  scale_y_log10(breaks = c(1+1,10+1,100+1,1000+1,10000+1),
                labels = c(1,10,100,1000,10000)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=8, family = "sans", face = "bold"),
        axis.title.x = element_text(size=8, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"), 
        legend.position = "none",
        legend.key.size = (unit(0.5, "cm")),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm")) +
  guides(size = "none") +
  ylab("MP Concentrations (items/kg)") +
  xlab("Elevation (m)") +
  geom_smooth(method = "gam",
              color = "grey17",
              formula = y ~ s(x),
              method.args = list(family = tw),
              se = FALSE) +
  ggtitle(expression(bold("C)")))


trends1 <-
  grid.arrange(HFI_current,depth_current,
               ncol = 2,
               nrow = 1)

trends <-
  grid.arrange(trends1, elev_current, 
               ncol=1,
               nrow=2,
               heights = c(8,5.5),
               widths = 5)

ggsave(trends, file = "C:/Users/lmills96/OneDrive - UBC/MSc Thesis Info/
       Global Analysis/Global MP Distribution/Figures/Current_Trends.png",
       width = 3.25, height = 3, dpi = 600, units = "in")

