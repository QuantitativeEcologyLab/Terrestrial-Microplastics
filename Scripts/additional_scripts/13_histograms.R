
# Histogram could also be density and overlay histogram w/ frequency on it 

# Load required packages 
library(ggplot2)
library(khroma)

# Load MPdf dataset 
MPdf <- read.csv(".Data/MPdf.csv")

#----------------------------------------------------------------------
# Plots
#----------------------------------------------------------------------

hist(MPdf$Max_Depth_cm)
hist(MPdf$HFI)
hist(MPdf$Elevation_m)

par(mar = c(5, 4, 4, 2))
    
bright <- color("bright")
plot_scheme(bright(7), colours = TRUE, names = TRUE, size = 0.9)

hist(MPdf$Max_Depth_cm,
     main = "Sampled Depth",
     xlab = "Max Depth (cm)")


#par(mar = c(1, 4, 3, 2))
par(mar = c(5, 4, 2, 2))

#HFI frequency  
ggplot(MPdf, aes(x = HFI)) +
  geom_histogram(binwidth = 0.05, fill = "#924900", color = "#924900", boundary = 0.2, alpha = 0.4) +
  labs(title = "",
       x = "HFI",
       y = "Frequency") +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=15, family = "sans", face = "bold"),
        axis.title.x = element_text(size=15, family = "sans", face = "bold"),
        axis.text.y = element_text(size=15, family = "sans"),
        axis.text.x  = element_text(size=15, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        legend.key.size = (unit(0.5, "cm")),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.5,0.2,0.2), "cm")) +
  coord_cartesian(xlim = c(0, 1))


#Soil depth frequency  
ggplot(MPdf, aes(x = Max_Depth_cm)) +
  geom_histogram(binwidth = 10, fill = "#924900", color = "#924900", boundary = 0.2, alpha = 0.4) +
  labs(title = "",
       x = "Soil Depth (cm)",
       y = "Frequency") +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=15, family = "sans", face = "bold"),
        axis.title.x = element_text(size=15, family = "sans", face = "bold"),
        axis.text.y = element_text(size=15, family = "sans"),
        axis.text.x  = element_text(size=15, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        legend.key.size = (unit(0.5, "cm")),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.5,0.2,0.2), "cm")) 

    
#par(mar = c(5, 4, 4, 2))

par(mar = c(5, 4, 2, 2))


#Elevation frequency  
ggplot(MPdf, aes(x = Elevation_m)) +
    geom_histogram(binwidth = 100, fill = "#924900", color = "#924900", boundary = 0.2, alpha = 0.4) +
    labs(title = "",
      x = "Elevation (m)",
      y = "Frequency") +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=15, family = "sans", face = "bold"),
        axis.title.x = element_text(size=15, family = "sans", face = "bold"),
        axis.text.y = element_text(size=15, family = "sans"),
        axis.text.x  = element_text(size=15, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        legend.key.size = (unit(0.5, "cm")),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.5,0.2,0.2), "cm")) 


ggsave("./Figures/combined_proj_plot.png", plot = trends22, width = 8, height = 6,
       dpi = 600, units = "in")

