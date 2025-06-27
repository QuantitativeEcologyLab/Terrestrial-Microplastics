
# Load required packages
library(grid)
library(gridExtra)
library(scales)

# Load MPdf dataset 
MPdf <- read.csv(".Data/MPdf.csv")

#----------------------------------------------------------------------
# Depth
#----------------------------------------------------------------------

pred_depth <-
  ggplot() +
  geom_point(aes(Max_Depth_cm, Items_kg), MPdf, col = "#924900") +
  geom_ribbon(aes(Max_Depth_cm, ymin = lower_95, ymax = upper_95), prediction_depth_ci, fill = '#924900', alpha = 0.4) +
  geom_line(aes(Max_Depth_cm, mu), prediction_depth, col = 'black', lwd = 1) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  scale_x_log10() +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=9, family = "sans"),
        axis.text.x  = element_text(size=9, family = "sans"),
        plot.title = element_text(size = 9, family = "sans", face = "bold"),
        legend.position = "right",
        legend.key.size = (unit(0.5, "cm")),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.5,0.2,0.2), "cm")) 

dens_depth <-
  ggplot(MPdf, aes(x = Max_Depth_cm, fill = "#924900")) +
  geom_histogram(aes(y = ..density..), alpha = 0.5, position = "identity", bins = 30) +
  theme_minimal() +
  labs(y = NULL, x=NULL) +
  #xlab("Soil Depth (cm)") +
  #ylab ("Density") +
  #scale_y_sqrt() +
  scale_fill_manual(values = c("#924900"),
                    labels = c("Samples taken")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.text.x = element_text(size=9, family = "sans", face = "bold", colour = "black"),
        axis.text.y  = element_text(size=9, family = "sans",  face = "bold"),
        strip.text.x = element_text(size=9, family = "sans", face = "bold"),
        plot.title = element_text(hjust = -0.05, size = 8, family = "sans", face = "bold"),
        legend.position = c(0.4, 0.85),
        legend.title = element_blank(),
        legend.text = element_text(size=9, family = "sans", face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.2, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.01,0.2,0.2), "cm"))


#x_depth <- textGrob("Soil Depth (cm)", rot = 360, gp = gpar(fontsize = 15))

depth_pred_dens <-
  grid.arrange(pred_depth, dens_depth,
               ncol = 2,
               nrow = 1)

ggsave("./Figures/depth_pred_dens.png", plot = depth_pred_dens, width = 8, height = 3,
       dpi = 600, units = "in")


#----------------------------------------------------------------------
# HFI
#----------------------------------------------------------------------

pred_HFI <-
  ggplot() +
  geom_point(aes(HFI, Items_kg), MPdf, col = "#924900") +
  geom_ribbon(aes(HFI, ymin = lower_95_HFI, ymax = upper_95_HFI), prediction_HFI_ci, fill = '#924900', alpha = 0.4) +
  geom_line(aes(HFI, mu), prediction_HFI, col = 'black', lwd = 1) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=9, family = "sans"),
        axis.text.x  = element_text(size=9, family = "sans"),
        plot.title = element_text(size = 9, family = "sans", face = "bold"),
        legend.position = "right",
        legend.key.size = (unit(0.5, "cm")),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.5,0.2,0.2), "cm"))
  

dens_HFI <- 
  ggplot(combined_HFI, aes(x = HFI, fill = dataset)) +
  geom_histogram(aes(y = ..density..), alpha = 0.5, position = "identity", bins = 30) +
  theme_minimal() +
  labs(x=NULL) +
  #xlab("HFI") +
  ylab ("Density") +
  #scale_y_sqrt() +
  scale_fill_manual(values = c("#515151", "#924900"),
                    labels = c("Global HFI", "Samples taken")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.text.x = element_text(size=9, family = "sans", face = "bold", colour = "black"),
        axis.text.y  = element_text(size=, family = "sans", face = "bold"),
        strip.text.x = element_text(size=9, family = "sans", face = "bold"),
        plot.title = element_text(hjust = -0.05, size = 8, family = "sans", face = "bold"),
        legend.position = c(0.4, 0.85),
        legend.title = element_blank(),
        legend.text = element_text(size=9, family = "sans", face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.2, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.01,0.2,0.2), "cm"))


HFI_pred_dens <-
  grid.arrange(pred_HFI, dens_HFI,
               ncol = 2,
               nrow = 1)

ggsave("./Figures/HFI_pred_dens.png", plot = HFI_pred_dens, width = 8, height = 6,
       dpi = 600, units = "in")


#----------------------------------------------------------------------
# Elevation
#----------------------------------------------------------------------

pred_elev <-
  ggplot() +
  geom_point(aes(Elevation_m, Items_kg), MPdf, col = "#924900") +
  geom_ribbon(aes(Elevation_m, ymin = lower_95_elev, ymax = upper_95_elev), prediction_elev_ci, fill = '#924900', alpha = 0.4) +
  geom_line(aes(Elevation_m, mu), prediction_elev, col = 'black', lwd = 1) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  scale_x_log10() +
  #labs(tag = "MP Concentrations (items/kg)") +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=9, family = "sans"),
        axis.text.x  = element_text(size=9, family = "sans"),
        plot.title = element_text(size = 9, family = "sans", face = "bold"),
        plot.tag = element_text(size = 12, family = "sans", face = "bold", angle = 90),
        plot.tag.position = c(-0.12, 0.5), # horizontal, vertical
        legend.position = "right",
        legend.key.size = (unit(0.5, "cm")),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.5,0.2,0.2), "cm")) 


dens_elev <- 
  ggplot(combined_elev, aes(x = Elevation_m, fill = dataset)) +
  geom_histogram(aes(y = ..density..), alpha = 0.5, position = "identity", bins = 30) +
  theme_minimal() +
  labs(x = NULL) +
  #xlab("Elevation (m)") +
  ylab ("Density") +
  scale_fill_manual(values = c("#515151", "#924900"),
                    labels = c("Global elevation", "Samples taken")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.text.x = element_text(size=9, family = "sans", face = "bold", colour = "black"),
        axis.text.y  = element_text(size=9, family = "sans",  face = "bold"),
        strip.text.x = element_text(size=9, family = "sans", face = "bold"),
        plot.title = element_text(hjust = -0.05, size = 8, family = "sans", face = "bold"),
        legend.position = c(0.4, 0.85),
        legend.title = element_blank(),
        legend.text = element_text(size=9, family = "sans", face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.2, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.01,0.2,0.2), "cm")) 

elev_pred_dens <-
  grid.arrange(pred_elev, dens_elev,
               ncol = 2,
               nrow = 1)

ggsave("./Figures/elev_pred_hist.png", plot = elev_pred_hist, width = 8, height = 6,
       dpi = 600, units = "in")


#----------------------------------------------------------------------
# Save all plots
#----------------------------------------------------------------------

pred_hist <- 
  grid.arrange(
    HFI_pred_dens, 
    elev_pred_dens, 
    depth_pred_dens, 
    ncol = 1)

ggsave("./Figures/pred_dens.png", plot = pred_dens, width = 8, height = 6,
       dpi = 600, units = "in")