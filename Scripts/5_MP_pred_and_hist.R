library(grid)

#### Depth ####

pred_depth <-
  ggplot() +
  geom_point(aes(Max_Depth_cm, Items_kg), MPdf, col = "#924900") +
  geom_ribbon(aes(Max_Depth_cm, ymin = lower_95, ymax = upper_95), prediction_depth_ci, fill = '#924900', alpha = 0.4) +
  geom_line(aes(Max_Depth_cm, mu), prediction_depth, col = 'black', lwd = 1) +
  scale_y_log10(labels = scales::label_number()) +
  scale_x_log10() +
  labs(y=NULL, x= NULL) +
  #ylab("MP Concentrations (items/kg)") +
  # xlab("Soil Depth (cm)") 
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=15, family = "sans", face = "bold"),
        axis.title.x = element_text(size=15, family = "sans", face = "bold"),
        axis.text.y = element_text(size=15, family = "sans"),
        axis.text.x  = element_text(size=15, family = "sans"),
        plot.title = element_text(size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        legend.key.size = (unit(0.5, "cm")),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.5,0.2,0.2), "cm")) +
  ggtitle("E)")

hist_depth <- 
  ggplot(MPdf, aes(x = Max_Depth_cm)) +
  geom_histogram(binwidth = 10, fill = "#924900", color = "#924900", boundary = 0.2, alpha = 0.4) +
  labs(title = "",
       x = NULL,
       #x = "Soil Depth (cm)",
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
  ggtitle("F)")


x_depth <- textGrob("Soil Depth (cm)", rot = 360, gp = gpar(fontsize = 15))

depth_pred_hist <-
  grid.arrange(pred_depth, hist_depth,
               ncol = 2,
               nrow = 1,
               bottom = x_depth)

ggsave("depth_pred_hist.png", plot = depth_pred_hist, width = 8, height = 3,
       dpi = 600, units = "in")



#### HFI ####

pred_HFI <-
  ggplot() +
  geom_point(aes(HFI, Items_kg), MPdf, col = "#924900") +
  geom_ribbon(aes(HFI, ymin = lower_95_HFI, ymax = upper_95_HFI), prediction_HFI_ci, fill = "#924900", alpha = 0.4) +
  geom_line(aes(HFI, mu), prediction_HFI, col = 'black', lwd = 1) +
  scale_y_log10(labels = scales::label_number()) +
  labs(y=NULL, x= NULL) +
  #ylab("MP Concentrations (items/kg)") +
  # xlab("HFI") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=15, family = "sans", face = "bold"),
        axis.title.x = element_text(size=15, family = "sans", face = "bold"),
        axis.text.y = element_text(size=15, family = "sans"),
        axis.text.x  = element_text(size=15, family = "sans"), 
        legend.position = "none",
        legend.key.size = (unit(0.5, "cm")),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"),
        plot.title = element_text(size = 12, family = "sans", face = "bold")) +
  ggtitle("A)")

hist_HFI <-
  ggplot(MPdf, aes(x = HFI)) +
  geom_histogram(binwidth = 0.05, fill = "#924900", color = "#924900", boundary = 0.2, alpha = 0.4) +
  labs(title = "",
       x = NULL,
       #x = "HFI",
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
  coord_cartesian(xlim = c(0, 1)) +
  ggtitle("B)")

x_HFI <- textGrob("HFI", rot = 360, gp = gpar(fontsize = 15))

HFI_pred_hist <-
  grid.arrange(pred_HFI, hist_HFI,
               ncol = 2,
               nrow = 1,
               bottom = x_HFI)

ggsave("HFI_pred_hist.png", plot = HFI_pred_hist, width = 8, height = 6,
       dpi = 600, units = "in")


#### Elev ####

pred_elev <-
  ggplot() +
  geom_point(aes(Elevation_km, Items_kg), MPdf, col = "#924900") +
  geom_ribbon(aes(Elevation_km, ymin = lower_95_elev, ymax = upper_95_elev), prediction_elev_ci, fill = "#924900", alpha = 0.4) +
  geom_line(aes(Elevation_km, mu), prediction_elev, col = 'black', lwd = 1) +
  scale_y_log10(labels = scales::label_number()) +
  scale_x_log10() +
  labs(y=NULL, x= NULL) +
  #ylab("MP Concentrations (items/kg)") +
  # xlab("Elevation (m)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=15, family = "sans", face = "bold",
                                    margin = margin(t= 1500)),
        axis.title.x = element_text(size=15, family = "sans", face = "bold"),
        axis.text.y = element_text(size=15, family = "sans"),
        axis.text.x  = element_text(size=15, family = "sans"), 
        legend.position = "none",
        legend.key.size = (unit(0.5, "cm")),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"),
        plot.title = element_text(size = 12, family = "sans", face = "bold")) +
  ggtitle("C)")


hist_elev <-
  ggplot(MPdf, aes(x = Elevation_km)) +
  geom_histogram(binwidth = 100, fill = "#924900", color = "#924900", boundary = 0.2, alpha = 0.4) +
  labs(title = "",
       x = NULL,
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
  ggtitle("D)")


x_elev <- textGrob("Elevation (m)", rot = 360, gp = gpar(fontsize = 15))

elev_pred_hist <-
  grid.arrange(pred_elev, hist_elev,
               ncol = 2,
               nrow = 1,
               bottom = x_elev)

ggsave("elev_pred_hist.png", plot = elev_pred_hist, width = 8, height = 6,
       dpi = 600, units = "in")


yleft <- textGrob("MP Concentration (items/kg)", rot = 90, gp = gpar(fontsize = 20))


#### All 3 plots in one ####

pred_hist <- 
  grid.arrange(
  HFI_pred_hist, 
  elev_pred_hist, 
  depth_pred_hist, 
  ncol = 1,
  left = yleft)

ggsave("pred_hist.png", plot = pred_hist, width = 8, height = 6,
       dpi = 600, units = "in")
