
# --------------------------------------------------------
  #HFI density plot
# --------------------------------------------------------

# Extract HFI global HFI values
global_HFI_values <- as.data.frame(hfi_raster, xy = TRUE)
names(global_HFI_values)[3] <- "HFI"

# Add a new column to indicate the dataset
global_HFI_values <- global_HFI_values %>% mutate(dataset = "Global HFI")
MPdf_2 <- MPdf %>% mutate(dataset = "MPdf")

# Combine the two datasets
combined_HFI <- bind_rows(global_HFI_values, MPdf_2)

# Plot
HFI_density <- 
  ggplot(combined_HFI, aes(x = HFI, fill = dataset)) +
  geom_histogram(aes(y = ..density..), alpha = 0.5, position = "identity", bins = 30) +
  theme_minimal() +
  xlab("HFI") +
  ylab ("Density") +
  scale_fill_manual(values = c("#515151", "red"),
                    labels = c("Global HFI", "Samples taken across HFI gradient")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.text.x = element_text(size=9, family = "sans", face = "bold", colour = "black"),
        axis.text.y  = element_text(size=9, family = "sans"),
        strip.text.x = element_text(size=9, family = "sans", face = "bold"),
        plot.title = element_text(hjust = -0.05, size = 8, family = "sans", face = "bold"),
        legend.position = c(0.8, 0.85),
        legend.title = element_blank(),
        legend.text = element_text(size=9, family = "sans", face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.2, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm")) 

ggsave("HFI_density.png", plot = HFI_density, width = 6,
       height = 4, units = "in", dpi = 300)


# --------------------------------------------------------
#Elevation density plot
# --------------------------------------------------------

# Extract HFI global HFI values
global_elev_values <- as.data.frame(Elevation_km, xy = TRUE)
names(global_HFI_values)[3] <- "HFI"

# Add a new column to indicate the dataset
global_HFI_values <- global_HFI_values %>% mutate(dataset = "Global HFI")
MPdf_2 <- MPdf %>% mutate(dataset = "MPdf")

# Combine the two datasets
combined_HFI <- bind_rows(global_HFI_values, MPdf_2)

# Plot
#elev_density <- 
  ggplot(combined_HFI, aes(x = HFI, fill = dataset)) +
  geom_histogram(aes(y = ..density..), alpha = 0.5, position = "identity", bins = 30) +
  theme_minimal() +
  xlab("HFI") +
  ylab ("Density") +
  scale_fill_manual(values = c("#515151", "red"),
                    labels = c("Global HFI", "Samples taken across HFI gradient")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.text.x = element_text(size=9, family = "sans", face = "bold", colour = "black"),
        axis.text.y  = element_text(size=9, family = "sans"),
        strip.text.x = element_text(size=9, family = "sans", face = "bold"),
        plot.title = element_text(hjust = -0.05, size = 8, family = "sans", face = "bold"),
        legend.position = c(0.8, 0.85),
        legend.title = element_blank(),
        legend.text = element_text(size=9, family = "sans", face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.2, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm")) 

ggsave("HFI_density.png", plot = HFI_density, width = 6,
       height = 4, units = "in", dpi = 300)


# --------------------------------------------------------
#Depth density plot
# --------------------------------------------------------

# No density plot for global depth 

#depth_density <- 
ggplot(MPdf, aes(x = Max_Depth_cm)) +
  geom_histogram(aes(y = ..density..), alpha = 0.5, position = "identity", bins = 30) +
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

ggsave("HFI_density.png", plot = HFI_density, width = 6,
       height = 4, units = "in", dpi = 300)

