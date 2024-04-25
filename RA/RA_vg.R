
#----------------------------------------------------------------------
# Identifying spatial gaps in variogram 
#----------------------------------------------------------------------

# Plot in ggplot (this is to find data)

V = variogramLine(mp.vg.fit, maxdist = max(mp.vg$dist))
head(Vtest)

RA_Variogram <- 
  ggplot(mp.vg, aes(x = dist, y = gamma)) +
  geom_point() +
  geom_line(data = V) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "white", fill = "white"),
        plot.background = element_rect(colour = "white", fill = "white"),
        legend.position="top",
        legend.title.align=0.5,
        legend.title=element_text(color="black", size=14, family = "sans", 
                                  face= "bold"),
        legend.text=element_text(color="black", size=12, family = "sans"),
        legend.key.size = unit(0.25, "cm"),
        legend.key.width = unit(4, "cm"),
        legend.background=element_blank(),
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(limits = c(0,6e+06), expand = c(0,10000)) +
  scale_x_sqrt(breaks = c(10000+1,36000+1, 100000+1,500000+1, 670000+1, 1020000+1),
               labels = c(10000,36000, 100000,500000,670000, 1020000)) +
  geom_vline(xintercept = c(36000,670000,1020000), linetype = 2)

ggsave("RA_Variogram.png", plot = Variogram, width = 8, height = 6,
       dpi = 600, units = "in")
