
# Note: Harmonized world soil database 2.0 - soil types from .mbd file 
# https://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/harmonized-world-soil-database-v20/en/



# Load required packages

library(sf)
library(terra)
library(RSQLite)

#-----------------------------------------------------------------------
# Import data
#-----------------------------------------------------------------------

HWSD_SMU <- read.csv("C:/Users/lmills96/OneDrive - UBC/MSc Thesis Info/Global Analysis/Terrestrial-Microplastics/Scripts/data/HWSD2_SMU.csv")
HWSD_SMU <- HWSD_SMU[,-c(1,3:9,11:23)] 

names(HWSD_SMU)[1] <- "HWSD2_SMU_ID"
names(HWSD_SMU)[2] <- "WRB2_code"

soildf <- MPdf[,-c(2:7,9,10,12,13,14)]

# Remove decimals from soildf "soil_type" column (could use 
# floor(soildf$soil_type) if wanting to round down)
soildf$soil_type <- as.integer(round(soildf$soil_type))

# Determine the number of unique values in column
length(unique(soildf$soil_type))
unique_soil_type <- unique(soildf$soil_type)
names(soildf)[3] <- "HWSD2_SMU_ID"


# Creating a new data frame
soil_data <- data.frame()

# Match values in soil_type column from HWSD_SMU to soildf (match values in a 
# column from one data frame to another data frame)
soil_data <- soildf %>%
  left_join(distinct(HWSD_SMU, HWSD2_SMU_ID, .keep_all = TRUE), by = "HWSD2_SMU_ID")

# Note: manually added soil type based on code to soil_data code 
# Assumed several soil types based off codes if there were NA's (closest soil type to existing code)
# (called soil_types_data)
write.csv(soil_data, "C:/Users/lmills96/OneDrive - UBC/MSc Thesis Info/Global Analysis/Terrestrial-Microplastics/Scripts/data/soil_data.csv")

#-----------------------------------------------------------------------
# Import soil type data
#-----------------------------------------------------------------------

soil_data <- read.csv("C:/Users/lmills96/OneDrive - UBC/MSc Thesis Info/Global Analysis/Terrestrial-Microplastics/Scripts/data/soil_data.csv")

#Frequency of soil types
soil_freq <-
  ggplot(soil_data, aes(x = soil)) +
  geom_bar(width = 0.5, fill = "#924900", color = "#924900") +
  labs(title = "",
       x = "Soil Type",
       y = "Frequency") +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=15, family = "sans", face = "bold"),
        axis.title.x = element_text(size=15, family = "sans", face = "bold"),
        axis.text.y = element_text(size=15, family = "sans"),
        axis.text.x  = element_text(size=15, family = "sans", angle = 90, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        legend.key.size = (unit(0.5, "cm")),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.5,0.2,0.2), "cm"))

ggsave("soil_freq.png", plot = soil_freq, width = 6,
       height = 4, units = "in", dpi = 300)


#Frequency of soil types within each country
soil_freq_country <- 
  ggplot(soil_data, aes(fill = Country, x = soil)) +
  geom_bar(width = 0.5) +
  labs(title = "",
       x = "Soil Type",
       y = "Frequency") +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=15, family = "sans", face = "bold"),
        axis.title.x = element_text(size=15, family = "sans", face = "bold"),
        axis.text.y = element_text(size=15, family = "sans"),
        axis.text.x  = element_text(size=15, family = "sans", angle = 75, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        legend.key.size = (unit(0.5, "cm")),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.5,0.2,0.2), "cm"))

ggsave("soil_freq_country.png", plot = soil_freq_country, width = 6,
       height = 4, units = "in", dpi = 300)





# Paring the types of soils to MP concentrations 
as.factor(MPdf$soil_type)

MP_conc_soil <- 
  ggplot(soil_data, aes(fill = Country, x = soil_type, y = Items_kg)) +
  geom_boxplot(width = 0.5) +
  labs(title = "",
       x = "Soil Type",
       y = "MP Concentration (items/kg)") +
  scale_y_log10() +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=15, family = "sans", face = "bold"),
        axis.title.x = element_text(size=15, family = "sans", face = "bold"),
        axis.text.y = element_text(size=15, family = "sans"),
        axis.text.x  = element_text(size=15, family = "sans", angle = 75, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        legend.key.size = (unit(0.5, "cm")),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.5,0.2,0.2), "cm"))


ggsave("MP_conc_soil.png", plot = MP_conc_soil, width = 15,
       height = 6, units = "in", dpi = 600)









# Add residuals to data frame...
# Percentage of clay / silt / sand... 
  # average % of each?
  # types of soil and microplastic movement - could see what is predominantly talked about in papers and use that 



# Other code for the data frame: 

# To see if and where a number appears in a data frame:
which(soildf== 7001, arr.ind=TRUE)

# Find the rows of a data frame where certain values appear in any of the columns
library(dplyr)
HWSD_SMU %>% filter_all(any_vars(. %in% c('5394')))

# Select rows where a certain number appears in any column
soildf %>% filter_all(any_vars(. %in% c(7001)))


