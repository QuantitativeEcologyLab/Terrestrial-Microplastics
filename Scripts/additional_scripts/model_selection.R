
#--------------------------------------------------------------------------
# Loading required data
#-------------------------------------------------------------------------- 

# Load required package
library(ggplot2)
library(mgcv)

# Load MPdf dataset 
MPdf <- read.csv(".Data/MPdf.csv")


#--------------------------------------------------------------------------
# Determining if there should be interaction terms in model
#-------------------------------------------------------------------------- 

HFI_quantiles <- quantile(MPdf$HFI, probs = c(0, 1/3, 2/3, 1))

MPdf$HFI_categories <- cut(MPdf$HFI, breaks = HFI_quantiles, labels = c("Low", 
                                                                        "Med", "High"), include.lowest = TRUE)

Elev_int <- 
  ggplot(data = MPdf, aes(x = Elevation_m, y = Items_kg, 
                          color = HFI_categories)) +
  geom_point() +
  geom_smooth(method = "lm",
              #color = "grey1",
              #formula = y ~ s(x),
              #method.args = list(family = tw),
              se = FALSE) +
  scale_color_manual(values = c("Low" = "red", "Med" = "blue", "High" = "green"))

Depth_int <- 
  ggplot(data = MPdf, aes(x = Max_Depth_cm, y = Items_kg, color = HFI_categories)) +
  geom_point() +
  geom_smooth(method = "lm",
              # color = "grey1",
              # formula = y ~ s(x),
              # method.args = list(family = tw),
              se = FALSE) +
  scale_color_manual(values = c("Low" = "red", "Med" = "blue", "High" = "green"))


#--------------------------------------------------------------------------
# Model selection and top model 
#--------------------------------------------------------------------------     

# Load required packages for dredging model 
library(MuMIn)

# Initial model
model1 <- gam(Items_kg ~
                # global terms
                s(HFI, k = 12, bs = "tp") + 
                s(Max_Depth_cm) +
                s(Elevation_m) +
                ti(Elevation_m, HFI, k = 5, bs = "tp") + #don't need study in here b/c interaction term
                ti(Max_Depth_cm, HFI, k = 5, bs = "tp") +
                # study-level terms
                s(Study, bs = 're'), #random int - it doesn't really have random slopes 
              weights = Weights,
              family = tw(link = 'log'),
              data = MPdf,
              method = "REML",
              na.action = "na.fail")

summary(model1)
#plot(model1, scale = 0)


# Dredge the model
dredge(model1)


# Top model based on AIC 
model <- gam(Items_kg ~
               # global terms
               s(HFI, k = 12, bs = "tp") + 
               s(Max_Depth_cm) +
               s(Elevation_m) +
               #ti(Elevation_m, HFI, k = 5, bs = "tp") + #don't need study in here b/c interaction term
               #ti(Max_Depth_cm, HFI, k = 5, bs = "tp") +
               # study-level terms
               s(Study, bs = 're'), #random int - it doesn't really have random slopes 
             weights = Weights,
             family = tw(link = 'log'),
             data = MPdf,
             method = "REML")

summary(model)
#plot(model, scale = 0)
gam.check(model)

saveRDS(model, file = "C:/Users/lmills96/OneDrive - UBC/MSc Thesis Info/Global Analysis/Global MP Distribution/gam_model.RDS")





