# Load Required Packages
library(mgcv)

model <- gam(Items_kg ~
               # global terms
               s(HFI, k = 12, bs = "tp") + 
               s(Max_Depth_cm) +
               s(Elevation_km) +
               ti(Elevation_km, HFI, k = 5, bs = "tp") + #don't need study in here b/c interaction term
               #ti(Max_Depth_cm, HFI, k = 5, bs = "tp") +
               # study-level terms
               s(Study, bs = 're'), #random int - it doesn't really have random slopes 
             weights = Weights,
             family = tw(link = 'log'),
             data = MPdf,
             method = "REML")

plot(model, pages = 1)

# RESID <- MPdf$Items_kg - predict(model, type = "response")
# RESID2 <- residuals(model)
# 
# 
# plot(RESID ~ RESID2)
