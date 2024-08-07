# Load Required Packages
library(mgcv)

model <- gam(Items_kg ~
               # global terms
               s(HFI, k = 12, bs = "tp") + 
               s(Max_Depth_cm, k = 5) +
               s(Elevation_km, k = 10) +
               #ti(Elevation_km, HFI, k = 5, bs = "tp"),# + #don't need study in here b/c interaction term
               #ti(Max_Depth_cm, HFI, k = 5, bs = "tp") +
               # study-level terms
               s(Study, bs = 're'), #random int - it doesn't really have random slopes 
             #weights = Weights,
             family = tw(link = 'log'),
             data = MPdf,
             method = "REML")

plot(model, pages = 1, scheme = 3, scale = 0)

MPdf$residuals <- MPdf$Items_kg - predict(model, type = "response")
mean_resid <- aggregate(residuals ~ x + y, data = MPdf, mean)

# RESID2 <- residuals(model)
# plot(RESID ~ RESID2)


summary(model)
#artificially explaining an excessive amount which impacts kriging (by a non-deterministic pattern) - 
#right now model is saying I've explained 92% - doesn't make sense given that the rest of the variables weren't explaining that much 
#partial effect on the y 



# Averaging residuals from duplicated coordinates
total_dup <- sum(duplicated(MPdf[, c("x", "y")]))
total_dup


