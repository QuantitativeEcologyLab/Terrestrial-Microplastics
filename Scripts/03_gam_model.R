# Load Required Packages
library(mgcv)

load("MPdf.rda")



#took sqrt out of elevation b/c it didn't make that much of a difference in the model
model <- gam(Items_kg ~
               # global terms
               s(HFI, k = 10, bs = "ad") + 
               s(sqrt(Max_Depth_cm), k = 5) +
               s(Elevation_km, k = 6) + 
               #ti(Elevation_km, HFI, k = 5, bs = "tp"),# + #don't need study in here b/c interaction term
               #ti(Max_Depth_cm, HFI, k = 5, bs = "tp") +
               # study-level terms
               s(Study, bs = 're'), #random int - it doesn't really have random slopes 
             #weights = Weights,
             family = tw(link = 'log'),
             data = MPdf,
             method = "REML",
             na.action = na.fail)
plot(model, pages = 1, scale = 0, scheme = 1)

#save(model, file = 'model.rda')


as.factor(MPdf$Study)
# 
# par(mar = c(3, 3, 3, 1) + 0.1)
# plot(model, pages = 1, scheme = 3, scale = 0)
# 
# summary(model)
# #artificially explaining an excessive amount which impacts kriging (by a non-deterministic pattern) - 
# #right now model is saying I've explained 92% - doesn't make sense given that the rest of the variables weren't explaining that much 
# #partial effect on the y 
# 
# library(MuMIn)
# dredge(model)




# MPdf$residuals <- MPdf$Items_kg - predict(model, type = "response")
# mean_resid <- aggregate(residuals ~ x + y, data = MPdf, mean)

# RESID2 <- residuals(model)
# plot(RESID ~ RESID2)