
message("Run GAM model")

# Load Required Packages
library(mgcv)
library(gratia)

# Load MPdf dataset 
MPdf <- read.csv(".Data/MPdf.csv")

# GAM model:
model <- gam(Items_kg ~
               # global terms
               s(HFI, k = 10, bs = "ad") + 
               s(sqrt(max_depth_cm), k = 5) +
               s(elevation_m, k = 6) + 
               #ti(Elevation_km, HFI, k = 5, bs = "tp"),# + #don't need study in here b/c interaction term
               #ti(Max_Depth_cm, HFI, k = 5, bs = "tp") +
               # study-level terms
               s(study, bs = 're'), #random int - it doesn't really have random slopes 
             #weights = Weights,
             family = tw(link = 'log'),
             data = MPdf,
             method = "REML",
             na.action = na.fail)

plot(model, pages = 1, scale = 0, scheme = 1)

draw(model)
draw(model, select = "s(HFI)")
draw(model, select = "s(sqrt(max_depth_cm))")
draw(model, select = "s(elevation_m)")

# Note: the 'partial effect' on the y-axis of the above plot represents the 
# contribution of that smooth term WRT the predicted response on the scale of
# the linear predictor (here, the log of Items_kg). The partial effect is the 
# change in the log-predicted response due to the smooth term for that term, 
# while all other variables are held constant in the model. It shows how the 
# response would change as you vary one specific variable. 
    # It is NOT the actual predicted [MP], but the contribution of this specific
    # variable to the prediction. 

    # E.g., let's say for a location at 500 meters elevation, partial effect = 0
      # and at a location at 4000 meters, partial effect = 0.3.
      # The change in elevation from 500 m to 4000 m is thus:
            # exp(0.3) ≈ 1.35...
      # meaning a 35% increase in the predicted [MP], assuming all other
      # variables stay the same.
    
    # It isolates the effect of one variable after accounting for the effects 
    # of other variables -- doesn't show the full prediction, just the piece of 
    # the prediction associated to that one term.

    # All 'parital effects' together then sum to form the log of the predicted
    # response, which is then exponentiated to get Items_kg.

summary(model)
  #artificially explaining an excessive amount which impacts kriging 
  #(by a non-deterministic pattern) -
  #right now model is saying I've explained 92% - doesn't make sense given that 
  #the rest of the variables weren't explaining that much
  #partial effect on the y

saveRDS(model, file = './Data/MPdf_model.RDS')