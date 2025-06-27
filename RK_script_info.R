
# -------------------------------------------------------------------------
## Predicting on the 'link' scale vs. 'response' scale:
# -------------------------------------------------------------------------

# The model is using family=tw(link = "log") - this means the model is going to 
# predict log(u) when on the link scale:
#   predict(model, type = "link") --> log(u)
#   predict(model, type = "response") --> u
# *Log is there because the Tweedie distribution is right-skewed 
# *and is strictly positive

# Kriging on the 'link' scale:
#  - Kriging assumes residuals are stationary and Gaussian
#  - Resdiuals on the link scale are often more symmetric, more normal, and 
#    have more stable variance
#     ** Note:
#    In a Tweedie distribution, the power parameter (p) dictates how the 
#    variance scales with the mean. In my model, the power parameter is 1.735, 
#    meaning that it is in between a Poisson and Gamma distribution. This means 
#    heteroskedasticity exists regardless as the variance does not EQUAL the 
#    mean, nor is it following a Gaussian distribution.
#      -> With p=1.735, the variance is increasing nonlinearly with the mean,
#         meaning there is decent heteroskedasticity. However, when you log-
#         transform (use the log link), it helps to stabalize the variance, 
#         making the residuals closer to homoskedastic
#      -> Log-transforming doesn't eliminate all heteroskedasticity, but it does
#         help to reduce it
#  - Staying on the link scale then exponentiating at the end helps to keep 
#    everything positive 
#  - Measures the multiplicative error (error that scales with the size of the
#    predicted values -- Log-normal, Gamma, Tweedie w/ log link) b/w observed
#    and predicted. These residuals (r) are Kriged, and the predicted residuals
#    are on the log-scale (Kriged values of the log-residual)
       
# Kriging on the 'response' scale:
#  - Could work if your residuals are small / variance is stable
#  - Some Kriged values are strongly negative and can result in negative RK 
#    values - this doesn't make sense for [MP] as particles can't be negative

# -------------------------------------------------------------------------
## RK script problems:
# -------------------------------------------------------------------------

# var1_resampled has 3 layers in the raster - when performing OK, the output
# raster typically contains:
#   Layer 1 (var1.pred): the predicted residual values from the Kriging
#   Layer 2 (var1.var): the Kriging variance (the uncertainty of the predicted residuals at each unsampled location)
#   Layer 3 (geometry): layer that stores geometric information (coordinates of prediction points / spatial features)

# Need to specifiy which layer you are using for the final RK map - should be 
# the predicted residuals