
# ---------- Trying to find optimal values for variogram models ----------

# Lots of issues trying to get optimal parameters here. A lot of convergence
# errors (no convergence after 200 iterations) and was unable to really find 
# optimal values from this... the approach directly below have suggested 
# values but none of the variograms looked good with those suggested values. 
# The initial variogram with the parameters I initially estimated were then   
# compared to the suggested values (SSErr compared) and none of suggested values
# ended up being used. 



# ---------------------------- SSE Sph -----------------------------

model_type_s <- "Sph"

grid_search <- function(empirical, model_type_s, nugget_range, psill_range, range_range) {
  best_params <- NULL
  best_sse <- Inf
  
  for (nugget in nugget_range) {
    for (psill in psill_range) {
      for (range in range_range) {
        model_vgm <- vgm(psill = psill, model = model_type, range = range, nugget = nugget)
        fit_vg <- fit.variogram(empirical, model_vgm)
        sse <- sum((empirical$gamma - variogramLine(fit_vg, maxdist = max(empirical$dist))$gamma)^2)
        
        if (sse < best_sse) {
          best_sse <- sse
          best_params <- c(nugget, psill, range)
        }
      }
    }
  }
  
  return(list(best_params = best_params, best_sse = best_sse))
}

# Example usage
nugget_range_s <- seq(0, 1e10, length.out = 10)
psill_range_s <- seq(4e4, 9e7, length.out = 10)
range_range_s <- seq(100000, 100000, length.out = 10)

grid_result_s <- grid_search(mp.vg, "Sph", nugget_range_s, psill_range_s, range_range_s)
print(grid_result_s)



mp.vg.fit.sph1 <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE, psill = 90000000, range = 500000))
mp.vg.fit.sph2 <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE, psill = 40000000, range = 500000))
plot(mp.vg, mp.vg.fit.sph1)
plot(mp.vg, mp.vg.fit.sph2)


results.sse.s <- data.frame(model = c("sph1", "sph2"),
                            SSErr = c( attr(mp.vg.fit.sph1, "SSErr"),
                                       attr(mp.vg.fit.sph2, "SSErr")))

results.sse.s <- results.sse[order(results.sse.s$SSErr), ] 
results.sse.s   

#sph1 best sse

# ---------------------------- SSE Exp -----------------------------

model_type_e <- "Exp"

grid_search <- function(empirical, model_type_e, nugget_range, psill_range, range_range) {
  best_params <- NULL
  best_sse <- Inf
  
  for (nugget in nugget_range) {
    for (psill in psill_range) {
      for (range in range_range) {
        model_vgm <- vgm(psill = psill, model = model_type, range = range, nugget = nugget)
        fit_vg <- fit.variogram(empirical, model_vgm)
        sse <- sum((empirical$gamma - variogramLine(fit_vg, maxdist = max(empirical$dist))$gamma)^2)
        
        if (sse < best_sse) {
          best_sse <- sse
          best_params <- c(nugget, psill, range)
        }
      }
    }
  }
  
  return(list(best_params = best_params, best_sse = best_sse))
}

# Example usage
nugget_range_e <- seq(0, 1e10, length.out = 10)
psill_range_e <- seq(4e4, 9e8, length.out = 10)
range_range_e <- seq(70000, 70000, length.out = 10)

grid_result_e <- grid_search(mp.vg, "Exp", nugget_range_e, psill_range_e, range_range_e)
print(grid_result_e)



mp.vg.fit.exp1 <- fit.variogram(mp.vg, vgm("Exp", nugget = TRUE, psill = 99000000, range = 73000))
mp.vg.fit.exp2 <- fit.variogram(mp.vg, vgm("Exp", nugget = TRUE, psill = 30002666, range = 70000))
plot(mp.vg, mp.vg.fit.exp1)
plot(mp.vg, mp.vg.fit.exp2)


results.sse.e <- data.frame(model = c("exp1", "exp2"),
                            SSErr = c( attr(mp.vg.fit.exp1, "SSErr"),
                                       attr(mp.vg.fit.exp2, "SSErr")))

results.sse.e <- results.sse.e[order(results.sse.e$SSErr), ] 
results.sse.e  

#exp1 best sse

# ---------------------------- SSE Gau -----------------------------

model_type_g <- "Gau"

grid_search <- function(empirical, model_type_g, nugget_range, psill_range, range_range) {
  best_params <- NULL
  best_sse <- Inf
  
  for (nugget in nugget_range) {
    for (psill in psill_range) {
      for (range in range_range) {
        model_vgm <- vgm(psill = psill, model = model_type, range = range, nugget = nugget)
        fit_vg <- fit.variogram(empirical, model_vgm)
        sse <- sum((empirical$gamma - variogramLine(fit_vg, maxdist = max(empirical$dist))$gamma)^2)
        
        if (sse < best_sse) {
          best_sse <- sse
          best_params <- c(nugget, psill, range)
        }
      }
    }
  }
  
  return(list(best_params = best_params, best_sse = best_sse))
}

# Example usage
nugget_range_g <- seq(0, 1e10, length.out = 10)
psill_range_g <- seq(4e4, 9e9, length.out = 10)
range_range_g <- seq(5000000, 5000000, length.out = 10)

grid_result_g <- grid_search(mp.vg, "Exp", nugget_range_g, psill_range_g, range_range_g)
print(grid_result_g)


mp.vg.fit.gau1 <- fit.variogram(mp.vg, vgm("Gau", nugget = TRUE, psill = 199999999, range = 5000000))
mp.vg.fit.gau2 <- fit.variogram(mp.vg, vgm("Gau", nugget = TRUE, psill = 4e4, range = 5000000))
plot(mp.vg, mp.vg.fit.gau1)
plot(mp.vg, mp.vg.fit.gau2)


results.sse.g <- data.frame(model = c("Gau1", "Gau2"),
                            SSErr = c( attr(mp.vg.fit.gau1, "SSErr"),
                                       attr(mp.vg.fit.gau2, "SSErr")))

results.sse.g <- results.sse.g[order(results.sse.g$SSErr), ] 
results.sse.g  

#gau1 best sse

# ---------------------------- SSE Lin -----------------------------

model_type_l <- "Lin"

grid_search <- function(empirical, model_type_l, nugget_range, psill_range, range_range) {
  best_params <- NULL
  best_sse <- Inf
  
  for (nugget in nugget_range) {
    for (psill in psill_range) {
      for (range in range_range) {
        model_vgm <- vgm(psill = psill, model = model_type, range = range, nugget = nugget)
        fit_vg <- fit.variogram(empirical, model_vgm)
        sse <- sum((empirical$gamma - variogramLine(fit_vg, maxdist = max(empirical$dist))$gamma)^2)
        
        if (sse < best_sse) {
          best_sse <- sse
          best_params <- c(nugget, psill, range)
        }
      }
    }
  }
  
  return(list(best_params = best_params, best_sse = best_sse))
}

# Example usage
nugget_range_l <- seq(0, 1e10, length.out = 10)
psill_range_l <- seq(5e7, 9e9, length.out = 10)
range_range_l <- seq(5000000, 5000000, length.out = 10)

grid_result_l <- grid_search(mp.vg, "Lin", nugget_range_l, psill_range_l, range_range_l)
print(grid_result_l)



mp.vg.fit.lin1 <- fit.variogram(mp.vg, vgm("Lin", nugget = TRUE, psill = 90000000, range = 500000))
mp.vg.fit.lin2 <- fit.variogram(mp.vg, vgm("Lin", nugget = TRUE, psill = 5e7, range = 5000000))
plot(mp.vg, mp.vg.fit.lin1)
plot(mp.vg, mp.vg.fit.lin2)


results.sse.l <- data.frame(model = c("lin1", "lin2"),
                            SSErr = c( attr(mp.vg.fit.lin1, "SSErr"),
                                       attr(mp.vg.fit.lin2, "SSErr")))

results.sse.l <- results.sse.l[order(results.sse.l$SSErr), ] 
results.sse.l

#lin1 best sse





mp.vg.fit.sph1 <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE, psill = 90000000, range = 500000))
mp.vg.fit.exp1 <- fit.variogram(mp.vg, vgm("Exp", nugget = TRUE, psill = 99000000, range = 73000))
mp.vg.fit.gau1 <- fit.variogram(mp.vg, vgm("Gau", nugget = TRUE, psill = 199999999, range = 5000000))
mp.vg.fit.lin1 <- fit.variogram(mp.vg, vgm("Lin", nugget = TRUE, psill = 90000000, range = 500000))







# ------------------------------ OTHER ATTEMPTS ------------------------------



vg.data <- st_as_sf(vect(data[,c("x", "y", "residuals"),],
                         geom=c("x", "y"),
                         crs="ESRI:53018"))

plot(test)


# Empirical variogram for the MP model residuals
mp.vg <- variogram(residuals ~ 1, data  = vg.data)
plot(mp.vg)




mp.vg.fit.sph <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE, psill = 80000000, range = 40000)) 
mp.vg.fit.lin <- fit.variogram(mp.vg, vgm("Lin", nugget = TRUE, psill = 80000000, range = 40000)) #psill = 40000000, range = 400000
mp.vg.fit.gau <- fit.variogram(mp.vg, vgm("Gau", nugget = TRUE, psill = 80000000, range = 40000)) #psill = 40000000, range = 400000)) 
mp.vg.fit.exp <- fit.variogram(mp.vg, vgm("Exp", nugget = TRUE, psill = 80000000, range = 40000)) #psill = 40000000, range = 400000)) 


mp.vg.fit.sph <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE, psill = 90000000, range = 500000))
mp.vg.fit.lin <- fit.variogram(mp.vg, vgm("Lin", nugget = TRUE, psill = 90000000, range = 500000))
mp.vg.fit.gau <- fit.variogram(mp.vg, vgm("Gau", nugget = TRUE, psill = 99000000, range = 2000000)) 
mp.vg.fit.exp <- fit.variogram(mp.vg, vgm("Exp", nugget = TRUE, psill = 99000000, range = 70000)) 

plot(mp.vg, mp.vg.fit.sph)
plot(mp.vg, mp.vg.fit.lin)
plot(mp.vg, mp.vg.fit.gau)
plot(mp.vg, mp.vg.fit.exp)

mp.vg.fit.sph <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE, psill = 90000000, range = 500000))
mp.vg.fit.sph <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE, psill = 85000000, range = 500000))
mp.vg.fit.sph <- fit.variogram(mp.vg, vgm("Sph", nugget = TRUE, psill = 60000000, range = 500000))

results <- data.frame(model = c("spherical", "linear", "Gaussian",
                                " exponential", "nugget"),
                      SSErr = c( attr(mp.vg.fit.sph, "SSErr"),
                                 attr(mp.vg.fit.lin, "SSErr"),
                                 attr(mp.vg.fit.gau, "SSErr"),
                                 attr(mp.vg.fit.exp, "SSErr"),
                                 attr(fit.nugget, "SSErr")))

results


# ----------- Trying to use optim function ----------------

# optim function - finds optimal value for SSErr
# provides algorithms for general-purpose optimizations

# Could not get optim() to work - kept giving back the same values I put into
# it, regardless of what I input


# Objective function for Spherical model
optim_function_sph <- function(par, empirical) {
  nugget <- par[1]
  psill <- par[2]
  range <- par[3]
  
  var_model <- vgm(psill = psill, model = "Sph", range = range, nugget = nugget)
  fit_vg <- fit.variogram(empirical, var_model)
  fit_line <- variogramLine(fit_vg, maxdist = max(empirical$dist))
  fit_values <- approx(fit_line$dist, fit_line$gamma, xout = empirical$dist, rule = 2)$y
  sse <- sum((empirical$gamma - fit_values)^2, na.rm = TRUE)
  
  return(sse)
}

# Objective function for Exponential model
optim_function_exp <- function(par, empirical) {
  tryCatch({
    nugget <- par[1]
    psill <- par[2]
    range <- par[3]
    
    var_model <- vgm(psill = psill, model = "Exp", range = range, nugget = nugget)
    fit_vg <- fit.variogram(empirical, var_model)
    fit_line <- variogramLine(fit_vg, maxdist = max(empirical$dist))
    fit_values <- approx(fit_line$dist, fit_line$gamma, xout = empirical$dist, rule = 2)$y
    sse <- sum((empirical$gamma - fit_values)^2, na.rm = TRUE)
    
    return(sse) 
  }
  , error = function(e) {
    print(paste("Error occurred:", e$message))
  })
}


# Initial parameters
initial_par <- c(0, 80000000, 500000)  # Example initial guess

# Optimize for Spherical model
opt_result_sph <- optim(
  par = initial_par,
  fn = optim_function_sph,
  empirical = mp.vg,
  method = "L-BFGS-B",
  lower = c(0, 0, 0),
  upper = c(Inf, Inf, Inf)
)

# Optimize for Exponential model
opt_result_exp <- optim(
  par = initial_par,
  fn = optim_function_exp,
  empirical = mp.vg,
  method = "L-BFGS-B",
  lower = c(0, 0, 0),
  upper = c(Inf, Inf, Inf)
)
print(opt_result_sph)


# Extract results for Spherical model
optimal_params_sph <- opt_result_sph$par
sse_sph <- opt_result_sph$value

# Extract results for Exponential model
optimal_params_exp <- opt_result_exp$par
sse_exp <- opt_result_exp$value





#model_types <- c("Sph", "Lin", "Gau", "Exp")

model_type <- "Sph"

optim_function <- function(param, empirical, model_type) {
  
  psill = param[1]
  range = param[2]
  nugget = param[3]
  
  var_model <- vgm(model_type, nugget, psill, range) 
  print(var_model)
  
  fit_vg <- fit.variogram(empirical, var_model)
  print(fit_vg)
  #fit.vg <- fit.variogram(mp.vg, var_model) #fit.variogram returns attributes like SSErr (INITIALLY HAD IT AS MP.VG, VAR_MODEL)
  
  fit_line <- variogramLine(fit_vg, maxdist = max(mp.vg$dist))
  print(head(fit_line))
  
  fit_val <- approx(fit_line$dist, fit_line$gamma, xout = mp.vg$dist, rule = 2)$y
  print(fit_val)
  
  sse <- sum((empirical$gamma - fit_val)^2, na.rm = TRUE)
  print(paste("SSE:", sse))
  
  return(sse)
  
}


original_par <- c(nugget = TRUE, psill = 85000000, range = 500000)

opt_result <- optim(par = original_par,
                    fn = optim_function,
                    empirical = mp.vg, # Additional argument for the empirical variogram
                    model_type = "Sph", # Additional argument for the empirical variogram
                    method = "Nelder-Mead",
                    lower = c(0,0,0),
                    upper = c(Inf, Inf, Inf))

opt_param <- opt_result$par
print(opt_param)







for(model_type in model_type){
  
  opt_result <- 
    optim(
      par = original_par,
      fn = optim_function,
      empirical = mp.vg,
      model_type = "Sph",
      method = "L-BFGS-B",
      lower = c(0,0,0),
      upper = c(Inf, Inf, Inf))
  
}


output[[model_type]] <- list(
  optimized_psill = opt_result$par[1],
  optimized_range = opt_result$par[2],
  optimized_nugget = opt_result$par[3],
  min_sse = opt_result$value)


# View the results
output











#emp_fit <- merge(mp.vg, fit_line, by = "dist", all.x)







original_par <- c(nugget = TRUE, psill = 850, range = 500000)

#model_types <- c("Sph", "Lin", "Gau", "Exp")

output <- list()


for(model_type in model_type){
  
  opt_result <- 
    optim(
      par = original_par,
      fn = optim_function,
      mp.vg = mp.vg,
      model_type = model_types)
  
}


output[[model_type]] <- list(
  optimized_psill = opt_result$par[1],
  optimized_range = opt_result$par[2],
  optimized_nugget = opt_result$par[3],
  min_sse = opt_result$value)


# View the results
output





model_type <- c("spherical", "linear", "Gaussian",
                " exponential", "nugget")


optim_function <- function(par, mp.vg) {
  
  SSErr <- results[[SSErr]]
  
  return(optim(SSErr))
  
}

optim_function(results)

#with(results, sum((mp.vg.fit.sph[1] + mp.vg.fit.lin[2] + mp.vg.fit.gau[3] + mp.vg.fit.exp[4])))
optim_function <- function(results, par) {
  with(results, sum((par[1] + par[2] * model - SSErr)^2))
}

optim_outcome <- optim(par = c(0,90000000),
                       fn = optim_function,
                       data = results)

# Plot variogram 
plot(mp.vg, mp.vg.fit.lin)
plot(mp.vg, mp.vg.fit.gau)
plot(mp.vg, mp.vg.fit.sph)
plot(mp.vg, mp.vg.fit.exp)


