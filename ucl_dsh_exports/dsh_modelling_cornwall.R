# read in necessary packages
library(tidyverse)
library(INLA)
library(spdep)
library(SpatialEpi)
library(ggplot2)
library(plyr)
library(dplyr)
library(readr)
library(sf)
library(rgdal)
library(tmap)
library(RSQLite)
library(tmaptools)
library(leaflet)
library(htmltools)
library(broom)
library(plotly) 
library(geojsonio) 
library(mapview) 
library(crosstalk)
library(viridis)
library(reshape2)
library(shinyjs)
library(janitor)
library(car)
library(corrplot)
library(shades)
library(ggpubr)

# Modelling Script

# First we need to create a neighbourhood matrix to define the spatial random effect
#by using the poly2nb() and the nb2INLA() functions of the spdep package (Bivand, 2019).
nb <- poly2nb(cornwall_lsoas)
head(nb)
#class(nb)

# create the neighbours matrix
nb2INLA("cornwall.adj", nb)
g <- inla.read.graph(filename = "cornwall.adj")
summary(g)
head(g)


# Now write the formula of the Bernardinelli model:
# Model 0 zero - just the unstructuctured spatial residual Vi
# I will then compute MORAN'S I to check for spatial autocorrelation in the RR
formula0 <- Y ~ log(pop_density) + log(green_access_prop) + log(green_access_dist) +
  f(area_v, model = "iid")

cornwall_model0 <- inla(formula0,
                        family= "poisson", data = all_vars, E=E,
                        control.compute = (list(dic = TRUE)),
                        control.predictor = list(compute = TRUE)
)
summary(cornwall_model0)
saveRDS(cornwall_model0, "cornwall_model0.rds")


# NOW for Moran's I
# We'll keep with the Queen's neighbourhood structure as it is the default (used here) 
# for the INLA models. We'll also use the Binary spatial weights

# Creating the binary spatial weights matric from the queens neighbourhood structure
lsoa_weights_m <- nb %>%
  nb2mat(., style = "B")
sum(lsoa_weights_m)

# changing the spatial weights matrix into a list as Moran's I requires it this way
lsoa_weights_m <- nb %>%
  nb2listw(., style = "B")

# Extract the residuals
head(cornwall_model0$summary.random$area_v)
residuals_vi <- cornwall_model0$summary.random$area_v[,"mean"]
head(residuals_vi)


# Going to use the Monte-Carlo simulation of Moran's I, rather than the basic analytical approach of 
# The analytical apprach is fast, but the MC simulative approach is a safer approach as it takes an 
# extra argument n, the number of simulations
I_Llsoa_binary <- moran.mc(x = residuals_vi, listw = lsoa_weights_m, nsim = 1000)   
I_Llsoa_binary


# Still with LOGGED COVARIATES 
#################            
# 1st only with the green access and popden covariates
formula1_log <- Y ~ log(pop_density) + log(green_access_prop) + log(green_access_dist) + 
  f(area_u, model = "besag", graph = g) +
  f(area_v, model = "iid")

cornwall_model1_log <- inla(formula1_log,
                            family = "poisson", data = all_vars, E = E,
                            control.compute = (list(dic = TRUE)),
                            control.predictor = list(compute = TRUE)
)
summary(cornwall_model1_log)
#saveRDS(cornwall_model1_log, "cornwall_model1_log.rds")


######### 1.5
# this model initially gave an error as 4 logged values became Inf's, suggesting that there were zeros or negative values
# After inspection it turns out there are neither negative values nor zeros in any of the columns,
# however, the social renting column has a few values that are close to zeo (e.g. 0.4) 
# so I will apply  log(UNEMPLOYED pc + 1) and see if it works

# MODEL 1.5 with the green access and popden covariates AND sociodemographic variables. I.e., without
# the health access variables
formula1.5_log <- Y ~ log(pop_density) + log(green_access_prop) + log(green_access_dist) + 
  log(unemployed_pc + 1) + log(socialrenting_pc + 1) + log(bame_pc) +
  f(area_u, model = "besag", graph = g) +
  f(area_v, model = "iid")

cornwall_model1.5_log <- inla(formula1.5_log,
                              family = "poisson", data = all_vars, E = E,
                              control.compute = (list(dic = TRUE)),
                              control.predictor = list(compute = TRUE)
)
summary(cornwall_model1.5_log)
#saveRDS(cornwall_model1.5_log, "cornwall_model1.5_log.rds")

