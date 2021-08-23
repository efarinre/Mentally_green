# read in necessary packages
library(plyr)
library(tidyverse)
library(INLA)
library(spdep)
library(SpatialEpi)
library(rgdal)
library(tmap)
library(RSQLite)
library(tmaptools)
library(leaflet)
library(htmltools)
library(htmlwidgets)
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
library(RDS)
library(hrbrthemes)

# Replace *path-to-folder* with the directory to this cloned repository

# read in the postcode -> OA -> LSOA -> MSOA -> LA lookup table. filter for cornwall
cornwall1 <-  read_csv(("*path-to-folder*/data/PCD_OA_LSOA_MSOA_LAD_NOV20_UK_LU/PCD_OA_LSOA_MSOA_LAD_NOV20_UK_LU.csv"),
                      col_names = TRUE,
                      locale = locale(encoding = "Latin1")) %>%
  dplyr::filter(str_detect(ladnm, "Cornwall"))
#head(cornwall1)

# keep the code and name columns only
cornwall1 <- cornwall1[c("lsoa11cd", "lsoa11nm")]
# remove the duplicate lsoa rows, given that there is a row for each postcode
cornwall1 <- cornwall1[!duplicated(cornwall1[,c("lsoa11cd", "lsoa11nm")]),]


# Reading in the map of emgland with the lsoa boundaries
england_lsoas <- st_read("*path-to-folder*/data/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3-shp/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3.shp")
#head(england_lsoas)

#filter to just cornwall
cornwall_lsoas <- inner_join(england_lsoas, cornwall1, by = c("LSOA11CD" = "lsoa11cd"))
cornwall_lsoas <- cornwall_lsoas[c("LSOA11CD", "LSOA11NM")]
#head(cornwall_lsoas)
rm(england_lsoas)

# plot the outline
#plot(cornwall_lsoas)

# plotting just the outline of the shape
#cornwall_lsoas %>%
#  st_geometry() %>%
#  plot()


### time to start reading in the covariates for cornwall. Unlike London, Cornwall does not have a data store
# so some of the covariates wil have to be calculated here
# let's start by erading in the social renting % column which is already calculated
social_renting <- read_csv("*path-to-folder*/data/cornwall_socialrenting.csv")
names(social_renting) <- c("names", "codes", "socialrenting_pc")
#head(social_renting)

# now for the economic activity. you'll need to calculate the unemployment rate (economically active people who are unemployed)
# by divided the two columns and rounding to 1 decimal place to match the London rates
unemployment <- read_csv("*path-to-folder*/data/cornwall_economicactivity.csv")
names(unemployment) <- c("names", "code", "active", "unemployed")
unemployment$unemployed_pc <- round(unemployment$unemployed/unemployment$active, digits = 1)
unemployment <- unemployment[c("code", "unemployed_pc")]
#head(unemployment)

# merge the two together
covariates1 <- inner_join(unemployment, social_renting, by = c("code" = "codes"))
#(covariates1)
# reordering the columns
covariates1 <- covariates1[c("code", "names", "unemployed_pc", "socialrenting_pc")]


# now read in the persons per hectare data
popden <- read_csv("*path-to-folder*/data/england_popdensity.csv")
names(popden) <- c("names", "code", "pop_density")
popden <- popden[c("code", "pop_density")]

covariates1 <- inner_join(covariates1, popden, by = c("code" = "code"))
#head(covariates1)


# now read in the ethnicity grouping data
ethnicity <- read_csv("*path-to-folder*/data/cornwall_ethnicity.csv")
#head(ethnicity)
names(ethnicity) <- c("names", "codes", "white_pc", "mixed_pc", "asian_pc", "black_pc", "other_pc")
ethnicity$bame_pc <- round(ethnicity$mixed_pc + ethnicity$asian_pc + ethnicity$black_pc + 
                             ethnicity$other_pc, digits = 1)
#drop the names column
ethnicity <- ethnicity[c("codes", "white_pc", "mixed_pc", "asian_pc", "black_pc", "other_pc", "bame_pc")]
#head(ethnicity)

# merge with covariates1
covariates1 <- inner_join(covariates1, ethnicity, by = c("code" = "codes"))
#head(covariates1)


##### now read in the CDRC Access to Health Assets and Hazards dataset 
# there are raw scores, deciles and exponentiated scores. Different values were explored, hence the variety of covairates
covariates2 <- read_csv("*path-to-folder*/data/allvariableslsoawdeciles.csv")
#head(covariates2)
# filter for the lsoa code,  gp, a&e, pharmacy, green (passive) and green (active) accessbitlity columns
covariates2 <- covariates2[c("lsoa11", "gpp_dist", "ed_dist", "pharm_dist", "green_pas", "green_act")]
#head(covariates2)
names(covariates2) <- c("lsoa", "gp_access", "ae_access", "pharm_access", "green_access_prop", "green_access_dist")


##### read in the composite INDEX values CDRC Access to Health Assets and Hazards dataset 
covariates3 <- read_csv("*path-to-folder*/data/ahahv2domainsindex.csv")
#head(covariates3)
# filter for the lsoa code, health domain deciles and blue/green space domain deciles columns
covariates3 <- covariates3[c("lsoa11", "h_exp", "g_exp", "h_dec", "g_dec")]
#head(covariates3)
names(covariates3) <- c("lsoa","health_access_valexp", "greenblue_access_valexp", "health_access_deciles", "greenblue_access_deciles")


# merge the dataframes together
cornwall_covariates <- inner_join(covariates1, covariates2, by = c("code" = "lsoa"))
#head(cornwall_covariates)
# add the remaing columns via another merge
cornwall_covariates <- inner_join(cornwall_covariates, covariates3, by = c("code" = "lsoa"))
#head(cornwall_covariates)

# Checking for na values
apply(cornwall_covariates, 2, function(x) any(is.na(x)))
# renaming
cornwall_covariates <- cornwall_covariates[c("code", "names", "pop_density", "green_access_prop", "green_access_dist", "unemployed_pc", "socialrenting_pc", "bame_pc")]


#    EXPLORATORY ANALYSIS
########### Checking the distribution of the covariates
###### I'll also use Tukey's ladder of transformations to see if and how the covariates need to be transformed
## Persons per hectare - population density
ggplot(cornwall_covariates, aes(x = pop_density)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 10) +
  geom_density(colour = "red", 
               size = 1,
               adjust =1)

symbox(~pop_density, 
       cornwall_covariates,
       na.rm=T,
       powers = seq(-2,2,by=.5))


## Total BAME ethnic percentage
ggplot(cornwall_covariates, aes(x = bame_pc)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.05) +
  geom_density(colour = "red", 
               size = 1,
               adjust =1)

symbox(~bame_pc, 
       cornwall_covariates,
       na.rm=T,
       powers = seq(-2,2,by=.5))


## Green space (passive) access 
ggplot(cornwall_covariates, aes(x = green_access_prop)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.1) +
  geom_density(colour = "red", 
               size = 1,
               adjust =1)    

symbox(~green_access_prop, 
       cornwall_covariates,
       na.rm=T,
       powers = seq(-2,2,by=.5))

## Green space (active) access 
ggplot(cornwall_covariates, aes(x = green_access_dist)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.1) +
  geom_density(colour = "red", 
               size = 1,
               adjust =1)

symbox(~green_access_dist, 
       cornwall_covariates,
       na.rm=T,
       powers = seq(-2,2,by=.5))


## Socially rented accomodation %
ggplot(cornwall_covariates, aes(x = socialrenting_pc)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1) +
  geom_density(colour = "red", 
               size = 1,
               adjust =1)

symbox(~socialrenting_pc, 
       cornwall_covariates,
       na.rm=T,
       powers = seq(-2,2,by=.5))


## Unemployment rate
ggplot(cornwall_covariates, aes(x = unemployed_pc)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.1) +
  geom_density(colour = "red", 
               size = 1,
               adjust =1)

symbox(~unemployed_pc, 
       cornwall_covariates,
       na.rm=T,
       powers = seq(-2,2,by=.5))



############ computing means and ranges of covariates
mean(cornwall_covariates$pop_density) 
range(cornwall_covariates$pop_density)

mean(cornwall_covariates$green_access_prop) 
range(cornwall_covariates$green_access_prop)

mean(cornwall_covariates$green_access_dist) 
range(cornwall_covariates$green_access_dist)

mean(cornwall_covariates$unemployed_pc) 
range(cornwall_covariates$unemployed_pc)

mean(cornwall_covariates$socialrenting_pc) 
range(cornwall_covariates$socialrenting_pc)

mean(cornwall_covariates$bame_pc) 
range(cornwall_covariates$bame_pc)


############ rename
names(cornwall_covariates) <- c("codes", "names", "pop_density","green_area", "distance_to_green",
                           "unemployment", "social_renters", "bame")


############# Boxplots
cornwall_covariates_adj <- cornwall_covariates[c("codes", "pop_density",
                                       "green_area", "distance_to_green",
                                       "unemployment", "social_renters", 
                                       "bame")]

# log-transform the covariates to make them more Gaussian
cornwall_covariates_adj$pop_density <- log(cornwall_covariates_adj$pop_density)
cornwall_covariates_adj$green_area <- log(cornwall_covariates_adj$green_area)
cornwall_covariates_adj$distance_to_green <- log(cornwall_covariates_adj$distance_to_green)
cornwall_covariates_adj$unemployment <- log(cornwall_covariates_adj$unemployment)
cornwall_covariates_adj$social_renters <- log(cornwall_covariates_adj$social_renters)
cornwall_covariates_adj$bame <- log(cornwall_covariates_adj$bame)


#using code from stackoverflow to get my data into the correct format for plotting nicer boxplots/violin plots
#url for page that helped with the data formatting: https://stackoverflow.com/questions/14604439/plot-multiple-boxplot-in-one-graph
boxplotdf_log <- cornwall_covariates_adj %>%
  dplyr::select(codes, 
                pop_density,
                green_area,
                distance_to_green,
                unemployment,
                social_renters,
                bame)

boxplotdf_log.m <- melt(boxplotdf_log, id.var = "codes")
#show the new format of the data 
#boxplotdf_log

# Boxplots
boxplot_cornwall1 <- ggplot(data = boxplotdf_log.m, aes(x=variable, y=value, fill=variable)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=1) +
  geom_jitter(color="black", size=0.8, alpha=0.4) +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("") +
  xlab("")

boxplot_cornwall1
#warnings()


###################### And now the unlogged covariates for comparison
boxplotdf <- cornwall_covariates %>%
  dplyr::select(codes, 
                pop_density,
                green_area,
                distance_to_green,
                unemployment,
                social_renters,
                bame)

boxplotdf.m <- melt(boxplotdf, id.var = "codes")
#show the new format of the data 
#boxplotdf

# Boxplots
boxplot_cornwall_appendix <- ggplot(data = boxplotdf.m, aes(x=variable, y=value, fill=variable)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=1) +
  geom_jitter(color = "black", size=0.8, alpha=0.4) +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("") +
  xlab("")

boxplot_cornwall_appendix
#warnings()


###
# join Cornwall data with the map of Cornwall 
map1 <- inner_join(cornwall_lsoas, cornwall_covariates, by = c("LSOA11CD" = "codes"))
# drop the repeated lsoa names column
drop <-  c("names")
map1 <- map1[,!(names(map1) %in% drop)]

#st_write(map1, "cornwall_covariates.shp")


# For the interactive map, because I'm using leaflet, I Need '+proj=longlat +datum=WGS84' 
# Checking projection of my map of Cornwall
print(map1) #Returns Projected CRS: OSGB 1936 / British National Grid
# which has the epsg code of 4326

# Reproject
map1REPROJECTED <- map1 %>%
  st_transform(., 4326)
print(map1REPROJECTED)



############### Computing Correlation Matrix
cMartixVars <- cornwall_covariates[c("pop_density",
                                "green_area",
                                "distance_to_green",
                                "unemployment",
                                "social_renters",
                                "bame")]

cMatrix <- cor(cMartixVars)
head(round(cMatrix, 2))

corrplot(cMatrix, method = "number")



################## 
# As this analysis was conducted in the UCL Data Safe Haven for data ethics purposes, only the results were exportable
# READ IN THE RESULTS OF THE SAVED MODEL 
load("cornwall.rds")
summary(cornwall)


########################## Mapping relative risks of model 1.5
head(cornwall$summary.fitted.values)

# Add these data points to the map of london, assigning the mean to the estimate of the relative risk
# and 0.025quant and 0.975quant to the lower and upper limits of 95% credible intervals of the risks
map1REPROJECTED$RR <-  cornwall$summary.fitted.values[, "mean"]
map1REPROJECTED$LL <-  cornwall$summary.fitted.values[, "0.025quant"]
map1REPROJECTED$UL <-  cornwall$summary.fitted.values[, "0.975quant"]


# We cannot export the observed of expected counts, but the SMR is okay
# read in the 2011 smr, exported from the data safe haven
smr_2011 <- read_csv("*path-to-folder*/data/smr_2011.csv")
head(smr_2011)
# filter to just cornwall smrs
cornwall_smr <- inner_join(smr_2011, cornwall1, by = c("codes" = "lsoa11cd"))
cornwall_smr <- cornwall_smr[c("codes", "smr")]
head(cornwall_smr)

# add smr column to mapReprojected
map1REPROJECTED$SMR <- cornwall_smr$smr
head(map1REPROJECTED)


############# Read in the London RR map then use it as the palette for the Cornwall RR mapping
# to keep the maps on the same scale
mapREPROJECTED <- st_read("*path-to-folder*/data/mapREPROJECTED_RR_ldn.shp")
head(mapREPROJECTED)


################ plotting interactive map of the SMR
pal <- colorNumeric(palette = "YlOrRd", domain = mapREPROJECTED$SMR)
labels <- sprintf("<strong> %s </strong> <br/> SMR: %s",
                  map1REPROJECTED$LSOA11NM, round(map1REPROJECTED$SMR, 2)) %>%
  lapply(htmltools::HTML)

twenty_11 <- leaflet(map1REPROJECTED) %>% addTiles() %>%
  addPolygons(color = "grey", weight = 1, fillColor = ~pal(SMR), fillOpacity = 0.5,
              highlightOptions = highlightOptions(weight = 4), label = labels,
              labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                       padding = "3px 8px"),
                                          textsize = "15px",
                                          direction = "auto")) %>%
  addLegend(pal = pal, values = ~SMR, opacity = 0.5, title = "SMR",
            position = "bottomright")

twenty_11
#saveWidget(twenty_11, file="cornwall_smr2011.html")



############### PLOTTING a Interactive map of the RR

# specify the palette
pal <- colorNumeric(palette = "YlOrRd", domain = mapREPROJECTED$SMR)
# specify the labels
labels <- sprintf("<strong> %s </strong> <br/> SMR: %s <br/> Person per hectare: %s <br/> Green area (km2): %s <br/> 
                  Distance to Greenery (km): %s <br/> RR: %s (%s, %s)",
                  map1REPROJECTED$LSOA11NM, round(map1REPROJECTED$SMR, 2),
                  map1REPROJECTED$pop_density,
                  round(map1REPROJECTED$green_area, 2),
                  round(map1REPROJECTED$distance_to_green, 2),
                  round(map1REPROJECTED$RR, 2), 
                  round(map1REPROJECTED$LL, 2), round(map1REPROJECTED$UL, 2) ) %>%
  lapply(htmltools::HTML)

# apply final touches and now plot
rr_twenty11 <- leaflet(map1REPROJECTED) %>% addTiles() %>%
  addPolygons(color = "grey", weight = 1, fillColor = ~pal(RR), fillOpacity = 0.5,
              highlightOptions = highlightOptions(weight = 4), label = labels,
              labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                       padding = "3px 8px"),
                                          textsize = "15px",
                                          direction = "auto")) %>%
  addLegend(pal = pal, values = ~RR, opacity = 0.5, title = "RR",
            position = "bottomright")

rr_twenty11
#saveWidget(rr_twenty11, file="cornwall_rr2011.html")


# Computing the range of the two measures 
range(map1REPROJECTED$SMR) 
range(map1REPROJECTED$RR) 


####### extracting RR mean, and 90/10 quantile ratio
mean(map1REPROJECTED$RR)

# most at risk / least at risk
ninety1 <- quantile(map1REPROJECTED$RR, .90)
ten1 <- quantile(map1REPROJECTED$RR, .10)
ninety1/ten1






########## plotting the posterior distribution curves of the covariates
### extract the marginal values from the results
marginal_popden <- inla.smarginal(cornwall$marginals.fixed$`log(pop_density)`)

#create a dataframe in order to plot
marginal_popden <- data.frame(marginal_popden)

# now plot
popden_density <- ggplot(marginal_popden, aes(x = x, y = y)) + geom_line() +
  labs(x = expression(beta[1]), y = "Density") +
  geom_vline(xintercept = 0, col = "blue") + theme_bw()


# next covariate
### extract the marginal values from the results
marginal_green_prop <- inla.smarginal(cornwall$marginals.fixed$`log(green_access_prop)`)
#create a dataframe in order to plot
marginal_green_prop <- data.frame(marginal_green_prop)
# now plot
greenprop_density <- ggplot(marginal_green_prop, aes(x = x, y = y)) + geom_line() +
  labs(x = expression(beta[2]), y = "Density") +
  geom_vline(xintercept = 0, col = "blue") + theme_bw()


# next covariate
### extract the marginal values from the results
marginal_green_dist <- inla.smarginal(cornwall$marginals.fixed$`log(green_access_dist)`)
#create a dataframe in order to plot
marginal_green_dist <- data.frame(marginal_green_dist)
# now plot
greendist_density <- ggplot(marginal_green_dist, aes(x = x, y = y)) + geom_line() +
  labs(x = expression(beta[3]), y = "Density") +
  geom_vline(xintercept = 0, col = "blue") + theme_bw()


# next covariate
### extract the marginal values from the results
marginal_unemployed <- inla.smarginal(cornwall$marginals.fixed$`log(unemployed_pc + 1)`)
#create a dataframe in order to plot
marginal_unemployed <- data.frame(marginal_unemployed)
# now plot
unemployed_density <- ggplot(marginal_unemployed, aes(x = x, y = y)) + geom_line() +
  labs(x = expression(beta[4]), y = "Density") +
  geom_vline(xintercept = 0, col = "blue") + theme_bw()


# next covariate
### extract the marginal values from the results
marginal_socialrent <- inla.smarginal(cornwall$marginals.fixed$`log(socialrenting_pc + 1)`)
#create a dataframe in order to plot
marginal_socialrent <- data.frame(marginal_socialrent)
# now plot
socialrent_density <- ggplot(marginal_socialrent, aes(x = x, y = y)) + geom_line() +
  labs(x = expression(beta[5]), y = "Density") +
  geom_vline(xintercept = 0, col = "blue") + theme_bw()


# next covariate
### extract the marginal values from the results
marginal_bame <- inla.smarginal(cornwall$marginals.fixed$`log(bame_pc)`)
#create a dataframe in order to plot
marginal_bame <- data.frame(marginal_bame)
# now plot
bame_density <- ggplot(marginal_bame, aes(x = x, y = y)) + geom_line() +
  labs(x = expression(beta[6]), y = "Density") +
  geom_vline(xintercept = 0, col = "blue") + theme_bw()


###### arrange the ggplot density plots on one page
ggarrange(popden_density, greenprop_density, greendist_density,
          unemployed_density, socialrent_density, bame_density)

