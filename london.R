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

# read in the postcode -> OA -> LSOA -> MSOA -> LA lookup table. filter for only london lsoas
london <- read_csv(("*path-to-folder*/data/PCD_OA_LSOA_MSOA_LAD_NOV20_UK_LU/PCD_OA_LSOA_MSOA_LAD_NOV20_UK_LU.csv"),
                col_names = TRUE,
                locale = locale(encoding = "Latin1")) %>%
  dplyr::filter(str_detect(ladcd, "^E09"))
#head(london)

# keep the code and name columns only
london <- london[c("lsoa11cd", "lsoa11nm")]
# remove the duplicate lsoa rows, given that there is a row for each postcode
london <- london[!duplicated(london[,c("lsoa11cd", "lsoa11nm")]),]


# Reading in the map of england with lsoa boundaries
england_lsoas <- st_read("*path-to-folder*/data/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3-shp/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3.shp")
#head(england_lsoas)

# filter to just london 
ldn_lsoas <- inner_join(england_lsoas, london, by = c("LSOA11CD" = "lsoa11cd"))
ldn_lsoas <- ldn_lsoas[c("LSOA11CD", "LSOA11NM")]
#(ldn_lsoas)
rm(england_lsoas)

# plot the outline
#plot(ldn_lsoas)

# plotting just the outline of the shape
#ldn_lsoas %>%
#  st_geometry() %>%
#  plot()


#################### read in the covariate from london data store - taken from the census
# ethnic propotions, and mean and median household income lsoa data
# I'll need to read in the pop dnsity data separately because the london data store doesn't provide the 2011 pop den value
# It's an issue because I have the 2011 pop density for cornwall, so they need to match
covariates1 <- read_csv("*path-to-folder*/data/lsoa_LDS_subset.csv")

# drop the 2013 population density column column
drop <-  c("PPH_2013")
covariates1 <- covariates1[,!(names(covariates1) %in% drop)]


# rename
names(covariates1) <- c("codes", "names", "white_pc", "mixed_pc", "asian_pc", "black_pc", 
                        "other_pc", "bame_pc", "mean_income", "median_income", "socialrenting_pc", "unemployed_pc")
#head(covariates1)

# read in the 2011 persons per hectare data
popden <- read_csv("*path-to-folder*/data/england_popdensity.csv")
names(popden) <- c("names", "code", "pop_density")
popden <- popden[c("code", "pop_density")]

# merge
covariates1 <- inner_join(covariates1, popden, by = c("codes" = "code"))
#head(covariates1)

#################### read in the CDRC Access to Health Assets and Hazards dataset 
# there are raw scores, deciles and exponentiated scores. Different values were explored, hence the variety of covairates

covariates2 <- read_csv("*path-to-folder*/data/allvariableslsoawdeciles.csv")
head(covariates2)
# filter for the lsoa code,  gp, a&e, pharmacy, green (passive) and green (active) accessbitlity columns
covariates2 <- covariates2[c("lsoa11", "gpp_dist", "ed_dist", "pharm_dist", "green_pas", "green_act")]
#head(covariates2)
names(covariates2) <- c("lsoa", "gp_access", "ae_access", "pharm_access", "green_access_prop", "green_access_dist")


#################### read in the composite INDEX values CDRC Access to Health Assets and Hazards dataset 
covariates3 <- read_csv("*path-to-folder*/data/ahahv2domainsindex.csv")
head(covariates3)
# filter for the lsoa code, health domain deciles and blue/green space domain deciles columns
covariates3 <- covariates3[c("lsoa11", "h_exp", "g_exp", "h_dec", "g_dec")]
#head(covariates3)
names(covariates3) <- c("lsoa","health_access_valexp", "greenblue_access_valexp", "health_access_deciles", "greenblue_access_deciles")



# merge the dataframes together
ldn_covariates <- inner_join(covariates1, covariates2, by = c("codes" = "lsoa"))
#head(ldn_covariates)
# add the remaing columns via another merge
ldn_covariates <- inner_join(ldn_covariates, covariates3, by = c("codes" = "lsoa"))
#head(ldn_covariates)

# Checking for na values
#apply(ldn_covariates, 2, function(x) any(is.na(x)))

# Reduce to only relevant columns
ldn_covariates <- ldn_covariates[c("codes", "names", "pop_density", "green_access_prop", "green_access_dist", "unemployed_pc", "socialrenting_pc", "bame_pc")]




#    EXPLORATORY ANALYSIS
########### Checking the distribution of the covariates
###### I'll also use Tukey's ladder of transformations to see if and how the covariates need to be transformed
## Persons per hectare - population density
ggplot(ldn_covariates, aes(x = pop_density)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 10) +
  geom_density(colour = "red", 
               size = 1,
               adjust =1)

symbox(~pop_density, 
       ldn_covariates,
       na.rm=T,
       powers = seq(-2,2,by=.5))


## Total BAME ethnic percentage
ggplot(ldn_covariates, aes(x = bame_pc )) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.5) +
  geom_density(colour = "red", 
               size = 1,
               adjust =1)

symbox(~bame_pc, 
       ldn_covariates,
       na.rm=T,
       powers = seq(-2,2,by=.5))


## Green space (passive) access 
ggplot(ldn_covariates, aes(x = green_access_prop)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.1) +
  geom_density(colour = "red", 
               size = 1,
               adjust =1)    

symbox(~green_access_prop, 
       ldn_covariates,
       na.rm=T,
       powers = seq(-2,2,by=.5))

## Green space (active) access 
ggplot(ldn_covariates, aes(x = green_access_dist)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.1) +
  geom_density(colour = "red", 
               size = 1,
               adjust =1)

symbox(~green_access_dist, 
       ldn_covariates,
       na.rm=T,
       powers = seq(-2,2,by=.5))


## Socially rented accomodation %
ggplot(ldn_covariates, aes(x = socialrenting_pc)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1) +
  geom_density(colour = "red", 
               size = 1,
               adjust =1)

symbox(~socialrenting_pc, 
       ldn_covariates,
       na.rm=T,
       powers = seq(-2,2,by=.5))


## Unemployment rate
ggplot(ldn_covariates, aes(x = unemployed_pc)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1) +
  geom_density(colour = "red", 
               size = 1,
               adjust =1)

symbox(~unemployed_pc, 
       ldn_covariates,
       na.rm=T,
       powers = seq(-2,2,by=.5))



############ computing means and ranges of covariates
mean(ldn_covariates$pop_density)
range(ldn_covariates$pop_density)

mean(ldn_covariates$green_access_prop) 
range(ldn_covariates$green_access_prop)

mean(ldn_covariates$green_access_dist) 
range(ldn_covariates$green_access_dist)

mean(ldn_covariates$unemployed_pc) 
range(ldn_covariates$unemployed_pc)

mean(ldn_covariates$socialrenting_pc) 
range(ldn_covariates$socialrenting_pc)

mean(ldn_covariates$bame_pc)
range(ldn_covariates$bame_pc)


###rename
names(ldn_covariates) <- c("codes", "names", "pop_density","green_area", "distance_to_green",
                           "unemployment", "social_renters", "bame")

  
############# Boxplots
ldn_covariates_adj <- ldn_covariates[c("codes", "pop_density",
                                       "green_area", "distance_to_green",
                                       "unemployment", "social_renters", 
                                       "bame")]

# log-transform the covariates to make them more Gaussian
ldn_covariates_adj$pop_density <- log(ldn_covariates_adj$pop_density)
ldn_covariates_adj$green_area <- log(ldn_covariates_adj$green_area)
ldn_covariates_adj$distance_to_green <- log(ldn_covariates_adj$distance_to_green)
ldn_covariates_adj$unemployment <- log(ldn_covariates_adj$unemployment)
ldn_covariates_adj$social_renters <- log(ldn_covariates_adj$social_renters)
ldn_covariates_adj$bame <- log(ldn_covariates_adj$bame)


#using code from stackoverflow to get my data into the correct format for plotting nicer boxplots/violin plots
#url for page that helped with the data formatting: https://stackoverflow.com/questions/14604439/plot-multiple-boxplot-in-one-graph
boxplotdf_log <- ldn_covariates_adj %>%
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
boxplot_ldn_1 <- ggplot(data = boxplotdf_log.m, aes(x=variable, y=value, fill=variable)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=1) +
  geom_jitter(color="black", size=0.4, alpha=0.15) +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("") +
  xlab("")

boxplot_ldn_1
#warnings()


###################### And now the unlogged covariates for comparison
boxplotdf <- ldn_covariates %>%
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
boxplot_london_appendix <- ggplot(data = boxplotdf.m, aes(x=variable, y=value, fill=variable)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=1) +
  geom_jitter(color = "black", size=0.4, alpha=0.15) +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("") +
  xlab("")

boxplot_london_appendix
#warnings()


##############################################################
# join london data with the map of london 
map <- inner_join(ldn_lsoas, ldn_covariates, by = c("LSOA11CD" = "codes"))
# drop the repeated lso names column
drop <-  c("names")
map <- map[,!(names(map) %in% drop)]

#st_write(map, "london_covariates.shp")


# For the interactive map, because I'm using leaflet, I Need '+proj=longlat +datum=WGS84' 
# Checking projection of my map of London
print(map) #Returns Projected CRS: OSGB 1936 / British National Grid
# which has the epsg code of 4326

# Reproject
mapREPROJECTED <- map %>%
  st_transform(., 4326)
print(mapREPROJECTED)


############### Computing Correlation Matrix
cMartixVars <- ldn_covariates[c("pop_density",
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
load("ldn.rds")
summary(ldn)

####### Mapping relative risks of model 1.5 aka 1c
head(ldn$summary.fitted.values)

# Add these data points to the map of london, assigning the mean to the estimate of the relative risk
# and 0.025quant and 0.975quant to the lower and upper limits of 95% credible intervals of the risks
mapREPROJECTED$RR <-  ldn$summary.fitted.values[, "mean"]
mapREPROJECTED$LL <-  ldn$summary.fitted.values[, "0.025quant"]
mapREPROJECTED$UL <-  ldn$summary.fitted.values[, "0.975quant"]


# We cannot export the observed of expected counts, but the SMR is okay
# read in the 2011 SMR, exported from the data safe haven
smr_2011 <- read_csv("*path-to-folder*/data/smr_2011.csv")
head(smr_2011)
# filter to just london smrs
ldn_smr <- inner_join(smr_2011, london, by = c("codes" = "lsoa11cd"))
ldn_smr <- ldn_smr[c("codes", "smr")]
head(ldn_smr)


# add smr column to mapReprojected
mapREPROJECTED$SMR <- ldn_smr$smr
head(mapREPROJECTED)

# save this to keep a consistent scale across all mappings
#st_write(mapREPROJECTED, "mapREPROJECTED_RR_ldn.shp")



################ ploting interactive map of the SMR
pal <- colorNumeric(palette = "YlOrRd", domain = mapREPROJECTED$SMR)
labels <- sprintf("<strong> %s </strong> <br/> SMR: %s",
                  mapREPROJECTED$LSOA11NM, round(mapREPROJECTED$SMR, 2)) %>%
  lapply(htmltools::HTML)

twenty_11 <- leaflet(mapREPROJECTED) %>% addTiles() %>%
  addPolygons(color = "grey", weight = 1, fillColor = ~pal(SMR), fillOpacity = 0.5,
              highlightOptions = highlightOptions(weight = 4), label = labels,
              labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                       padding = "3px 8px"),
                                          textsize = "15px",
                                          direction = "auto")) %>%
  addLegend(pal = pal, values = ~SMR, opacity = 0.5, title = "SMR",
            position = "bottomright")

twenty_11
#saveWidget(twenty_11, file="ldn_smr2011.html")




############### PLOTTING a Interactive map of the RR
# specify the palette
pal <- colorNumeric(palette = "YlOrRd", domain = mapREPROJECTED$SMR)
# specify the labels
labels <- sprintf("<strong> %s </strong> <br/> SMR: %s <br/> Person per hectare: %s <br/> Green area (km2): %s <br/> 
                  Distance to Greenery (km): %s <br/> RR: %s (%s, %s)",
                  mapREPROJECTED$LSOA11NM, round(mapREPROJECTED$SMR, 2),
                  mapREPROJECTED$pop_density,
                  round(mapREPROJECTED$green_area, 2),
                  round(mapREPROJECTED$distance_to_green, 2),
                  round(mapREPROJECTED$RR, 2), 
                  round(mapREPROJECTED$LL, 2), round(mapREPROJECTED$UL, 2)) %>%
  lapply(htmltools::HTML)
# apply final touches and now plot
rr2011 <- leaflet(mapREPROJECTED) %>% addTiles() %>%
  addPolygons(color = "grey", weight = 1, fillColor = ~pal(RR), fillOpacity = 0.5,
              highlightOptions = highlightOptions(weight = 4), label = labels,
              labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                       padding = "3px 8px"),
                                          textsize = "15px",
                                          direction = "auto")) %>%
  addLegend(pal = pal, values = ~RR, opacity = 0.5, title = "RR",
            position = "bottomright")

rr2011
saveWidget(rr2011, file="ldn_rr2011.html")


# Computing the range of the two measures 
range(mapREPROJECTED$SMR) 
range(mapREPROJECTED$RR)    


####### extracting RR mean, and 90/10 quantile ratio
mean(mapREPROJECTED$RR)

# most at risk / least at risk
ninety1 <- quantile(mapREPROJECTED$RR, .90)
ten1 <- quantile(mapREPROJECTED$RR, .10)
ninety1/ten1


########## plotting the posterior distribution curves of the covariates
### extract the marginal values from the results
marginal_popden <- inla.smarginal(ldn$marginals.fixed$`log(pop_density)`)

#create a dataframe in order to plot
marginal_popden <- data.frame(marginal_popden)

# now plot
popden_density <- ggplot(marginal_popden, aes(x = x, y = y)) + geom_line() +
  labs(x = expression(beta[1]), y = "Density") +
  geom_vline(xintercept = 0, col = "blue") + theme_bw()


# next covariate
### extract the marginal values from the results
marginal_green_prop <- inla.smarginal(ldn$marginals.fixed$`log(green_access_prop)`)
#create a dataframe in order to plot
marginal_green_prop <- data.frame(marginal_green_prop)
# now plot
greenprop_density <- ggplot(marginal_green_prop, aes(x = x, y = y)) + geom_line() +
  labs(x = expression(beta[2]), y = "Density") +
  geom_vline(xintercept = 0, col = "blue") + theme_bw()


# next covariate
### extract the marginal values from the results
marginal_green_dist <- inla.smarginal(ldn$marginals.fixed$`log(green_access_dist)`)
#create a dataframe in order to plot
marginal_green_dist <- data.frame(marginal_green_dist)
# now plot
greendist_density <- ggplot(marginal_green_dist, aes(x = x, y = y)) + geom_line() +
  labs(x = expression(beta[3]), y = "Density") +
  geom_vline(xintercept = 0, col = "blue") + theme_bw()


# next covariate
### extract the marginal values from the results
marginal_unemployed <- inla.smarginal(ldn$marginals.fixed$`log(unemployed_pc + 1)`)
#create a dataframe in order to plot
marginal_unemployed <- data.frame(marginal_unemployed)
# now plot
unemployed_density <- ggplot(marginal_unemployed, aes(x = x, y = y)) + geom_line() +
  labs(x = expression(beta[4]), y = "Density") +
  geom_vline(xintercept = 0, col = "blue") + theme_bw()


# next covariate
### extract the marginal values from the results
marginal_socialrent <- inla.smarginal(ldn$marginals.fixed$`log(socialrenting_pc + 1)`)
#create a dataframe in order to plot
marginal_socialrent <- data.frame(marginal_socialrent)
# now plot
socialrent_density <- ggplot(marginal_socialrent, aes(x = x, y = y)) + geom_line() +
  labs(x = expression(beta[5]), y = "Density") +
  geom_vline(xintercept = 0, col = "blue") + theme_bw()


# next covariate
### extract the marginal values from the results
marginal_bame <- inla.smarginal(ldn$marginals.fixed$`log(bame_pc)`)
#create a dataframe in order to plot
marginal_bame <- data.frame(marginal_bame)
# now plot
bame_density <- ggplot(marginal_bame, aes(x = x, y = y)) + geom_line() +
  labs(x = expression(beta[6]), y = "Density") +
  geom_vline(xintercept = 0, col = "blue") + theme_bw()


###### arrange the ggplot density plots on one page
ggarrange(popden_density, greenprop_density, greendist_density,
          unemployed_density, socialrent_density, bame_density)


