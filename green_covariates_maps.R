#Load all our data
library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)


################################################################### london
#### read in the london covariates shapefile - already in 277000
london_covariates <- st_read("*your-file-path*/london_covariates.shp")
head(london_covariates)
# fix the names
names(london_covariates) <- c("lsoacodes", "names", "popdensity", "greenarea", 
                              "greendistance", "unemployed", "socialrent", "bame", "geometry")
london_covariates <- london_covariates[c("lsoacodes", "greenarea", "greendistance", "geometry")]
head(london_covariates)
# remove lsoa geometry column
london_covariates <- st_set_geometry(london_covariates, NULL)




####### read in the postcode -> OA -> LSOA -> MSOA -> LA lookup table. filter for only london lsoas
london_lookup <- read_csv(("*your-file-path*/data/PCD_OA_LSOA_MSOA_LAD_NOV20_UK_LU/PCD_OA_LSOA_MSOA_LAD_NOV20_UK_LU.csv"),
                          col_names = TRUE,
                          locale = locale(encoding = "Latin1")) %>%
  dplyr::filter(str_detect(ladcd, "^E09"))
#head(london_lookup)

# keep the code and name columns only
london_lookup <- london_lookup[c("lsoa11cd", "msoa11cd")]
# remove the duplicate lsoa rows, given that there is a row for each postcode
london_lookup <- london_lookup[!duplicated(london_lookup[,c("lsoa11cd")]),]

# merge the two dataframe together
ldn_covariates <- inner_join(london_covariates, london_lookup, by = c("lsoacodes" = 'lsoa11cd'))
head(ldn_covariates)



#calculate the msoa mean of the two variables
# green area first
greenarea_msoa_mean <- aggregate(greenarea ~ msoa11cd, ldn_covariates, mean)
head(greenarea_msoa_mean) 

# now green distance
greendistance_msoa_mean <- aggregate(greendistance ~ msoa11cd, ldn_covariates, mean)
head(greendistance_msoa_mean)

# merge the two together
ldn_covariates <- inner_join(greenarea_msoa_mean, greendistance_msoa_mean, by = c("msoa11cd" = 'msoa11cd'))
head(ldn_covariates)


############ Reading in the map of england with msoa boundaries
england_msoas <- st_read("*your-file-path*/data/Middle_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3-shp/Middle_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3.shp")
head(england_msoas)

# filter to just london 
london_covariates <- inner_join(england_msoas, ldn_covariates, by = c("MSOA11CD" = "msoa11cd"))
london_covariates <- london_covariates[c("MSOA11CD", "greenarea", "greendistance")]
head(london_covariates)







################################################################## cornwall
#### read in the cornwall covariates shapefile - already in 277000
cornwall_covariates <- st_read("*your-file-path*/cornwall_covariates.shp")
head(cornwall_covariates)
# fix the names
names(cornwall_covariates) <- c("lsoacodes", "names", "popdensity", "greenarea", 
                              "greendistance", "unemployed", "socialrent", "bame", "geometry")
cornwall_covariates <- cornwall_covariates[c("lsoacodes", "greenarea", "greendistance", "geometry")]
head(cornwall_covariates)
# remove lsoa geometry column
cornwall_covariates <- st_set_geometry(cornwall_covariates, NULL)



# read in the postcode -> OA -> LSOA -> MSOA -> LA lookup table. filter for cornwall 
cornwall_lookup <-  read_csv(("*your-file-path*/data/PCD_OA_LSOA_MSOA_LAD_NOV20_UK_LU/PCD_OA_LSOA_MSOA_LAD_NOV20_UK_LU.csv"),
                       col_names = TRUE,
                       locale = locale(encoding = "Latin1")) %>%
  dplyr::filter(str_detect(ladnm, "Cornwall"))
#head(cornwall_lookup)

# keep the code and name columns only
cornwall_lookup <- cornwall_lookup[c("lsoa11cd", "msoa11cd")]
# remove the duplicate lsoa rows, given that there is a row for each postcode
cornwall_lookup <- cornwall_lookup[!duplicated(cornwall_lookup[,c("lsoa11cd")]),]


# merge the two dataframe together
crnw_covariates <- inner_join(cornwall_covariates, cornwall_lookup, by = c("lsoacodes" = 'lsoa11cd'))
head(crnw_covariates)



#calculate the msoa mean of the two variables
# green area first
greenarea_msoa_mean <- aggregate(greenarea ~ msoa11cd, crnw_covariates, mean)
head(greenarea_msoa_mean) 

# now green distance
greendistance_msoa_mean <- aggregate(greendistance ~ msoa11cd, crnw_covariates, mean)
head(greendistance_msoa_mean)

# merge the two together
crnw_covariates <- inner_join(greenarea_msoa_mean, greendistance_msoa_mean, by = c("msoa11cd" = 'msoa11cd'))
head(crnw_covariates)



# filter to just london 
cornwall_covariates <- inner_join(england_msoas, crnw_covariates, by = c("MSOA11CD" = "msoa11cd"))
cornwall_covariates <- cornwall_covariates[c("MSOA11CD", "greenarea", "greendistance")]
head(cornwall_covariates)




#####################################################################
# Trying to determine breaks - London
q_ldn <- quantile(london_covariates$greenarea, c(0, 0.25, 0.50, 0.75, 1))
q_ldn

# Cornwall
q_cornwall <- quantile(cornwall_covariates$greenarea, c(0, 0.25, 0.50, 0.75, 1))
q_cornwall


tmap_mode("plot")
# set the breaks
# for our mapped data
#tmaptools::palette_explorer()
breaks1 = c(0, 0.25, 1, 2.5, 7.5, 15)

### LONDON
t1 <- tm_shape(london_covariates) + 
  tm_polygons("greenarea",
              title = "Total green\nspace (km2)",
              breaks=breaks1, 
              palette="YlGn")+
  tm_scale_bar(position = c(0.01 , 0.02), text.size = .70)+
  tm_layout(title = "",
            title.size = 1.0,
            title.position = c(0.02, 0.98),
            legend.position = c(0.75, 0.01), 
            legend.text.size=.75, 
            legend.title.size = 1.0,
            frame=FALSE)+
  tm_credits("(c) Consumer Data Research Centre contributors", position=c(0.0,0.0))+
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", size = 4, position = c(0.75, 0.70)) +
  
  #bottom left top right
  tm_layout(inner.margin=c(0.08,0.04,0.06,0.2))

t1


### CORNWALL
t2 <- tm_shape(cornwall_covariates) + 
  tm_polygons("greenarea",
              title = "Total green\nspace (km2)",
              breaks=breaks1, 
              palette="YlGn")+
  tm_scale_bar(position = c(0.01 , 0.02), text.size = .70)+
  tm_layout(title = "",
            title.size = 1.0,
            title.position = c(0.02, 0.98),
            legend.position = c(0.75, 0.01), 
            legend.text.size=.75, 
            legend.title.size = 1.0,
            frame=FALSE)+
  tm_credits("(c) Consumer Data Research Centre contributors", position=c(0.0,0.0))+
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", size = 4, position = c(0.75, 0.70)) +
  
  #bottom left top right
  tm_layout(inner.margin=c(0.08,0.04,0.06,0.2))

t2





###### AND NOW THE DISTANCE TO GREEN SPACE
# Trying to determine breaks - London
q2_ldn <- quantile(london_covariates$greendistance, c(0, 0.25, 0.50, 0.75, 1))
q2_ldn

# Cornwall
q2_cornwall <- quantile(cornwall_covariates$greendistance, c(0, 0.25, 0.50, 0.75, 1))
q2_cornwall



#tmaptools::palette_explorer()
breaks2 = c(0, 0.25, 0.5, 1, 2.5, 5)

### LONDON
t3 <- tm_shape(london_covariates) + 
  tm_polygons("greendistance",
              title = "Average distance\nto nearest open\ngreen space (km2)",
              breaks=breaks2, 
              palette="YlOrBr")+
  tm_scale_bar(position = c(0.01 , 0.02), text.size = .70)+
  tm_layout(title = "",
            title.size = 1.0,
            title.position = c(0.02, 0.98),
            legend.position = c(0.75, 0.01), 
            legend.text.size=.75, 
            legend.title.size = 1.0,
            frame=FALSE)+
  tm_credits("(c) Consumer Data Research Centre contributors", position=c(0.0,0.0))+
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", size = 4, position = c(0.75, 0.70)) +
  
  #bottom left top right
  tm_layout(inner.margin=c(0.08,0.04,0.06,0.2))

t3


### CORNWALL
t4 <- tm_shape(cornwall_covariates) + 
  tm_polygons("greendistance",
              title = "Average distance\nto nearest open\ngreen space (km2)",
              breaks=breaks2, 
              palette="YlOrBr")+
  tm_scale_bar(position = c(0.01 , 0.02), text.size = .70)+
  tm_layout(title = "",
            title.size = 1.0,
            title.position = c(0.02, 0.98),
            legend.position = c(0.75, 0.01), 
            legend.text.size=.75, 
            legend.title.size = 1.0,
            frame=FALSE)+
  tm_credits("(c) Consumer Data Research Centre contributors", position=c(0.0,0.0))+
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", size = 4, position = c(0.75, 0.70)) +
  
  #bottom left top right
  tm_layout(inner.margin=c(0.08,0.04,0.06,0.2))

t4

