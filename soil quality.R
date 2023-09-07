#https://stackoverflow.com/questions/13443372/simple-way-to-subset-spatialpolygonsdataframe-i-e-delete-polygons-by-attribut
#https://stackoverflow.com/questions/23073669/clipping-raster-using-shapefile-in-r-but-keeping-the-geometry-of-the-shapefile
#https://stackoverflow.com/questions/43761967/r-how-to-extract-the-mean-values-of-a-raster-for-each-polygon-for-an-altitude-gr
#https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1247
#https://www.mapspam.info/data/
#https://www.worldclim.org/data/cmip6/cmip6_clim2.5m.html
#https://asterweb.jpl.nasa.gov/gdem.asp
#https://ipad.fas.usda.gov/rssiws/al/global_cropprod.aspx

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting

library(sf)
library(raster)
library(here)
library(ggplot2)
library(viridis)
library(dplyr)
library(rayshader)
library(rasterVis)
library(RColorBrewer)

nc_data <- nc_open('crop92_v1.1_5min.nc')
# Save the print(nc) dump to a text file
{
  sink('gimms3g_ndvi_1982-2012_metadata.txt')
  print(nc_data)
  sink()
}

raster1 <- raster(here::here("C:/Users/syedm/Desktop/soil/out (6).tif"))
plot(raster1)

raster1 <- raster(here::here("C:/Users/syedm/Downloads/co2_amt_tower-insitu_1_ccgg_HourlyData.nc"))
plot(raster1)

raster1 <- read_stars("C:/Users/syedm/Desktop/Columbus/crop92_v1.1_5min.nc")
plot(raster1)

raster1 <- raster(here::here("C:/Users/syedm/Desktop/Columbus/wise_05min_v12.tif"))
e <- extent(-83.67, -82.33, 39.67, 40.35)
rc <- crop(raster1, e)

poly <- readOGR(dsn="C:/Users/syedm/Desktop/Columbus/World_Countries__Generalized_.shp")
plot(poly)

IGBP_raster <- projectRaster(IGBP_raster, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

#IGBP_raster <- raster::mask(IGBP_raster, as_Spatial(map_boundary))

#plot(IGBP_raster, xlab = "", ylab = "", xaxt='n', yaxt = 'n')

#plotting after cropping

mean_val <- extent(x=raster1, y=poly)
rc <- crop(IGBP_raster, e)
plot(rc)

head(poly@data, 249)

bd <- poly[poly$COUNTRY=="Bangladesh",]
plot(bd)
bd_raster <- crop(raster1, extent(bd))
plot(bd_raster)
bd_raster2 <- mask(bd_raster, bd)
plot(bd_raster2)

d <- getValues(bd_raster2)
mean(d, na.rm=TRUE)

#SSA

c <- poly@data

poly <- readOGR(dsn="C:/Users/syedm/Desktop/soil/world shape/World_Countries__Generalized_.shp")
plot(poly)

poly <- readOGR(dsn="C:/Users/syedm/Desktop/soil/ind shapefile/idn_admbnda_adm1_bps_20200401.shp")
plot(poly)
bd <- poly

bd <- spTransform(poly, "+proj=longlat +ellps=WGS84 +datum=WGS84")

d <- extract(x=bd_raster2, y=bd, fun=mean, na.rm=TRUE, sp=T)
dat <- d@data

mean(d, na.rm=TRUE)

name <- "Nigeria"

#area
raster1 <- raster(here::here("C:/Users/syedm/Desktop/soil/spam2017v2r1_ssa_harv_area.geotiff/spam2017V2r1_SSA_H_WHEA_A.tif"))

bd <- poly[poly$COUNTRY=="Nigeria",]
plot(bd)
bd_raster <- crop(raster1, extent(bd))
plot(bd_raster)
bd_raster2 <- mask(bd_raster, bd)
plot(bd_raster2)
plot(bd, add=TRUE)

#production
raster1 <- raster(here::here("C:/Users/syedm/Desktop/soil/spam2017v2r1_ssa_prod.geotiff/spam2017V2r1_SSA_P_YAMS_A.tif"))

bd <- poly[poly$COUNTRY==name,]
plot(bd)
bd_raster <- crop(raster1, extent(bd))
plot(bd_raster)
bd_raster2 <- mask(bd_raster, bd)
plot(bd_raster2)
plot(bd, add=TRUE)

#value of production
raster1 <- raster(here::here("C:/Users/syedm/Desktop/soil/spam2017v2r1_ssa_val_prod.geotiff/spam2017V2r1_SSA_V_YAMS_A.tif"))

bd <- poly[poly$COUNTRY==name,]
plot(bd)
bd_raster <- crop(raster1, extent(bd))
plot(bd_raster)
bd_raster2 <- mask(bd_raster, bd)
plot(bd_raster2)
plot(bd, add=TRUE)

#yield
raster1 <- raster(here::here("C:/Users/syedm/Desktop/soil/spam2010v2r0_global_yield.geotiff/spam2010V2r0_global_Y_OILP_A.tif"))

bd <- poly[poly$COUNTRY=="Nigeria",]
plot(bd)
bd_raster <- crop(raster1, extent(bd))
plot(bd_raster)
bd_raster2 <- mask(bd_raster, bd)
plot(bd_raster2)
plot(bd, add=TRUE)

#soil

poly <- readOGR(dsn="C:/Users/syedm/Desktop/soil/world shape/World_Countries__Generalized_.shp")
plot(poly)

raster1 <- raster(here::here("C:/Users/syedm/Desktop/soil/HWSD_1247/data/T_OC.nc4"))

bd <- poly[poly$COUNTRY==name,]
plot(bd)
bd_raster <- crop(raster1, extent(bd))
plot(bd_raster)
bd_raster2 <- mask(bd_raster, bd)
plot(bd_raster2)
plot(bd, add=TRUE)

#elevation

raster1 <- raster(here::here("C:/Users/syedm/Desktop/soil/GDEM-10km-colorized.tif"))

bd_raster <- crop(raster1, extent(bd))
plot(bd_raster)
bd_raster2 <- mask(bd_raster, bd)
plot(bd_raster2)
plot(bd, add=TRUE)

#temperature

raster1 <- raster(here::here("C:/Users/syedm/Desktop/soil/avg temperature/wc2.1_2.5m_tavg_01.tif"))
raster2 <- raster(here::here("C:/Users/syedm/Desktop/soil/avg temperature/wc2.1_2.5m_tavg_02.tif"))
raster3 <- raster(here::here("C:/Users/syedm/Desktop/soil/avg temperature/wc2.1_2.5m_tavg_03.tif"))
raster4 <- raster(here::here("C:/Users/syedm/Desktop/soil/avg temperature/wc2.1_2.5m_tavg_04.tif"))
raster5 <- raster(here::here("C:/Users/syedm/Desktop/soil/avg temperature/wc2.1_2.5m_tavg_05.tif"))
raster6 <- raster(here::here("C:/Users/syedm/Desktop/soil/avg temperature/wc2.1_2.5m_tavg_06.tif"))
raster7 <- raster(here::here("C:/Users/syedm/Desktop/soil/avg temperature/wc2.1_2.5m_tavg_07.tif"))
raster8 <- raster(here::here("C:/Users/syedm/Desktop/soil/avg temperature/wc2.1_2.5m_tavg_08.tif"))
raster9 <- raster(here::here("C:/Users/syedm/Desktop/soil/avg temperature/wc2.1_2.5m_tavg_09.tif"))
raster10 <- raster(here::here("C:/Users/syedm/Desktop/soil/avg temperature/wc2.1_2.5m_tavg_10.tif"))
raster11 <- raster(here::here("C:/Users/syedm/Desktop/soil/avg temperature/wc2.1_2.5m_tavg_11.tif"))
raster12 <- raster(here::here("C:/Users/syedm/Desktop/soil/avg temperature/wc2.1_2.5m_tavg_12.tif"))

raster_stack <- stack(raster1, raster2, raster3, raster4, raster5, raster6, raster7, raster8, raster9, raster10, raster11, raster12)
raster_file <- stackApply(raster_stack, indices=rep(1,nlayers(raster_stack)), fun=mean)

bd_raster <- crop(raster_file, extent(bd))
plot(bd_raster)
bd_raster2 <- mask(bd_raster, bd)
plot(bd_raster2)
plot(bd, add=TRUE)

#precipitation

raster1 <- raster(here::here("C:/Users/syedm/Desktop/soil/precipitation/wc2.1_2.5m_prec_01.tif"))
raster2 <- raster(here::here("C:/Users/syedm/Desktop/soil/precipitation/wc2.1_2.5m_prec_02.tif"))
raster3 <- raster(here::here("C:/Users/syedm/Desktop/soil/precipitation/wc2.1_2.5m_prec_03.tif"))
raster4 <- raster(here::here("C:/Users/syedm/Desktop/soil/precipitation/wc2.1_2.5m_prec_04.tif"))
raster5 <- raster(here::here("C:/Users/syedm/Desktop/soil/precipitation/wc2.1_2.5m_prec_05.tif"))
raster6 <- raster(here::here("C:/Users/syedm/Desktop/soil/precipitation/wc2.1_2.5m_prec_06.tif"))
raster7 <- raster(here::here("C:/Users/syedm/Desktop/soil/precipitation/wc2.1_2.5m_prec_07.tif"))
raster8 <- raster(here::here("C:/Users/syedm/Desktop/soil/precipitation/wc2.1_2.5m_prec_08.tif"))
raster9 <- raster(here::here("C:/Users/syedm/Desktop/soil/precipitation/wc2.1_2.5m_prec_09.tif"))
raster10 <- raster(here::here("C:/Users/syedm/Desktop/soil/precipitation/wc2.1_2.5m_prec_10.tif"))
raster11 <- raster(here::here("C:/Users/syedm/Desktop/soil/precipitation/wc2.1_2.5m_prec_11.tif"))
raster12 <- raster(here::here("C:/Users/syedm/Desktop/soil/precipitation/wc2.1_2.5m_prec_12.tif"))

raster_stack <- stack(raster1, raster2, raster3, raster4, raster5, raster6, raster7, raster8, raster9, raster10, raster11, raster12)
raster_file <- stackApply(raster_stack, indices=rep(1,nlayers(raster_stack)), fun=mean)

bd_raster <- crop(raster_file, extent(bd))
plot(bd_raster)
bd_raster2 <- mask(bd_raster, bd)
plot(bd_raster2)
plot(bd, add=TRUE)

#solar radiation

raster1 <- raster(here::here("C:/Users/syedm/Desktop/soil/solar radiation/wc2.1_2.5m_srad_01.tif"))
raster2 <- raster(here::here("C:/Users/syedm/Desktop/soil/solar radiation/wc2.1_2.5m_srad_02.tif"))
raster3 <- raster(here::here("C:/Users/syedm/Desktop/soil/solar radiation/wc2.1_2.5m_srad_03.tif"))
raster4 <- raster(here::here("C:/Users/syedm/Desktop/soil/solar radiation/wc2.1_2.5m_srad_04.tif"))
raster5 <- raster(here::here("C:/Users/syedm/Desktop/soil/solar radiation/wc2.1_2.5m_srad_05.tif"))
raster6 <- raster(here::here("C:/Users/syedm/Desktop/soil/solar radiation/wc2.1_2.5m_srad_06.tif"))
raster7 <- raster(here::here("C:/Users/syedm/Desktop/soil/solar radiation/wc2.1_2.5m_srad_07.tif"))
raster8 <- raster(here::here("C:/Users/syedm/Desktop/soil/solar radiation/wc2.1_2.5m_srad_08.tif"))
raster9 <- raster(here::here("C:/Users/syedm/Desktop/soil/solar radiation/wc2.1_2.5m_srad_09.tif"))
raster10 <- raster(here::here("C:/Users/syedm/Desktop/soil/solar radiation/wc2.1_2.5m_srad_10.tif"))
raster11 <- raster(here::here("C:/Users/syedm/Desktop/soil/solar radiation/wc2.1_2.5m_srad_11.tif"))
raster12 <- raster(here::here("C:/Users/syedm/Desktop/soil/solar radiation/wc2.1_2.5m_srad_12.tif"))

raster_stack <- stack(raster1, raster2, raster3, raster4, raster5, raster6, raster7, raster8, raster9, raster10, raster11, raster12)
raster_file <- stackApply(raster_stack, indices=rep(1,nlayers(raster_stack)), fun=mean)

bd_raster <- crop(raster_file, extent(bd))
plot(bd_raster)
bd_raster2 <- mask(bd_raster, bd)
plot(bd_raster2)
plot(bd, add=TRUE)

#predicted precipitation (2021-2040)

raster1 <- raster(here::here("C:/Users/syedm/Desktop/soil/pred precipitation/wc2.1_2.5m_prec_CNRM-CM6-1_ssp126_2021-2040.tif"))

bd_raster <- crop(raster1, extent(bd))
plot(bd_raster)
bd_raster2 <- mask(bd_raster, bd)
plot(bd_raster2)
plot(bd, add=TRUE)

#predicted bio (2021-2040)

raster1 <- raster(here::here("C:/Users/syedm/Desktop/soil/pred bio/wc2.1_2.5m_bioc_CNRM-CM6-1_ssp126_2021-2040.tif"))

bd_raster <- crop(raster1, extent(bd))
plot(bd_raster)
bd_raster2 <- mask(bd_raster, bd)
plot(bd_raster2)
plot(bd, add=TRUE)

#predicted precipitation (2041-2060)

raster1 <- raster(here::here("C:/Users/syedm/Desktop/soil/pred precipitation 2/wc2.1_2.5m_prec_CNRM-CM6-1_ssp126_2041-2060.tif"))

bd_raster <- crop(raster1, extent(bd))
plot(bd_raster)
bd_raster2 <- mask(bd_raster, bd)
plot(bd_raster2)
plot(bd, add=TRUE)

#predicted bio (2041-2060)

raster1 <- raster(here::here("C:/Users/syedm/Desktop/soil/pred bio 2/wc2.1_2.5m_bioc_CNRM-CM6-1_ssp126_2041-2060.tif"))

bd_raster <- crop(raster1, extent(bd))
plot(bd_raster)
bd_raster2 <- mask(bd_raster, bd)
plot(bd_raster2)
plot(bd, add=TRUE)

#prediction temp diff

raster1 <- raster(here::here("C:/Users/syedm/Desktop/soil/avg temperature/wc2.1_2.5m_tavg_01.tif"))
raster2 <- raster(here::here("C:/Users/syedm/Desktop/soil/avg temperature/wc2.1_2.5m_tavg_02.tif"))
raster3 <- raster(here::here("C:/Users/syedm/Desktop/soil/avg temperature/wc2.1_2.5m_tavg_03.tif"))
raster4 <- raster(here::here("C:/Users/syedm/Desktop/soil/avg temperature/wc2.1_2.5m_tavg_04.tif"))
raster5 <- raster(here::here("C:/Users/syedm/Desktop/soil/avg temperature/wc2.1_2.5m_tavg_05.tif"))
raster6 <- raster(here::here("C:/Users/syedm/Desktop/soil/avg temperature/wc2.1_2.5m_tavg_06.tif"))
raster7 <- raster(here::here("C:/Users/syedm/Desktop/soil/avg temperature/wc2.1_2.5m_tavg_07.tif"))
raster8 <- raster(here::here("C:/Users/syedm/Desktop/soil/avg temperature/wc2.1_2.5m_tavg_08.tif"))
raster9 <- raster(here::here("C:/Users/syedm/Desktop/soil/avg temperature/wc2.1_2.5m_tavg_09.tif"))
raster10 <- raster(here::here("C:/Users/syedm/Desktop/soil/avg temperature/wc2.1_2.5m_tavg_10.tif"))
raster11 <- raster(here::here("C:/Users/syedm/Desktop/soil/avg temperature/wc2.1_2.5m_tavg_11.tif"))
raster12 <- raster(here::here("C:/Users/syedm/Desktop/soil/avg temperature/wc2.1_2.5m_tavg_12.tif"))
raster14 <- raster(here::here("C:/Users/syedm/Desktop/soil/pred bio 2/wc2.1_2.5m_bioc_CNRM-CM6-1_ssp126_2041-2060.tif"))

raster_stack <- stack(raster1, raster2, raster3, raster4, raster5, raster6, raster7, raster8, raster9, raster10, raster11, raster12)
raster_file <- stackApply(raster_stack, indices=rep(1,nlayers(raster_stack)), fun=mean)

raster_stack2 <- stack(raster_file, raster14)
raster_file2 <- stackApply(raster_stack2, indices=rep(1,nlayers(raster_stack2)), fun=diff)

bd_raster <- crop(raster_file2, extent(bd))
plot(bd_raster)
bd_raster2 <- mask(bd_raster, bd)
plot(bd_raster2)
plot(bd, add=TRUE)

#prediction precipitation diff

raster1 <- raster(here::here("C:/Users/syedm/Desktop/soil/precipitation/wc2.1_2.5m_prec_01.tif"))
raster2 <- raster(here::here("C:/Users/syedm/Desktop/soil/precipitation/wc2.1_2.5m_prec_02.tif"))
raster3 <- raster(here::here("C:/Users/syedm/Desktop/soil/precipitation/wc2.1_2.5m_prec_03.tif"))
raster4 <- raster(here::here("C:/Users/syedm/Desktop/soil/precipitation/wc2.1_2.5m_prec_04.tif"))
raster5 <- raster(here::here("C:/Users/syedm/Desktop/soil/precipitation/wc2.1_2.5m_prec_05.tif"))
raster6 <- raster(here::here("C:/Users/syedm/Desktop/soil/precipitation/wc2.1_2.5m_prec_06.tif"))
raster7 <- raster(here::here("C:/Users/syedm/Desktop/soil/precipitation/wc2.1_2.5m_prec_07.tif"))
raster8 <- raster(here::here("C:/Users/syedm/Desktop/soil/precipitation/wc2.1_2.5m_prec_08.tif"))
raster9 <- raster(here::here("C:/Users/syedm/Desktop/soil/precipitation/wc2.1_2.5m_prec_09.tif"))
raster10 <- raster(here::here("C:/Users/syedm/Desktop/soil/precipitation/wc2.1_2.5m_prec_10.tif"))
raster11 <- raster(here::here("C:/Users/syedm/Desktop/soil/precipitation/wc2.1_2.5m_prec_11.tif"))
raster12 <- raster(here::here("C:/Users/syedm/Desktop/soil/precipitation/wc2.1_2.5m_prec_12.tif"))

raster14 <- raster(here::here("C:/Users/syedm/Desktop/soil/pred precipitation 2/wc2.1_2.5m_prec_CNRM-CM6-1_ssp126_2041-2060.tif"))

raster_stack <- stack(raster1, raster2, raster3, raster4, raster5, raster6, raster7, raster8, raster9, raster10, raster11, raster12)
raster_file <- stackApply(raster_stack, indices=rep(1,nlayers(raster_stack)), fun=mean)

raster_stack2 <- stack(raster_file, raster14)
raster_file2 <- stackApply(raster_stack2, indices=rep(1,nlayers(raster_stack2)), fun=diff)

bd_raster <- crop(raster_file2, extent(bd))
plot(bd_raster)
bd_raster2 <- mask(bd_raster, bd)
plot(bd_raster2)
plot(bd, add=TRUE)











#do black neighborhoods sell more than white ones

data_census <- read.csv("data_curate.csv")

library(tidycensus)
library(tidyverse)
library(maps)
library(sf)
library(data.table)
data(state.fips)

vt1 <- get_acs(geography = "tract", 
               variables = c(hh_number="DP02_0001"), 
               state="GA",
               year = 2015)

vt1_dt <- as.data.table(vt1)
data_wide_vt1 <- dcast(vt1_dt, GEOID ~ variable, fun.aggregate = mean,
                       value.var=c("estimate"))

write.csv(data_wide_vt1, file="data_curate_2.csv")

mean(data_wide_vt1$hh_number)

data_org <- read.csv("data_curate.csv")
data_new <- read.csv("data_curate_2.csv")

data_merge <- merge(data_org, data_new, by.x=c("GEOID2"), by.y=c("GEOID2"))

data <- read.csv("atlanta single family 6116.csv")

data_m <- merge(data, data_merge, by.x=c("tract2010"), by.y=c("Tract2015"))


data_m %>% 
  distinct(X, .keep_all = T) -> df

summary(df$salesprice)
summary(data$salesprice)

df_tract = df %>% group_by(tract2010)  %>%
  summarise(pct_black = mean(pct_black),
            sold = n(),
            hh = mean(hh_number))

df_tract$perc <- (df_tract$sold/df_tract$hh)*100

plot(df_tract$pct_black, df_tract$perc, xlab="Pct_black", ylab="% houses sold in tract")
fit <- glm(df_tract$perc~df_tract$pct_black)
co <- coef(fit)
abline(fit, col="blue", lwd=2)

df_tract %>%
  mutate(quantile = ntile(pct_black, 10)) -> df_tract

df_tract$quantile <- as.factor(df_tract$quantile)

levels(df_tract$quantile)

group_by(df_tract, quantile)  %>%
  summarise(perc = mean(perc, na.rm=TRUE),
            count=n())

res <- aov(perc ~ as.factor(quantile), data=df_tract)
summary(res)

#original stuff

d <- read.csv("data_ds.csv")
d %>% select(X) -> d
data <- merge(data, d, by=c("X"))

data <- read.csv("atlanta single family 6116.csv")
data <- data[1:4978,] #3626 #4978
data <- subset(data, taxpaid>0)
summary(data$taxpaid)

data <- data[!is.na(data$median_house_value),]

data %>% select(salesprice, calcacres, stories, age, centheat, totbath, ENGMeanScaleScore15, median_income, sqft,
                median_house_value, HHsize, pct_black, pct_white, landuse) -> data

mod1 <- lm(log(taxpaid_ass) ~ log(calcacres) + stories + age + age_sq + centheat + totbath + ENGMeanScaleScore15 + median_income +
           median_house_value + HHsize + pct_black, data=data)

middlesch
census tract
I((tax_last/sqft4)/ass_last)
I((ass_last/sqft4)/salesprice)

data$sqft4 <- as.numeric(data$sqft3)

mod1 <- lm(log(taxpaid_ass) ~ calcacres + stories + age + age_sq + centheat + totbath + ENGMeanScaleScore15 + median_income +
             median_house_value + HHsize + years_since_remodel + rmtot + fixtot + style + extwall + fuel + attic + fronting +
             street1 + topo1 + util1 + parktype + parkprox + dcdu + pct_renter_occupied + pct_HSdiploma + 
             revcode + deedtype + saleval + saletype + saledtyearyyyy + saledtmonthmon + sqft4 + as.factor(location) +
             as.factor(luc) + zoning + xdist + xdist_sq + ydist + ydist_sq + as.factor(parcel_district) + CitySalesdata +
             pct_black, data=data)

summary(mod1)

data$census <- as.factor(data$BlockGroup)

vcov_firm <- cluster.vcov(mod1, as.factor(data$census))
coeftest(mod1, vcov_firm)

mod1 <- lm(log(I((tax_last/sqft4)/ass_last)) ~ pct_black + pct_renter_occupied + I(median_income/1000) + 
           pct_HSdiploma + I(median_house_value/1000) + pct_above_f + pct_below_f +
           revcode + deedtype + saleval + saletype + saledtyearyyyy + 
           saledtmonthmon, data=data)

summary(mod1)

data$census <- as.factor(data$BlockGroup)

vcov_firm <- cluster.vcov(mod1, as.factor(data$census))
coeftest(mod1, vcov_firm)

mod1 <- lm(log(I((ass_last)/salesprice)) ~ pct_black + pct_renter_occupied + I(median_income/1000) + 
             pct_HSdiploma + I(median_house_value/1000) + pct_above_f + pct_below_f +
             revcode + deedtype + saleval + saletype + saledtyearyyyy + saledtmonthmon +
             calcacres + stories + age + age_sq + sqft4 + centheat + totbath + years_since_remodel + 
             rmtot + fixtot + style + extwall + fuel + attic + fronting +
             street1 + topo1 + util1 + parktype + parkprox + dcdu, data=data)

summary(mod1)

data$census <- as.factor(data$BlockGroup)

vcov_firm <- cluster.vcov(mod1, as.factor(data$census))
coeftest(mod1, vcov_firm)

mod1 <- lm(log(I((ass_last)/salesprice)) ~ pct_black + pct_renter_occupied + I(median_income/1000) + 
             pct_HSdiploma + I(median_house_value/1000) + pct_above_f + pct_below_f +
             revcode + deedtype + saleval + saletype + saledtyearyyyy + saledtmonthmon +
             calcacres + stories + age + age_sq + sqft4 + centheat + totbath + years_since_remodel + 
             rmtot + fixtot + style + extwall + fuel + attic + fronting +
             street1 + topo1 + util1 + parktype + parkprox + dcdu + as.factor(location) +
             as.factor(luc) + zoning, data=data)

summary(mod1)

data$census <- as.factor(data$BlockGroup)

vcov_firm <- cluster.vcov(mod1, as.factor(data$census))
coeftest(mod1, vcov_firm)

mod1 <- lm(log(I((ass_last)/salesprice)) ~ pct_black + pct_renter_occupied + I(median_income/1000) + 
             pct_HSdiploma + I(median_house_value/1000) + pct_above_f + pct_below_f +
             revcode + deedtype + saleval + saletype + saledtyearyyyy + saledtmonthmon +
             calcacres + stories + age + age_sq + sqft4 + centheat + totbath + years_since_remodel + 
             rmtot + fixtot + style + extwall + fuel + attic + fronting +
             street1 + topo1 + util1 + parktype + parkprox + dcdu + as.factor(location) +
             as.factor(luc) + zoning + xdist + xdist_sq + ydist + ydist_sq, data=data)

summary(mod1)

data$census <- as.factor(data$BlockGroup)

vcov_firm <- cluster.vcov(mod1, as.factor(data$census))
coeftest(mod1, vcov_firm)

mod1 <- lm(log(I((ass_last)/salesprice)) ~ pct_black + pct_renter_occupied + I(median_income/1000) + 
             pct_HSdiploma + I(median_house_value/1000) + pct_above_f + pct_below_f +
             revcode + deedtype + saleval + saletype + saledtyearyyyy + saledtmonthmon +
             calcacres + stories + age + age_sq + sqft4 + centheat + totbath + years_since_remodel + 
             rmtot + fixtot + style + extwall + fuel + attic + fronting +
             street1 + topo1 + util1 + parktype + parkprox + dcdu + as.factor(location) +
             as.factor(luc) + zoning + xdist + xdist_sq + ydist + ydist_sq +
             as.factor(parcel_district) + as.factor(middlesch), data=data)

summary(mod1)

data$census <- as.factor(data$BlockGroup)

vcov_firm <- cluster.vcov(mod1, as.factor(data$census))
coeftest(mod1, vcov_firm)

data %>%
  mutate(quantile = ntile(pct_black, 10)) -> data1

data1$quantile <- as.factor(data1$quantile)

levels(data1$quantile)

group_by(data1, quantile) %>%
  summarise(
    count = n(),
    mean = mean(I((tax_last/sqft4)/ass_last), na.rm = TRUE),
    sd = sd(I((tax_last/sqft4)/ass_last), na.rm = TRUE)
  )

res <- aov(I((ass_last)/salesprice) ~ quantile, data=data1)
summary(res)

TukeyHSD(res)

pairwise.t.test(data1$taxpaid_ass_last, data1$quantile, p.adjust.method="BH")

[1] "Long"          "Harper-Archer" "Price"         "Brown"         "Sylvan"        "Young"         "King"          "Bunche"        "Sutton"       
[10] "Inman"

data1 <- subset(data, middlesch=="Inman")
summary(data1$pct_black)
summary(data1$median_income)

mod1 <- lm(log(I((ass_last)/salesprice)) ~ pct_black + pct_renter_occupied + I(median_income/1000) + 
             pct_HSdiploma + I(median_house_value/1000) + pct_above_f + pct_below_f +
             revcode + deedtype + saleval + saletype + saledtyearyyyy + saledtmonthmon +
             calcacres + stories + age + age_sq + sqft4 + centheat + totbath + years_since_remodel + 
             rmtot + fixtot + style + extwall + fuel + attic + fronting +
             street1 + topo1 + util1 + parktype + parkprox + dcdu + zoning + xdist + xdist_sq + ydist + ydist_sq, data=data1)

summary(mod1)

data1$census <- as.factor(data1$BlockGroup)

vcov_firm <- cluster.vcov(mod1, as.factor(data1$census))
coeftest(mod1, vcov_firm)

# + calcacres + stories + age + age_sq + centheat + totbath

mod1 <- lm(log(taxpaid_ass) ~ pct_black + pct_renter_occupied + median_income + pct_HSdiploma + 
           pct_collegeDegree + pct_graduateDegree + median_house_value + pct_above_f + pct_below_f + 
           revcode + deedtype + saleval + saletype + saledtyearyyyy + saledtmonthmon +
           calcacres + stories + age + centheat + totbath, data=data)
summary(mod1)








