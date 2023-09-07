#https://www.usgs.gov/programs/gap-analysis-project/science/land-cover-data-download
#https://cran.r-project.org/web/packages/ForestTools/vignettes/treetop_analysis.html
#https://earthenginepartners.appspot.com/science-2013-global-forest/download_v1.7.html
#https://www.agi.com/tech-tips/tech-tips-october/gathering-and-importing-usgs-terrain-data-into-stk
#https://www.tylermw.com/adding-open-street-map-data-to-rayshader-maps-in-r/
#https://hal.archives-ouvertes.fr/hal-03108315/document
#https://www.tylermw.com/3d-ggplots-with-rayshader/
#https://www.agi.com/tech-tips/tech-tips-october/gathering-and-importing-usgs-terrain-data-into-stk

#library(rgeoboundaries)
library(sf)
library(raster)
library(here)
library(ggplot2)
library(viridis)
library(dplyr)
library(rayshader)
library(rasterVis)
library(RColorBrewer)

#land cover
#https://www.usgs.gov/programs/gap-analysis-project/science/land-cover-data-download

IGBP_raster <- raster(here::here("C:/Users/syedm/Desktop/Columbus/gaplf2011lc_v30_OH/gaplf2011lc_v30_oh.tif"))

IGBP_raster <- projectRaster(IGBP_raster, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

#IGBP_raster <- raster::mask(IGBP_raster, as_Spatial(map_boundary))

#plot(IGBP_raster, xlab = "", ylab = "", xaxt='n', yaxt = 'n')

#plotting after cropping

e <- extent(-83.25, -82.75, 39.8, 40.2)
rc <- crop(IGBP_raster, e)
plot(rc)
points(x=data$hlong, y=data$hlat, 
       col=rainbow)

plot(rc) +
  geom_point(data = data, aes(x = hlong, y = hlat, colour = as.factor(freq2)), size = 1, alpha=0.25) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Longitude", y="Latitude", colour="Submarket")

point <- SpatialPoints(coords = cbind(data$hlong, data$hlat))


#slope
area_slope <- terrain(rc, opt = 'slope', neighbors=8, unit = 'degrees')

#aspect
area_aspect <- terrain(rc, opt = 'aspect', unit = 'degrees')

#plot

#detach("package:ggplot2", unload=TRUE)

my.col <- colorRampPalette(brewer.pal(11, "RdBu"))(diff(range(data$price)))

rasterVis::levelplot(rc, layers=1,
                     margin = list(x = TRUE, 
                                   y = TRUE),
                     col.regions = terrain.colors(16),
                     #main=list('Red = more expensive homes, black = less expensive homes',side=1,line=1),
                     xlab = list(label = "", 
                                 vjust = -0.25),
                     sub = list(
                       label = "Elevation level (m)",
                       font = 1,
                       cex = .9,
                       hjust = 0.70)) + 
  #latticeExtra::layer(panel.points(x=data$hlong, y=data$hlat, col=data$freq2, alpha=0.30))
  latticeExtra::layer(panel.points(x=data$hlong, y=data$hlat, col=my.col[data$price], alpha=0.30))



#get raster values

#crs(raster1)
#CRS arguments: +proj=longlat +datum=WGS84 +no_defs 
data <- read.csv("columbus_update.csv")
coor <- select(data, hlong, hlat)

spdf <- SpatialPointsDataFrame(coords=coor[,1:2], data=coor, proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs"))
p <- spTransform(spdf, crs(area_slope))
slope <- extract(area_slope, p)

#terrain
#file:///C:/Users/syedm/Downloads/remotesensing-12-01156.pdf
#ASTER Global Digital Elevation Model (GDEM) and ASTER Global Water Body Dataset (ASTWBD)

raster1 <- raster(here::here("C:/Users/syedm/Desktop/Columbus/gaplf2011lc_v30_OH/USGS_1_n40w083_20210111.tif"))
raster2 <- raster(here::here("C:/Users/syedm/Desktop/Columbus/gaplf2011lc_v30_OH/USGS_1_n40w084_20200302.tif"))
raster3 <- raster(here::here("C:/Users/syedm/Desktop/Columbus/gaplf2011lc_v30_OH/USGS_1_n41w083_20200302.tif"))
raster4 <- raster(here::here("C:/Users/syedm/Desktop/Columbus/gaplf2011lc_v30_OH/USGS_1_n41w084_20200302.tif"))

x <- list(raster1, raster2, raster3, raster4)
m <- do.call(merge, x)
e <- extent(-83.25, -82.75, 39.8, 40.2)
rc <- crop(m, e)
bryce_mat = raster_to_matrix(rc)

rm(m)
rm(bryce_mat)

bryce_small <- bryce_mat
bryce_small %>% 
  height_shade() %>% 
  plot_map()

bryce_small %>% 
  height_shade() %>% 
  add_overlay(sphere_shade(bryce_small, texture = "desert", 
                           zscale=4, colorintensity = 5), alphalayer=0.5) %>%
  plot_map()

bryce_small %>% 
  height_shade() %>% 
  add_overlay(sphere_shade(bryce_small, texture = "desert", 
                           zscale=4, colorintensity = 5), alphalayer=0.5) %>%
  add_shadow(lamb_shade(bryce_small,zscale = 6),0) %>%
  plot_map()

bryce_small %>% 
  height_shade() %>% 
  add_overlay(sphere_shade(bryce_small, texture = "desert", 
                           zscale=4, colorintensity = 5), alphalayer=0.5) %>%
  add_shadow(lamb_shade(bryce_small,zscale=6), 0) %>%
  add_shadow(texture_shade(bryce_small,detail=8/10,contrast=9,brightness = 11), 0.1) %>%
  plot_map()

bryce_small %>% 
  height_shade() %>% 
  add_overlay(sphere_shade(bryce_small, texture = "desert", 
                           zscale=4, colorintensity = 5), alphalayer=0.5) %>%
  add_shadow(lamb_shade(bryce_small,zscale=6), 0) %>%
  add_shadow(ambient_shade(bryce_small), 0) %>%
  add_shadow(texture_shade(bryce_small,detail=8/10,contrast=9,brightness = 11), 0.1) %>%
  plot_map()

base_map = bryce_small %>% 
  height_shade() %>%
  add_overlay(sphere_shade(bryce_small, texture = "imhof1", colorintensity = 5), alphalayer=0.5) %>%
  add_shadow(lamb_shade(bryce_small), 0) %>%
  add_shadow(ambient_shade(bryce_small),0) %>% 
  add_shadow(texture_shade(bryce_small,detail=8/10,contrast=9,brightness = 11), 0.1)

base_map %>% 
  plot_3d(bryce_small, windowsize=c(1200,800)) #+ 
  #render_points(extent=e, lat = data$hlat, long = data$hlong, 
  #altitude = data$tranamt, color="black")
render_camera(theta=240,  phi=30, zoom=0.3,  fov=60)
render_snapshot()



osm_bbox = c(-83.25, -82.75, 39.8, 40.2)
bryce_water_lines = opq(osm_bbox) %>% 
  add_osm_feature("waterway") %>% 
  osmdata_sf() 
bryce_water_lines
bryce_streams = st_transform(bryce_water_lines$osm_lines,crs=crs(m))

stream_layer = generate_line_overlay(bryce_streams,extent = e,
                                     linewidth = 4, color="skyblue2", 
                                     heightmap = bryce_small)

base_map %>% 
  add_overlay(stream_layer, alphalayer = 0.8) %>% 
  add_overlay(trails_layer) %>%
  plot_map()


#extracting terrain values

crs(m)
#CRS arguments: +proj=longlat +datum=WGS84 +no_defs 
data <- read.csv("columbus_update.csv")
coor <- select(data, hlong, hlat)

#.rs.unloadPackage("tidyr")

spdf <- SpatialPointsDataFrame(coords=coor[,1:2], data=coor, proj4string=CRS("+init=epsg:4326"))
p <- spTransform(spdf, crs(m))
extract(m, p)






#tree canopy 2000
#http://earthenginepartners.appspot.com/science-2013-global-forest/download_v1.7.html
#canopy closure for all vegetation taller than 5m in height, encoded as a percentage per output grid cell, in the range 0-100

raster1 <- raster(here::here("C:/Users/syedm/Desktop/Columbus/Hansen_GFC-2019-v1.7_treecover2000_50N_090W.tif"))
e <- extent(-83.25, -82.75, 39.8, 40.2)
rc <- crop(raster1, e)
bryce_mat = raster_to_matrix(rc)


#canopy height 2019
#https://glad.umd.edu/dataset/gedi
#0-60 Forest canopy height, meters
#101 Water
#102 Snow/ice
#103 No data

raster1 <- raster(here::here("C:/Users/syedm/Desktop/Columbus/Forest_height_2019_NAM.tif"))
e <- extent(-83.25, -82.75, 39.8, 40.2)
rc <- crop(raster1, e)

#getting water from this file

raster1 <- raster(here::here("C:/Users/syedm/Desktop/Columbus/Forest_height_2019_NAM.tif"))
e <- extent(-83.70, -82.30, 39.65, 40.35)
rc <- crop(raster1, e)

water <- rc
water[rc != 101] <- NA
plot(water, col="blue")
#points(x=data$hlong, y=data$hlat)

p1 <- as.data.frame(water, xy=TRUE)
p1 = p1[is.na(p1[,3]),1:2]

p2 = as.data.frame(coor, xy=TRUE)
dnear = knnx.dist(p2, p1, k=1)
water[is.na(water)] = dnear[,1]

data <- read.csv("columbus_update.csv")
coor <- select(data, hlong, hlat)

spdf <- SpatialPointsDataFrame(coords=coor[,1:2], data=coor, proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs"))
p <- spTransform(spdf, crs(water))
slope2 <- extract(water, p)

water2 <- water
water2[water == 101] <- NA

#p2$Forest_height_2019_NAM <- 1
#p2 <- rename(p2, x=hlong)
#p2 <- rename(p2, y=hlat)

#df= p1 %>% full_join(p2,by=c("x", "y"))
#df$val <- rowSums(df[,c("Forest_height_2019_NAM.x", "Forest_height_2019_NAM.y")], na.rm=TRUE)
#df$Forest_height_2019_NAM.x <- NULL
#df$Forest_height_2019_NAM.y <- NULL

#df <- rename(df, lon=x)
#df <- rename(df, lat=y)
#df <- rename(df, value=val)

#d <- gridDistance(df, origin = 101, omit = "NA")

#p2 = p2[!is.na(p2[,3]),1:2]

#p3 <- p1[complete.cases(p1), ]
#xym <- cbind(p3$x, p3$y)
#p = Polygon(xym)
#ps = Polygons(list(p),1)
#sps = SpatialPolygons(list(ps))
#plot(sps)

raster1 <- raster(here::here("C:/Users/syedm/Desktop/Columbus/Forest_height_2019_NAM.tif"))
e <- extent(-83.67, -82.33, 39.67, 40.35)
rc <- crop(raster1, e)

water <- rc
water[rc != 101] <- 1
plot(water, col="blue")

data <- read.csv("columbus_update.csv")
coor <- select(data, hlong, hlat)

#e <- extent(-83.65, -82.35, 39.69, 40.33)
e1 <- extent(-83.67, -83.00, 40.01, 40.35)
e2 <- extent(-83.00, -82.33, 40.01, 40.35)
e3 <- extent(-83.67, -83.00, 39.67, 40.01)
e4 <- extent(-83.00, -82.33, 39.67, 40.01)

rc1 <- crop(water, e1)
rc2 <- crop(water, e2)
rc3 <- crop(water, e3)
rc4 <- crop(water, e4)

x <- list(rc1, rc2, rc3, rc4)
m <- do.call(merge, x)

xy <- coor
icell <- cellFromXY(rc1, xy)
d1 <- gridDistance(rc1, origin = 101)/1000
d2 <- gridDistance(rc2, origin = 101)/1000
d3 <- gridDistance(rc3, origin = 101)/1000
d4 <- gridDistance(rc4, origin = 101)/1000

x <- list(d1, d2, d3, d4)
m <- do.call(merge, x)

data <- read.csv("columbus_update.csv")
coor <- dplyr::select(data, hlong, hlat)

spdf <- SpatialPointsDataFrame(coords=coor[,1:2], data=coor, proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs"))
p <- spTransform(spdf, crs(m))
slope <- extract(m, p)

d1_df <- as.data.frame(d1, xy=TRUE)
d2_df <- as.data.frame(d2, xy=TRUE)
d3_df <- as.data.frame(d3, xy=TRUE)
d4_df <- as.data.frame(d4, xy=TRUE)

#tree canopy 2016
#https://www.mrlc.gov/data?f%5B0%5D=category%3ATree%20Canopy
#percent tree canopy cover layer containing values ranging from 0 to 100 percent, 
#with each individual value representing the area or proportion of that 30 m cell covered by tree canopy

raster1 <- raster("C:/Users/syedm/Downloads/usfs_carto_CONUS_2016/usfs_2016_treecanopy_cartographic_12-14-2018.img")
coor2 <- coor
str(coor2)
coordinates(coor2)  <-  c("hlong",  "hlat")
proj4string(coor2)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
sites_transformed<-spTransform(coor2, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

#xy <- cbind(c(-83.25, -82.75), c(39.8, 40.2))
#project(xy, "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
raster1 <- raster("C:/Users/syedm/Downloads/usfs_carto_CONUS_2016/usfs_2016_treecanopy_cartographic_12-14-2018.img")
e <- extent(1078702, 1114507, 1937037, 1987207)
#e <- extent(1045954, 1145839, 1919124, 2006382)
rc <- crop(raster1, e)
plot(rc)
points(sites_transformed, col=data$price)

test_df <- as.data.frame(rc, xy=TRUE)
colnames(test_df) <- c("x", "y", "value")
test_df$value <- ifelse(test_df$value>=50, 50, test_df$value)

r <- raster(nrow=1673, ncol=1194, ext=extent(1078695, 1114515, 1937025, 1987215), crs="+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
values(r) <- test_df$value
plot(r)

#canopy height

#get only rows that are needed
data2 <- subset(data, hlat2>=1937025 & hlat2<=1987215 & hlong2>=1078695 & hlong2<=1114515)

gplot(r) +
  geom_raster(aes(fill = value)) + guides(fill=guide_legend(title="Canopy cover (%)")) + ggtitle("Canopy cover") +
  scale_fill_gradientn(colours = rev(terrain.colors(10))) +
  geom_point(data = data2, aes(x = hlong2, y = hlat2, colour = price), size = 1, alpha=0.35) +
  #scale_color_manual(values=c("red", "blue")) +
  scale_colour_gradient(low = "blue", high = "red", na.value = NA) +
  coord_equal()



#land use 2019
#https://www.mrlc.gov/data/nlcd-2019-land-cover-conus
#https://www.arcgis.com/home/item.html?id=3ccf118ed80748909eb85c6d262b426f
#https://stackoverflow.com/questions/56771620/get-values-from-adjacent-cells-raster-r
raster1 <- raster("C:/Users/syedm/Downloads/nlcd_2019_land_cover_l48_20210604/nlcd_2019_land_cover_l48_20210604.img")
#e <- extent(-83.66, -82.36, 39.69, 40.33)
#rc <- crop(raster1, e)

data <- read.csv("columbus_update.csv")
data$price <- ifelse(data$tranamt>=1000000, 1000000, data$tranamt)
coor <- dplyr::select(data, hlong, hlat)
coor2 <- coor
str(coor2)
coordinates(coor2)  <-  c("hlong",  "hlat")
proj4string(coor2)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
sites_transformed<-spTransform(coor2, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

#xy <- cbind(c(-83.66, -82.35), c(39.68, 40.33))
#project(xy, "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
e <- extent(1045954, 1145839, 1919124, 2006382)
e <- extent(1078702, 1114507, 1937037, 1987207) #new one #remove
rc <- crop(raster1, e)
plot(rc)

xy <- cbind(c(data$hlong), c(data$hlat))
c <- project(xy, "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
data$c <- c

##extra
test_df <- as.data.frame(rc, xy=TRUE)
colnames(test_df) <- c("x", "y", "value")
test_df$value <- ifelse(test_df$value==21, 0.90, ifelse(test_df$value==22, 0.65,
                 ifelse(test_df$value==23, 0.35, ifelse(test_df$value==24, 0.10, 0.90))))

r <- raster(nrow=1673, ncol=1194, ext=extent(1078695, 1114515, 1937025, 1987215), crs="+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
values(r) <- test_df$value
plot(r)

#open space

#get only rows that are needed
data2 <- subset(data, hlat2>=1937025 & hlat2<=1987215 & hlong2>=1078695 & hlong2<=1114515)

gplot(r) +
  geom_raster(aes(fill = value)) + guides(fill=guide_legend(title="Open space (fraction)")) + ggtitle("Open space and home prices") +
  scale_fill_gradientn(colours = rev(terrain.colors(7))) +
  geom_point(data = data2, aes(x = hlong2, y = hlat2, colour = price), size = 1, alpha=0.30) +
  #scale_color_manual(values=c("red", "blue")) +
  scale_colour_gradient(low = "blue", high = "red", na.value = NA) +
  coord_equal()



gplot(rc) +
  geom_raster(aes(fill = as.factor(value))) +
  #scale_fill_stepsn(n.breaks = 12, colours = terrain.colors(12)) +
  #geom_point(data = data, aes(x = hlong, y = hlat, colour = price), size = 1, alpha=0.50) +
  #scale_color_gradientn(colours = rainbow(10)) +
  coord_equal()

#neighbors

c <- cbind(coor[,1], coor[,2])
c2 <- project(c, "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
place <- extract(rc, SpatialPoints(cbind(c2[,1], c2[,2])), cellnumbers=TRUE)

place2 <- extract(raster1, SpatialPoints(cbind(c2[,1], c2[,2])), cellnumbers=TRUE)

cells <- c(place$cells)
cells2 <- c(place2$cells)

cells[is.na(cells)] <- 1
ad <- t(sapply(cells, adj, rr=raster(rc), ngb=nbrs, global=FALSE))
ad2 <- ad
ad2[] <- extract(rc, as.vector(ad))

water <- rc
water[rc != 11] <- NA
plot(water)
waterdist <- distance(water)
plot(waterdist)
plot(water, add=TRUE, col="blue", legend=FALSE)


#https://stackoverflow.com/questions/56771620/get-values-from-adjacent-cells-raster-r
#https://gis.stackexchange.com/questions/142156/r-how-to-get-latitudes-and-longitudes-from-a-rasterlayer
#https://gis.stackexchange.com/questions/229455/calculating-areas-of-different-raster-classes-in-r
#https://gis.stackexchange.com/questions/336093/how-to-detect-green-areas-in-google-earth-imagery-using-r
#https://mmeredith.net/blog/2012/1212_GIS_layer_for_Distance_from.htm
#https://www.seascapemodels.org/rstats/2020/02/08/calculating-distances-in-R.html
#https://www.researchgate.net/post/How-can-I-speed-up-the-Euclidian-distance-estimation-in-a-raster-using-R
#https://gis.stackexchange.com/questions/360516/measure-shortest-distance-between-raster-cell-to-raster-cell-of-another-raster-i
#https://stackoverflow.com/questions/14025176/finding-k-nearest-neighbor-for-only-one-point-not-an-entire-matrix-using-r/14025875

#extra chapter for adjacent cells
adj <- function(x, rr, ngb, global) {
  a <- x + ngb
  # the below line is not strictly needed, so it should be faster without it
  # a[a < 1 | a > ncell(rr)] <- NA
  if (!global) {
    col <- colFromCell(rr, x)
    if (col == 1) {
      a[c(1,4,6)] <- NA
    } else if (col==nc) {
      a[c(3,5,8)] <- NA
    }
  }
  a
}






#ndvi
#https://www.ncei.noaa.gov/products/climate-data-records/normalized-difference-vegetation-index
#https://earthobservatory.nasa.gov/features/MeasuringVegetation/measuring_vegetation_2.php
raster1 <- raster(here::here("C:/Users/syedm/Downloads/UrbanFraction_1_8_dgr_GEOTIFF_v1/UrbanFraction_1_8_dgr_GEOTIFF_Projections_SSPs1-5_2010-2100_v1/ssp5_2020.tif"))
plot(raster1)
data <- read.csv("columbus_update.csv")
coor <- dplyr::select(data, hlong, hlat)

spdf <- SpatialPointsDataFrame(coords=coor[,1:2], data=coor, proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs"))
p <- spTransform(spdf, crs(raster1))
slope <- extract(raster1, p)




data <- read.csv("columbus_update.csv")

sub2_1 <- subset(data, freq2==1)
sub2_2 <- subset(data, freq2==2)
mean(sub2_1$dist2)
mean(sub2_2$dist2)

sub3_1 <- subset(data, freq3==1)
sub3_2 <- subset(data, freq3==2)
sub3_3 <- subset(data, freq3==3)

