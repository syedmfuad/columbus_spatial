library(readxl)
library(splm)
library(rgdal)
library(spdep)
library(plm)
library(lmtest)
library(fields)
library(tidyverse)
library(sf)
library(leaflet)
library(mapview)
library(ggmap)
library(ggridges)
library(viridis)
library(rayshader)

data <- read.csv("columbus_update.csv")
data$price <- ifelse(data$tranamt>=1000000, 1000000, data$tranamt)

#API
ggmap::register_google(key = "########################")

#Atlanta map

#mylocation <- c(-83.25, 39.8, -82.75, 40.2)
mymap <- get_map(location="columbus ohio", source="stamen", maptype="terrain-background", crop=FALSE)
map1 <- ggmap(mymap) + 
  scale_x_continuous(limits = c(-83.25, -82.75), expand = c(0, 0)) +
  scale_y_continuous(limits = c(39.80, 40.20), expand = c(0, 0))

map1 +
  geom_point(data = data, aes(x = hlong, y = hlat, colour = as.factor(freq2)), size = 1, alpha=0.25) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Longitude", y="Latitude", colour="Submarket")

map1 +
  geom_point(data = data, aes(x = hlong, y = hlat, colour = as.factor(freq3)), size = 1, alpha=0.25) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  labs(x = "Longitude", y="Latitude", colour="Submarket")

mtplot = map1 +
  geom_point(data = data, aes(x = hlong, y = hlat, colour = price), size = 1, alpha=0.50) + 
  scale_color_gradientn(colours = rainbow(10)) +
  labs(x = "Longitude", y="Latitude", colour="Price")
mtplot

#not good 
plot_gg(mtplot, multicore = TRUE, windowsize = c(1400,866), sunangle=225,
        zoom = 0.60, phi = 30, theta = 45)
render_snapshot(clear = TRUE)
rgl.close()

#with highways

get_local_spot <-  get_map(c(left = -83.25, bottom = 39.8, right = -82.75, top = 40.2)) 
map12 <- ggmap(get_local_spot)

get_local_spot <-  get_map(c(left = -83.50, bottom = 39.70, right = -82.30, top = 40.35)) 
map12 <- ggmap(get_local_spot)

map12 + geom_point(data = data, aes(x = hlong, y = hlat, colour = as.factor(freq2)), size = 1, alpha=0.25) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Longitude", y="Latitude", colour="Submarket")

map12 +
  geom_point(data = data, aes(x = hlong, y = hlat, colour = as.factor(freq3)), size = 1, alpha=0.25) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  labs(x = "Longitude", y="Latitude", colour="Submarket")

map12 + geom_point(data = data, aes(x = hlong, y = hlat, colour = price), size = 1, alpha=0.50) +
  scale_color_gradientn(colours = rainbow(10)) +
  labs(x = "Longitude", y="Latitude", colour="Price")

#terrain with highways #no
get_local_spot_ter <-  get_map("Columbus Ohio", maptype = "terrain", zoom = 10) 
map1_ter <- ggmap(get_local_spot_ter) + 
  scale_x_continuous(limits = c(-83.25, -82.75), expand = c(0, 0)) +
  scale_y_continuous(limits = c(39.8, 40.2), expand = c(0, 0))
map1_ter + geom_point(data = data, aes(x = hlong, y = hlat, colour = as.factor(freq2)), size = 1, alpha=0.25) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Longitude", y="Latitude", colour="Submarket")

#terrain without highways 

get_local_spot_sat <-  get_map("Columbus Ohio", maptype = "satellite", zoom = 10) 
map1_sat <- ggmap(get_local_spot_sat) + 
  scale_x_continuous(limits = c(-83.25, -82.75), expand = c(0, 0)) +
  scale_y_continuous(limits = c(39.8, 40.2), expand = c(0, 0))
mtplot = map1_sat + geom_point(data = data, aes(x = hlong, y = hlat, colour = tranamt), size = 1, alpha=0.25) +
  scale_color_gradientn(colours = rainbow(10)) +
  labs(x = "Longitude", y="Latitude", colour="Submarket")
mtplot

#satellite with highways #no
get_local_spot_hy <-  get_map("Columbus Ohio", maptype = "hybrid", zoom = 10) 
map1_hy <- ggmap(get_local_spot_hy) + 
  scale_x_continuous(limits = c(-83.25, -82.75), expand = c(0, 0)) +
  scale_y_continuous(limits = c(39.8, 40.2), expand = c(0, 0))
mtplot = map1_hy + geom_point(data = data, aes(x = hlong, y = hlat, colour = as.factor(freq2)), size = 1, alpha=0.25) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Longitude", y="Latitude", colour="Submarket")
mtplot

#getting Atlanta map #no
get_local_spot_rd <-  get_map("Columbus Ohio", maptype = "roadmap", zoom = 10) 
map1_rd <- ggmap(get_local_spot_rd) + 
  scale_x_continuous(limits = c(-83.25, -82.75), expand = c(0, 0)) +
  scale_y_continuous(limits = c(39.8, 40.2), expand = c(0, 0)) + 
  geom_point(data = data, aes(x = hlong, y = hlat, color=tranamt), size = 1, alpha=0.25) +
  #scale_color_brewer(palette = "Dark2") +
  scale_color_gradientn("tranamt", colors=rainbow(10)) +
  labs(x = "Longitude", y="Latitude", colour="Submarket")
map1_rd

#not good
plot_gg(map1_rd, multicore = TRUE, width=8, height=8, scale=300, #windowsize = c(1400,866), sunangle=225,
        zoom = 0.60, phi = 60, theta = 45, background="#afceff", shadowcolor="#3a4f70")
render_snapshot(clear = TRUE)


theme_set(theme_bw(16))
get_local_spot_bw <- qmap("Columbus Ohio", zoom = 10, legend = "topleft") 

get_local_spot_bw + 
  scale_x_continuous(limits = c(-83.25, -82.75), expand = c(0, 0)) +
  scale_y_continuous(limits = c(39.8, 40.2), expand = c(0, 0)) +
  geom_point(data = data, aes(x = hlong, y = hlat, colour = as.factor(freq2)), size = 1, alpha=0.25) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Longitude", y="Latitude", colour="Submarket")


#mapping by middle school

#ms_submarket denotes the submarket classification

map1 +
  geom_point(data = data, aes(x = hlong, y = hlat, colour = as.factor(freq2)), size = 1, alpha=0.25) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Longitude", y="Latitude", colour="Submarket")

mtplot = map1 +
  geom_point(data = data, aes(x = hlong, y = hlat, colour = tranamt), size = 1, alpha=0.50) +
  scale_color_gradientn(colours = rainbow(10)) +
  labs(x = "Longitude", y="Latitude", colour="Price")


