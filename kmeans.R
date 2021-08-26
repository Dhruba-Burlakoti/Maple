### K-means cluster analysis example in forestry
### Spring 2021 FNR59800 Big Data in forest Research by Dr. Liang
### Code prepared by A. Abbasi (aota@purdue.edu)
### 03/11/2021

#######################################
# 1. Create data for cluster analysis #
#######################################

dat <- read.csv("TREEdata.csv")

# Subset data. Only the eastern US, records after 2000
dat_east <- subset(dat, LON > -100 & LON < -65 & YR >= 2010 & YR <= 2020)
rm(dat)

# Converting data to metric units
dat_east$DBH <- dat_east$DBH*2.54    # inch to cm
dat_east$TPH <- dat_east$TPA/0.405   # acre to hectare
dat_east$ELEV <- dat_east$ELEV*0.305 # ft to m

# Identifier for plot & year
dat_east$PLT_YR <- paste0(dat_east$PLT_CN,"_",dat_east$YR)

# Complete tree list
names(dat_east)
# dat_east <- dat_east[,c(4:7,9:10,12,14:15)]
dat_east <- dat_east[, c("PLT_CN", "YR", "STATUSCD", "DBH", "LAT", "LON", "GENUS", "SPECIES", "TPH", "PLT_YR")]

dat_east <- na.omit(dat_east)
dat_east <- subset(dat_east, dat_east$STATUSCD==1)   # select live trees only
dat_east$STATUSCD <- NULL

# Create plot-level data frame
B <- tapply(3.14*dat_east$DBH^2/40000*dat_east$TPH, dat_east$PLT_YR, sum)
Coords_y <- tapply(dat_east$LAT,dat_east$PLT_YR,mean)
Coords_x <- tapply(dat_east$LON,dat_east$PLT_YR,mean)
PLT <- tapply(dat_east$PLT_CN, dat_east$PLT_YR, mean)
YR <- tapply(dat_east$YR, dat_east$PLT_YR, mean)

PLT_df <- cbind.data.frame(PLT, YR, Coords_x, Coords_y, B) # Basal area
PLT_df$PLT_YR <- paste0(PLT_df$PLT,"_",PLT_df$YR)

rm(B, Coords_y, Coords_x, PLT, YR)

# Append genus columns
PLT_df[as.character(unique(dat_east$GENUS))] <- NA

progress_bar <- txtProgressBar(min = 7, max = ncol(PLT_df), style = 3)
for(i in 7:ncol(PLT_df)){
  PLT_df[,i] <- as.numeric(tapply(ifelse(dat_east$GENUS == colnames(PLT_df)[i],
                                         3.14*dat_east$DBH^2/40000*dat_east$TPH, 0),
                                  dat_east$PLT_YR, sum))
  setTxtProgressBar(progress_bar, i)
}

# Aggregate to hexagon

library(sp)
library(rgdal)
library(sf)
library(tmap)
library(raster)

# Create a spatial point dataframe
spdat <- PLT_df
coordinates(spdat) = ~Coords_x + Coords_y

# Define the map projection
proj4string(spdat) <- CRS("+init=epsg:4326")

# convert sp to sf
sfdat <- st_as_sf(spdat)

hex_shape <- st_read("C:/Users/dburlako/Class/R/Practice/Hexagon/NA_hexagon.shp")
crs(hex_shape)
crs(spdat)

# aggregate
sfdat2 <- st_join(sfdat, hex_shape,  join = st_intersects)
#summary(sfdat2)


# Aggregate plot data by grid and period
grid_df <- aggregate(st_drop_geometry(sfdat2[,c(1:3,5:274)]), list(sfdat2$GRID_ID), mean, na.rm=TRUE)

# Remove genera of small sample size
samp.size <- apply(grid_df[5:ncol(grid_df)], 2, function(x) length(which(x > 0)))
grid_df <- grid_df[,c(1:4,(which(samp.size >= 60)+4))]
apply(grid_df[5:ncol(grid_df)], 2, function(x) length(which(x > 0)))
grid_df$Tree <- NULL
colnames(grid_df)[1] <- "GRID_ID"

write.csv(grid_df, "KMeansCluster.csv")

grid_df <- read.csv("KMeansCluster.csv")
grid_df$X <- NULL

# Make spatial version of grid_df
grid <- merge(hex_shape, grid_df, by="GRID_ID", all.y = TRUE)

rm(dat, dat_east, hex_shape, PLT_df, progress_bar, sfdat, sfdat2, spdat, i, samp.size)

library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(ggplot2)
baseborderUS <- st_as_sf(ne_states(country="United States of America"))

ggplot()+
  geom_sf(data=baseborderUS, fill="gray90", col="gray68")+
  geom_sf(data=grid, fill = "red", col = NA)+
  xlim(-100, -65)+
  ylim(25, 50)+
  theme_bw()

ggplot()+
  geom_sf(data=baseborderUS, fill="gray90", col="gray68")+
  geom_sf(data=grid, col = "red")+
  xlim(-85, -82)+
  ylim(30, 33)+
  theme_bw()


###############################
# 2. K-means cluster analysis #
###############################

########## k = 3 #############

cl <- kmeans(grid_df[5:ncol(grid_df)], centers = 5, nstart = 50, iter.max = 100)
grid$cluster5 <- cl$cluster

library(RColorBrewer)
# Entire area
ggplot()+
  geom_sf(data=baseborderUS, fill="gray90", col="gray68")+
  geom_sf(data=grid, aes(fill = as.factor(cluster5)), col = NA)+
  scale_fill_brewer(palette = "Set1")+
  xlim(-100, -65)+
  ylim(25, 50)+
  theme_bw()

# Zoom in
ggplot()+
  geom_sf(data=baseborderUS, fill="gray90", col="gray68")+
  geom_sf(data=grid, aes(fill = as.factor(cluster5)), col = NA)+
  scale_fill_brewer(palette = "Set1")+
  xlim(-85, -82)+
  ylim(32, 34)+
  theme_bw()

# Species composition # dominant species 
center5 <- as.data.frame(cl$centers)
for(i in 1:5){
  print(sort(center5[i,], decreasing = TRUE)[1:5])
}


########### k = 5 ############

cl <- kmeans(grid_df[5:ncol(grid_df)], centers = 5, nstart = 50, iter.max = 100)
grid$cluster5 <- cl$cluster

# Entire area
ggplot()+
  geom_sf(data=baseborderUS, fill="gray90", col="gray68")+
  geom_sf(data=grid, aes(fill = as.factor(cluster5)), col = NA)+
  scale_fill_brewer(palette = "Set1")+
  xlim(-100, -65)+
  ylim(25, 50)+
  theme_bw()

# Zoom in
ggplot()+
  geom_sf(data=baseborderUS, fill="gray90", col="gray68")+
  geom_sf(data=grid, aes(fill = as.factor(cluster5)), col = NA)+
  scale_fill_brewer(palette = "Set1")+
  xlim(-94, -92)+
  ylim(36, 38)+
  theme_bw()

center5 <- as.data.frame(cl$centers)
for(i in 1:5){
  print(sort(center5[i,], decreasing = TRUE)[1:5])
}

###########################################################################
## Additional: Determine number of clusters:
library("factoextra")
cl$cluster <- NULL

fviz_nbclust(grid_df[5:ncol(grid_df)], kmeans,
             method = "wss")



############# Ok, but how do you determine k? ##########
# https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/


############# Are there any assumptions? ############
# https://www.r-bloggers.com/2017/08/exploring-assumptions-of-k-means-clustering-using-r/
