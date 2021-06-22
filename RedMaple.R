setwd("C:/Users/dburl\\Mapl")
dat <- read.csv("New.csv")

str(dat)
head(dat)
# # Add Species name:
# dat$SciNam <- paste0(dat$GENUS, "_", dat$SPECIES)
# summary(as.factor(dat$SciNam))
# dat <- subset(dat, select = -c("SciNam"))

# Give Scientific Names to Maple species
dat$SciNam <- NA

dat$SciNam[dat$SPCD == 318] <- "Acer_saccharum"
dat$SciNam[dat$SPCD == 316] <- "Acer_rubrum"
dat$SciNam <- as.factor(dat$SciNam)
summary(as.factor(dat$SPCD))
summary(dat$SciNam)


# Converting data to Metric units:
dat$DBH <- dat$DIA * 2.54 # DBH inch to cm
dat$TPH <- dat$TPA_UNADJ/0.405 # acre to hectare
dat$ELEV <- dat$ELEV*0.305 # ft to m
dat$TPHMORT <- dat$TPAMORT_UNADJ/0.405
dat$TPHREMV <- dat$TPAREMV_UNADJ/0.405

dat$SPCD <- as.factor(dat$SPCD)
dat$STATUSCD <- as.factor(dat$STATUSCD) # choose statuscd 2(dead), 3 (removed)
dat$AGENTCD <- as.factor(dat$AGENTCD)
dat$DSTRBCD1 <- as.factor(dat$DSTRBCD1) # observed disturbance within past 5 yrs
dat$TRTCD1 <- as.factor(dat$TRTCD1) # type of stand treatment within past 5 yrs

head(dat)
summary(dat$AGENTCD)

# Combine agentcodes and name it as Agents
dat$AGENTCD_BINS <- NA

dat$AGENTCD_BINS[dat$AGENTCD %in% c(10, 11, 12, 13, 15, 17, 19)] <- "Insect"
dat$AGENTCD_BINS[dat$AGENTCD %in% c(20,21,22,23,24,25,26,27,28,29)] <- "Disease"
dat$AGENTCD_BINS[dat$AGENTCD %in% c(30, 31)] <- "Fire"
dat$AGENTCD_BINS[dat$AGENTCD %in% c(40, 41,42,43,44,45)] <- "Animal"
dat$AGENTCD_BINS[dat$AGENTCD %in% c(50,51,52,53,54,55,56)] <- "Weather"
dat$AGENTCD_BINS[dat$AGENTCD %in% c(60, 65)] <- "VegSupp"
dat$AGENTCD_BINS[dat$AGENTCD %in% c(70, 71,72,73,76,77)] <- "Unknown"
dat$AGENTCD_BINS[dat$AGENTCD %in% c(80,81,82,83,84,85,86)] <- "Silvi"

library(dplyr)
dat <- rename(dat, Agents = AGENTCD_BINS)
summary(as.factor(dat$Agents))


##### Make new column with Treatments #####
dat$TRT_BINS <- NA

dat$TRT_BINS[dat$TRTCD1 == 00] <- "No treat"
dat$TRT_BINS[dat$TRTCD1 == 10] <- "Cutting"
dat$TRT_BINS[dat$TRTCD1 == 20] <- "Site prep"
dat$TRT_BINS[dat$TRTCD1 == 30] <- "Artf reg"
dat$TRT_BINS[dat$TRTCD1 == 40] <- "Nat reg"
dat$TRT_BINS[dat$TRTCD1 == 50] <- "Other"

library(dplyr)
dat <- rename(dat, Treatments = TRT_BINS)
dat$Treatments <- as.factor(dat$Treatments)
summary(dat$Treatments)
counts <- table(dat$Treatments)
barplot(counts)
rm(counts)

# Identifier for plot and year
dat$PLT_YR <- paste0(dat$PLT_CN, "_", dat$INVYR)

# Calculate Key plot-level attributes:
names(dat)

var <- c("PLT_YR", "INVYR", "PLT_CN", "STATUSCD", "DBH", "TPH", "TPHREMV", "TPHMORT",
         "Agents", "Treatments", "KINDCD", "LAT", "LON", "ELEV", "VOLCFNET", "SPCD" )

dat1 <- dat[ , var]
# datl <- subset(dat1, dat1$STATUSCD == 1) # select live trees
# datd <- subset(dat1, dat1$STATUSCD == 2) # select dead trees
# datr <- subset(dat1, dat1$STATUSCD == 3) # select removal trees

##################################################################################
###### Start working on Plot level attributes #########
# Calculate total number of trees by plot & year
N <-  tapply(dat1$TPH, dat1$PLT_YR, sum) # this includes every tree species from TREE.csv dataset
summary(N)

# Calculate total number of Red Maple trees by plot & year
N_ar <-  tapply(ifelse(dat1$SPCD == 316, dat1$TPH, 0), dat1$PLT_YR, sum)
summary(N_ar)

Perc_ar <- tapply(ifelse(dat1$SPCD == 316, dat1$TPH, 0), dat1$PLT_YR, sum)*(100/N)
summary(Perc_ar)


# Calculate Removal trees
# N_rem <- tapply(datr$TPHREMV, datr$PLT_YR, sum)
# summary(N_rem)

# Percentage of removal trees:
N_rem_perc <- tapply(ifelse(dat1$STATUSCD == 3, dat1$TPH, 0), dat1$PLT_YR,sum)/N *100
summary(N_rem_perc)

# Removal percentage for Red maple
N_rem_perc_ar <- tapply(ifelse(dat1$STATUSCD == 3 & dat1$SPCD == 316, dat1$TPH, 0), dat1$PLT_YR,sum)/N_ar *100
summary(N_rem_perc_ar)


# Percentage of Mortality trees:
N_mort_perc <- tapply(ifelse(dat1$STATUSCD == 2, dat1$TPH, 0), dat1$PLT_YR,sum)/N *100
summary(N_mort_perc)

# Mortality percentage for Red maple
N_mort_perc_ar <- tapply(ifelse(dat1$STATUSCD == 2 & dat1$SPCD == 316, dat1$TPH, 0), dat1$PLT_YR,sum)/N_ar *100
summary(N_mort_perc_ar)


###################################################

# Calculate total stand basal area
B <- tapply(3.14*dat1$DBH^2/40000*dat1$TPH, dat1$PLT_YR, sum)
summary(B)

# Calculate total stand basal area for Red Maple
B_ar <- tapply(ifelse(dat1$SPCD == 316, 3.14*dat1$DBH^2/40000*dat1$TPH, 0), dat1$PLT_YR, sum)
summary(B_ar)


######################################################
# Stand basal area removal percentage for Red Maple
B_rem_perc_ar <- tapply(ifelse(dat1$STATUSCD == 3 & dat1$SPCD == 316, 3.14*dat1$DBH^2/40000*dat1$TPH, 0), dat1$PLT_YR, sum)/B_ar *100
summary(B_rem_perc_ar)


# Percentage Stand basal area loss d/t mortality Red maple
B_mort_perc_ar <- tapply(ifelse(dat1$STATUSCD == 2 & dat1$SPCD == 316, 3.14*dat1$DBH^2/40000*dat1$TPH, 0), dat1$PLT_YR, sum)/B_ar * 100
summary(B_mort_perc_ar)

# Plot coordinates
Coords_y <- tapply(dat1$LAT,dat1$PLT_YR,mean)
Coords_x <- tapply(dat1$LON,dat1$PLT_YR,mean)

# Plot
PLT <- tapply(dat1$PLT_CN, dat1$PLT_YR, mean)

###########################################
# dat1$Agents <- as.factor(dat1$Agents)
# 
# # Agent <- tapply(ifelse(dat1$SPCD == 316, dat1$Treatments, 0),.)
# 
# 
# 
# Agent <- tapply(ifelse(dat1$SPCD == 316, dat1$Agents, NA), dat1$PLT_YR, function(x){
#   ifelse(max(table(x)) == 0, NA, levels(dat1$Agents)[which.max(table(x))])
# })
# 
# Agent <- as.factor(Agent)
# summary(Agent)
# 
# dat1$Treatments <- as.factor(dat1$Treatments)
# 
# Treatment <- tapply(ifelse(dat1$SPCD == 316, dat1$Treatments, NA), dat1$PLT_YR, function(x){
#   ifelse(max(table(x)) == 0, NA, levels(dat1$Treatments)[which.max(table(x))])
# })
# 
# Treatment <- as.factor(Treatment)
# summary(Treatment)

###########################

dat1$Agents[which(dat1$SPCD != 316)] <- NA
Animal <- tapply(ifelse(dat1$Agents == "Animal", dat1$TPH, 0), dat1$PLT_YR, sum, na.rm = TRUE)
Disease <- tapply(ifelse(dat1$Agents == "Disease", dat1$TPH, 0), dat1$PLT_YR, sum, na.rm = TRUE)
Fire <- tapply(ifelse(dat1$Agents == "Fire", dat1$TPH, 0), dat1$PLT_YR, sum, na.rm = TRUE)
Insect <- tapply(ifelse(dat1$Agents == "Insect", dat1$TPH, 0), dat1$PLT_YR, sum, na.rm = TRUE)
Silvi <- tapply(ifelse(dat1$Agents == "Silvi", dat1$TPH, 0), dat1$PLT_YR, sum, na.rm = TRUE)
VegSupp <- tapply(ifelse(dat1$Agents == "VegSupp", dat1$TPH, 0), dat1$PLT_YR, sum, na.rm = TRUE)
Weather <- tapply(ifelse(dat1$Agents == "Weather", dat1$TPH, 0), dat1$PLT_YR, sum, na.rm = TRUE)


#################################
dat1$Treatments[which(dat1$SPCD != 316)] <- NA

Artf_reg <- tapply(ifelse(dat1$Treatments == "Artf reg", dat1$TPH, 0), dat1$PLT_YR, sum, na.rm = TRUE)
Cutting <- tapply(ifelse(dat1$Treatments == "Cutting", dat1$TPH, 0), dat1$PLT_YR, sum, na.rm = TRUE)
Nat_reg <- tapply(ifelse(dat1$Treatments == "Nat reg", dat1$TPH, 0), dat1$PLT_YR, sum, na.rm = TRUE)
Other <- tapply(ifelse(dat1$Treatments == "Other", dat1$TPH, 0), dat1$PLT_YR, sum, na.rm = TRUE)
Site_prep <- tapply(ifelse(dat1$Treatments == "Site prep", dat1$TPH, 0), dat1$PLT_YR, sum, na.rm = TRUE)
No_treat <- tapply(ifelse(dat1$Treatments == "No treat", dat1$TPH, 0), dat1$PLT_YR, sum, na.rm = TRUE)

# Year
YR <- tapply(dat1$INVYR, dat1$PLT_YR, mean)

# Merge plot-level data
PLT_df <- cbind.data.frame(PLT, YR, Coords_x, Coords_y, N, B, N_ar, B_ar, B_mort_perc_ar, B_rem_perc_ar,
                           N_rem_perc_ar, N_rem_perc, N_mort_perc_ar, Animal, Disease, Fire, Insect, Silvi, VegSupp, Weather,
                           Artf_reg, Cutting, Nat_reg, Other, Site_prep, No_treat)
PLT_df$PLT_YR <- paste0(PLT_df$PLT,"_",PLT_df$YR)

PLT_df <- PLT_df[-which(is.na(PLT_df$N_ar)),] # remove NA's from dataset (Red maple is absent)
PLT_df <- PLT_df[which(PLT_df$N_ar > 0),] # remove 0

PLT_df$Prd <- NA
PLT_df$Prd[PLT_df$YR < 2000] <- "P1"
PLT_df$Prd[between(PLT_df$YR, 2000, 2010)] <- "P2"
PLT_df$Prd[PLT_df$YR > 2010] <- "P3"

PLT_df$Prd <- as.factor(PLT_df$Prd)


# Save plot-level data
write.csv(PLT_df,"FIA_PLT.csv",row.names = FALSE)


# # Make Separate csv files for Red Maple
redMap <- dat1 %>% filter(SPCD == 316)
# write.csv(redMap, "RedMaple.csv", row.names = FALSE)

####################################################
# redMap <- read.csv("RedMaple.csv")

###################### Mortality/dead Red Maple ##########
RM_dead <- subset(redMap, STATUSCD == 2)

# Agents summary for all dead Red maple trees:
RM_dead$Agents <- as.factor(RM_dead$Agents)

RM_dead %>%
  count(Agents)

# Add time period in the dataset
RM_dead$Prd <- NA
RM_dead$Prd[RM_dead$INVYR < 2000] <- "P1"
RM_dead$Prd[between(RM_dead$INVYR, 2000, 2010)] <- "P2"
RM_dead$Prd[RM_dead$INVYR > 2010] <- "P3"


# Calculate summary count for each agents for three period
sum_dead <- RM_dead %>%
  group_by(Prd) %>%
  count(Agents)

print(sum_dead, n = nrow(sum_dead))

# Pivot sum_dead to tidydata:
View(sum_dead)
library(tidyr)
DeadRM <- sum_dead %>%
  pivot_wider(names_from = Prd, values_from = n)

DeadRM

#################### Cut/Harvest Red Maple ###############
RM_cut <- subset(redMap, STATUSCD == 3)

# Agents summary for all cut/removed Red maple trees:
summary(as.factor(RM_cut$Agents))


# Treatment summary for Red maple trees:
RM_cut %>%
  count(Treatments)

# Add time period in the dataset
RM_cut$Prd <- NA
RM_cut$Prd[RM_cut$INVYR < 2000] <- "P1"
RM_cut$Prd[between(RM_cut$INVYR, 2000, 2010)] <- "P2"
RM_cut$Prd[RM_cut$INVYR > 2010] <- "P3"

# Calculate summary count for each agents for three period
sum_cut <- RM_cut %>%
  group_by(Prd) %>%
  count(Treatments)

print(sum_cut, n = nrow(sum_cut))

# Pivot sum_cut to tidydata with each period:
library(tidyr)
CutRM <- sum_cut %>%
  pivot_wider(names_from = Prd, values_from = n)

CutRM



#############################################################################
#################### Mapping ###############################
PLT_df <- read.csv("RedMaple_Plt.csv")

library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
# If you are having trouble installing rnaturalearthhires, try this:
# devtools::install_github("ropensci/rnaturalearthhires")
# you need to install devtools package.

library(ggplot2)
library(sp)
library(rgdal)
library(rgeos)
library(sf)
library(RColorBrewer)
library(gridExtra)

dat_sp <- SpatialPointsDataFrame(coords = PLT_df[,c(3,4)],data = PLT_df,
                                 proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
dat_sf <- st_as_sf(dat_sp)
rm(dat_sp)

baseborderUS <- st_as_sf(ne_states(country="United States of America"))

############# Animal #############
hist(dat_sf$Animal)
dat_sf$Animal_fact <- ifelse(dat_sf$Animal > 10,7,
                             ifelse(dat_sf$Animal > 5, 6,
                                    ifelse(dat_sf$Animal > 4, 5,
                                           ifelse(dat_sf$Animal > 3, 4,
                                                  ifelse(dat_sf$Animal > 2, 3,
                                                         ifelse(dat_sf$Animal > 1, 2, 1))))))
dat_sf$Animal_fact <- as.factor(dat_sf$Animal_fact)

summary(dat_sf$Animal_fact)

dat_sf_p1 <- dat_sf[which(dat_sf$Prd == "P1"),]
dat_sf_p2 <- dat_sf[which(dat_sf$Prd == "P2"),]
dat_sf_p3 <- dat_sf[which(dat_sf$Prd == "P3"),]



P1 <- ggplot()+
  geom_sf(data = baseborderUS, fill = "white", col = "black")+
  geom_sf(data = dat_sf_p1, aes(color = Animal_fact), size = 0.03)+
  scale_color_brewer("Number of red maple trees (/ha)\n affected by animal(P1)", palette = "Spectral",
                     labels = c("0-1","1-5","5-10","10-20",
                                "20-30","30-40",">40"))+
  xlim(min(dat_sf$Coords_x), max(dat_sf$Coords_x))+
  ylim(min(dat_sf$Coords_y), max(dat_sf$Coords_y))+
  theme_bw()


P2 <- ggplot()+
  geom_sf(data = baseborderUS, fill = "white", col = "black")+
  geom_sf(data = dat_sf_p2, aes(color = Animal_fact), size = 0.03)+
  scale_color_brewer("Number of red maple trees (/ha)\n affected by animal(P2)", palette = "Spectral",
                     labels = c("0-1","1-5","5-10","10-20",
                                "20-30","30-40",">40"))+
  xlim(min(dat_sf$Coords_x), max(dat_sf$Coords_x))+
  ylim(min(dat_sf$Coords_y), max(dat_sf$Coords_y))+
  theme_bw()

P3 <- ggplot()+
  geom_sf(data = baseborderUS, fill = "white", col = "black")+
  geom_sf(data = dat_sf_p3, aes(color = Animal_fact), size = 0.03)+
  scale_color_brewer("Number of red maple trees (/ha)\n affected by animal(P3)", palette = "Spectral",
                     labels = c("0-1","1-5","5-10","10-20",
                                "20-30","30-40",">40"))+
  xlim(min(dat_sf$Coords_x), max(dat_sf$Coords_x))+
  ylim(min(dat_sf$Coords_y), max(dat_sf$Coords_y))+
  theme_bw()


library("gridExtra")
grid.arrange(P1, P2, P3, 
             ncol = 3, nrow = 1)

#####################################################################################
# Plot Basal Area of Maple (P1, P2, P3)
BA <- ggplot()+
  geom_sf(data = baseborderUS, fill = "white", col = "black")+
  geom_sf(data = dat_sf, aes(color = B_ar), size = 0.03)+
  scale_fill_distiller(palette = "Spectral") +
  xlim(min(dat_sf$Coords_x), max(dat_sf$Coords_x))+
  ylim(min(dat_sf$Coords_y), max(dat_sf$Coords_y))+
  theme_bw() +
  ggtitle("Red Maple Basal Area")



mortBA <- ggplot()+
  geom_sf(data = baseborderUS, fill = "white", col = "black")+
  geom_sf(data = dat_sf, aes(color = B_mort_perc_ar), size = 0.03)+
  scale_fill_distiller(palette = "Set1") +
  xlim(min(dat_sf$Coords_x), max(dat_sf$Coords_x))+
  ylim(min(dat_sf$Coords_y), max(dat_sf$Coords_y))+
  theme_bw() +
  ggtitle("Percentage of Dead Red Maple trees")


grid.arrange(BA, mortBA,
             ncol = 2)

ggplot()+
  geom_sf(data = baseborderUS, fill = "white", col = "black")+
  geom_sf(data = dat_sf, aes(color = B_ar), size = 0.03)+
  scale_fill_distiller(palette = "Spectral") +
  xlim(min(dat_sf$Coords_x), max(dat_sf$Coords_x))+
  ylim(min(dat_sf$Coords_y), max(dat_sf$Coords_y))+
  theme_bw()

###########################################
############# Insects #############
hist(dat_sf$Insect)
Ins <- cut(dat_sf$Insect, br = c(0, 10, 50, 100, 2040 ))
dat_sf$INsect_fac <- Ins


baseborderUS <- st_as_sf(ne_states(country="United States of America"))
ggplot()+
  geom_sf(data = baseborderUS, fill = "white", col = "black")+
  geom_sf(data = dat_sf, aes(color = INsect_fac), size = 1)+
  scale_color_brewer("Number of red maple trees\n  (/ha) affected by insects", palette = "Spectral",
                     labels = c("0-10","10-50", "50-100", ">100"))+
  xlim(min(dat_sf$Coords_x), max(dat_sf$Coords_x))+
  ylim(min(dat_sf$Coords_y), max(dat_sf$Coords_y))+
  xlab("Longitude") + ylab("Latitude") +
  theme_bw() +
  ggtitle("Red Maple Trees affected by Insect")

################ Disease ####################################################
hist(dat_sf$Disease)
Dis <- cut(dat_sf$Disease, br = c(0, 50, 100, 200, 300, 5000))
is.factor(Dis)
summary(Dis)

dat_sf$Disease_fac <- Dis
ggplot()+
  geom_sf(data = baseborderUS, fill = "white", col = "black")+
  geom_sf(data = dat_sf, aes(color = Disease_fac), size = 1)+
  scale_color_brewer("Number of red maple trees\n  (/ha) affected by disease", palette = "Spectral",
                     labels = c("0-50","50-100", "100-200", "200-300", ">300"))+
  xlim(min(dat_sf$Coords_x), max(dat_sf$Coords_x))+
  ylim(min(dat_sf$Coords_y), max(dat_sf$Coords_y))+
  xlab("Longitude") + ylab("Latitude") +
  theme_bw() +
  ggtitle("Red Maple Trees affected by Disease")





################ Fire ##########################################
hist(dat_sf$Fire)
summary(dat_sf$Fire)
Fire <- cut(dat_sf$Fire, br = c(0, 25, 100, 200, 3800))
summary(Fire)

dat_sf$Fire_fac <- Fire
ggplot() +
  geom_sf(data = baseborderUS, fill = "white", col = "black") +
  geom_sf(data = dat_sf, aes(color = Fire_fac), size = 1) +
  scale_color_brewer("Number of red maple trees\n(/ha) affected by fire", 
                     palette = "Spectral",
                     labels = c("0-25", "25-100", "100-200", ">200")) +
  xlim(min(dat_sf$Coords_x), max(dat_sf$Coords_x)) +
  ylim(min(dat_sf$Coords_y), max(dat_sf$Coords_y)) +
  xlab("Longitude") + ylab("Latitude") +
  theme_bw() +
  ggtitle("Red Maple Trees affected by Fire")


################ Animal ################################

hist(dat_sf$Animal)
summary(dat_sf$Animal)
Anml <- cut(dat_sf$Animal, br = c(0, 25, 100, 200, 2300))
summary(Anml)

dat_sf$Anml_fac <- Anml
ggplot() +
  geom_sf(data = baseborderUS, fill = "white", col = "black") +
  geom_sf(data = dat_sf, aes(color = Anml_fac), size = 0.3) +
  scale_color_brewer("Number of red maple trees\n(/ha) affected by animal", 
                     palette = "Spectral",
                     labels = c("0-25", "25-100", "100-200", ">200")) +
  xlim(min(dat_sf$Coords_x), max(dat_sf$Coords_x)) +
  ylim(min(dat_sf$Coords_y), max(dat_sf$Coords_y)) +
  xlab("Longitude") + ylab("Latitude") +
  theme_bw() +
  ggtitle("Red Maple Trees affected by Animal")


############### Weather ###############################

hist(dat_sf$Weather)
summary(dat_sf$Weather)
Weather <- cut(dat_sf$Weather, br = c(0, 25, 100, 200, 3800))
summary(Weather)

dat_sf$Weather_fac <- Weather
ggplot() +
  geom_sf(data = baseborderUS, fill = "white", col = "black") +
  geom_sf(data = dat_sf, aes(color = Weather_fac), size = 1) +
  scale_color_brewer("Number of red maple trees\n(/ha) affected by weather", 
                     palette = "Spectral",
                     labels = c("0-25", "25-100", "100-200", ">200")) +
  xlim(min(dat_sf$Coords_x), max(dat_sf$Coords_x)) +
  ylim(min(dat_sf$Coords_y), max(dat_sf$Coords_y)) +
  xlab("Longitude") + ylab("Latitude") +
  theme_bw() +
  ggtitle("Red Maple Trees affected by Weather Events")



############# Vegetation Suppression #####################

hist(dat_sf$VegSupp)
summary(dat_sf$VegSupp)
VegSuppr <- cut(dat_sf$VegSupp, br = c(0, 25, 100, 200, 3400))
summary(VegSuppr)

dat_sf$VegSupp_fac <- VegSuppr
ggplot() +
  geom_sf(data = baseborderUS, fill = "white", col = "black") +
  geom_sf(data = dat_sf, aes(color = VegSupp_fac), size = 1) +
  scale_color_brewer("Number of red maple\n trees(/ha) affected by \nvegetation suppression", 
                     palette = "Spectral",
                     labels = c("0-25", "25-100", "100-200", ">200")) +
  xlim(min(dat_sf$Coords_x), max(dat_sf$Coords_x)) +
  ylim(min(dat_sf$Coords_y), max(dat_sf$Coords_y)) +
  theme_bw() +
  ggtitle("Red Maple Trees affected by Vegetation Suppression")



############# Silvicultural Clearing #####################
hist(dat_sf$Silvi)
summary(dat_sf$Silvi)

Silvi <- cut(dat_sf$Silvi, br = c(0, 25, 100, 200, 8000))
summary(Silvi)

dat_sf$Silvi_fac <- Silvi
ggplot() +
  geom_sf(data = baseborderUS, fill = "white", col = "black") +
  geom_sf(data = dat_sf, aes(color = VegSupp_fac), size = 1) +
  scale_color_brewer("Number of red maple\n trees(/ha) affected by \nsilvicultural treatment", 
                     palette = "Spectral",
                     labels = c("0-25", "25-100", "100-200", ">200")) +
  xlim(min(dat_sf$Coords_x), max(dat_sf$Coords_x)) +
  ylim(min(dat_sf$Coords_y), max(dat_sf$Coords_y)) +
  theme_bw() +
  ggtitle("Red Maple Trees affected by Silvicultural Clearing")

#####################################################################################
#####################################################################################
################## MANAGEMENT ACTIVITIES #################

hist(dat_sf$Cutting)
summary(dat_sf$Cutting)

Cutting <- cut(dat_sf$Cutting, br = c(0, 20, 100, 200, 8000))
summary(Cutting)

dat_sf$Cutting_fac <- Cutting
ggplot() +
  geom_sf(data = baseborderUS, fill = "white", col = "black") +
  geom_sf(data = dat_sf, aes(color = Cutting_fac), size = 1) +
  scale_color_brewer("Number of red\n maple trees(/ha) cut", 
                     palette = "Spectral",
                     labels = c("0-20", "20-100", "100-200", ">200")) +
  xlim(min(dat_sf$Coords_x), max(dat_sf$Coords_x)) +
  ylim(min(dat_sf$Coords_y), max(dat_sf$Coords_y)) +
  theme_bw() +
  ggtitle("Red Maple Trees Cut")



########## Basal Area of Red Maple ##############

BA_RM <- ggplot()+
  geom_sf(data = baseborderUS, fill = "white", col = "black")+
  geom_sf(data = dat_sf, aes(color = B_ar), size = 1)+
  scale_color_distiller(palette = "Spectral") +
  xlim(min(dat_sf$Coords_x), max(dat_sf$Coords_x))+
  ylim(min(dat_sf$Coords_y), max(dat_sf$Coords_y))+
  theme_bw() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Red Maple Basal Area")


BA_RM_Mort <- ggplot()+
  geom_sf(data = baseborderUS, fill = "white", col = "black")+
  geom_sf(data = dat_sf, aes(color = B_mort_perc_ar), size = 1)+
  scale_color_distiller(palette = "Spectral") +
  xlim(min(dat_sf$Coords_x), max(dat_sf$Coords_x))+
  ylim(min(dat_sf$Coords_y), max(dat_sf$Coords_y))+
  xlab("Longitude") + ylab("Latitude") +
  theme_bw() +
  ggtitle("Mortality Percentage of Red Maple")

BA_RM_Rem <- ggplot()+
  geom_sf(data = baseborderUS, fill = "white", col = "black")+
  geom_sf(data = dat_sf, aes(color = B_rem_perc_ar), size = 1)+
  scale_color_distiller(palette = "Spectral") +
  xlim(min(dat_sf$Coords_x), max(dat_sf$Coords_x))+
  ylim(min(dat_sf$Coords_y), max(dat_sf$Coords_y))+
  xlab("Longitude") + ylab("Latitude") +
  theme_bw() +
  ggtitle("Removal (Cut) Percentage of Red Maple")


library(patchwork)
# BA_RM+ BA_RM_Mort+ BA_RM_Rem
BA_RM | (BA_RM_Mort/ BA_RM_Rem)



library("gridExtra")
grid.arrange(BA_RM, BA_RM_Mort, BA_RM_Rem, ncol = 3)



