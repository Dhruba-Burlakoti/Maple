setwd("C:/Users/dburl\\Mapl")
dat <- read.csv("New.csv")
library(tidyverse)
plot <- read.csv("PLOT.csv")
maplestates <- filter(plot, STATECD %in% c(27, 55,19,29,5,22,17,21,47,28,1,18, 26,39,13,12,37,45,51,54,36,42,34,10,15,9,25,44,33,50,23,11))
maplestates <- select(maplestates, CN, STATECD) # 32 states

dat <- dat %>% left_join(maplestates, by = c("PLT_CN" = "CN"))
dat$STATECD <- as.factor(dat$STATECD)

head(dat)
rm(maplestates, plot)

# Convert data to metric units and calculate Basal Area for each trees
dat$DBH <- dat$DIA * 2.54 # DBH inch to cm
dat$TPH <- dat$TPA_UNADJ/0.405 # acre to hectare

# Classify data into periods:
dat$Prd <- NA
dat$Prd[between(dat$INVYR, 1990, 1999)] <- "P1"
dat$Prd[between(dat$INVYR, 2000, 2010)] <- "P2"
dat$Prd[dat$INVYR>=2010] <- "P3"

# Calculate basal area of trees
dat <- mutate(dat,
              BA = 0.0054514*DIA^2 *TPA_UNADJ)


summary(dat$BA)
# Select only required columns:
dat1 <- select(dat, PLT_CN:SPCD, STATECD:BA)

# Remove data which does not belong within our designated period
dat1 <- dat1 %>% drop_na(Prd)

# # Select Red Maple trees:
# RM <- filter(dat1, SPCD == 316)
# RM$SPCD <- NULL
# RM <- filter(RM, INVYR != 9999) # Remove year 9999
# 
# # Separate object for each period:
# RM_P1 <- filter(RM, Prd == "P1")
# RM_P1$Prd <- NULL
# RM_P2 <- filter(RM, Prd == "P2")
# RM_P2$Prd <- NULL
# RM_P3 <- filter(RM, Prd == "P3")
# RM_P3$Prd <- NULL
# 
# # Calculate Basal Area for each State for 3 periods
# aggregate(RM_P1$BA, list(as.factor(RM_P1$STATECD)), sum, na.rm = TRUE)
# aggregate(RM_P2$BA, list(as.factor(RM_P2$STATECD)), sum, na.rm = TRUE)
# aggregate(RM_P3$BA, list(as.factor(RM_P3$STATECD)), sum, na.rm = TRUE)
# 
# # Calculate Mean Basal Area of Red Maple for each period:
# aggregate(RM_P1$BA, list(as.factor(RM_P1$STATECD)), mean, na.rm = TRUE)
# aggregate(RM_P2$BA, list(as.factor(RM_P2$STATECD)), mean, na.rm = TRUE)
# aggregate(RM_P3$BA, list(as.factor(RM_P3$STATECD)), mean, na.rm = TRUE)

###########################################################################
# Calculate Basal area of whole forest for each period:
# Separate object for each period:
P1 <- filter(dat1, Prd == "P1")
P1$Prd <- NULL
P1 <- P1 %>% drop_na(STATECD)

P2 <- filter(dat1, Prd == "P2")
P2$Prd <- NULL
P3 <- filter(dat1, Prd == "P3")
P3$Prd <- NULL

aggregate(P1$BA, list(as.factor(P1$STATECD)), mean, na.rm = TRUE)


######################################################################
# With Plot Level attributes:
plot <- read.csv("PLOT.csv")
plot <- rename(plot, PLT = CN)
test <- read.csv("test.csv")

# Join test and plot (For STATECD)

t1 <- plot[, c("PLT", "LAT", "LON", "STATECD")]
t2 <- left_join(test, t1, by = "PLT")


# Find basal area for each states:
aggregate(t2$B, list(as.factor(t2$STATECD)), sum, na.rm = TRUE)
aggregate(t2$B, list(as.factor(t2$STATECD)), mean, na.rm = TRUE)

aggregate(t2$B_ar, list(as.factor(t2$STATECD)), mean, na.rm = TRUE)
aggregate(t2$B_ar, list(as.factor(t2$STATECD)), mean, na.rm = TRUE)
aggregate(t2$B_ar, list(as.factor(t2$Prd)), mean, na.rm = TRUE)
aggregate(t2$B_rem_perc_ar, list(t2$STATECD), mean, na.rm = TRUE)

#####################################
################################################################
# Separate object for each period:
P1 <- filter(t2, Prd == "P1")
P1$Prd <- NULL
P2 <- filter(t2, Prd == "P2")
P2$Prd <- NULL
P3 <- filter(t2, Prd == "P3")
P3$Prd <- NULL 

##############################################################
# Calculate Basal Area for each State for 3 periods
aggregate(P1$B, list(as.factor(P1$STATECD)), mean, na.rm = TRUE)
aggregate(P2$B, list(as.factor(P2$STATECD)), mean, na.rm = TRUE)
aggregate(P3$B, list(as.factor(P3$STATECD)), mean, na.rm = TRUE)

# Red maple Basal Area
aggregate(P1$B_ar, list(as.factor(P1$STATECD)), mean, na.rm = TRUE)
aggregate(P2$B_ar, list(as.factor(P2$STATECD)), mean, na.rm = TRUE)
aggregate(P3$B_ar, list(as.factor(P3$STATECD)), mean, na.rm = TRUE)

# Red maple Removal Basal Area:
aggregate(P1$B_rem_perc_ar, list(as.factor(P1$STATECD)), mean, na.rm = TRUE)
aggregate(P2$B_rem_perc_ar, list(as.factor(P2$STATECD)), mean, na.rm = TRUE)
aggregate(P3$B_rem_perc_ar, list(as.factor(P3$STATECD)), mean, na.rm = TRUE)

