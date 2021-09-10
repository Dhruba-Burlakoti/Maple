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


# Classify data into periods:
dat$Prd <- NA
dat$Prd[between(dat$INVYR, 1990, 1999)] <- "P1"
dat$Prd[between(dat$INVYR, 2000, 2010)] <- "P2"
dat$Prd[dat$INVYR>=2010] <- "P3"

# Calculate basal area of each trees
dat <- mutate(dat,
              BA = (3.14/40000)*DBH^2 *TPH)


# Select only required columns:
dat1 <- select(dat, PLT_CN:SPCD, STATECD:BA)


####################################
# Remove data which does not belong within our designated period
dat1 <- dat1 %>% drop_na(Prd)

# Select Red Maple trees:
RM <- filter(dat1, SPCD == 316)
RM$SPCD <- NULL
RM <- filter(RM, INVYR != 9999) # Remove year 9999

################################################################
# Separate object for each period:
RM_P1 <- filter(RM, Prd == "P1")
RM_P1$Prd <- NULL
RM_P2 <- filter(RM, Prd == "P2")
RM_P2$Prd <- NULL
RM_P3 <- filter(RM, Prd == "P3")
RM_P3$Prd <- NULL

##############################################################
# Calculate Basal Area for each State for 3 periods
rem <- aggregate(RM_P1$BA, list(as.factor(RM_P1$STATECD)), sum, na.rm = TRUE)
# write.table(rem, file = "RM_BA_States", sep = ",")
# rm(rem)

# Now calculate for period 2 and 3 # copy to excel
aggregate(RM_P2$BA, list(as.factor(RM_P2$STATECD)), sum, na.rm = TRUE)
aggregate(RM_P3$BA, list(as.factor(RM_P3$STATECD)), sum, na.rm = TRUE)


###############################################################
# Calculate removal basal area for each state for 3 periods
RM_rem1 <- filter(RM_P1, STATUSCD == 3)
RM_rem1$STATUSCD <- NULL
RM_rem2 <- filter(RM_P2, STATUSCD == 3)
RM_rem2$STATUSCD <- NULL
RM_rem3 <- filter(RM_P3, STATUSCD == 3)
RM_rem3$STATUSCD <- NULL

# Calculate Removal basal area for each state for 3 periods
aggregate(RM_rem1$BA, list(as.factor(RM_rem1$STATECD)), sum, na.rm = TRUE)
aggregate(RM_rem2$BA, list(as.factor(RM_rem2$STATECD)), sum, na.rm = TRUE)
aggregate(RM_rem3$BA, list(as.factor(RM_rem3$STATECD)), sum, na.rm = TRUE)




## Calculate Sum of Basal Area for each inventory year ###
levels(as.factor(RM$INVYR))
tapply(RM$BA, as.factor(RM$INVYR), sum, na.rm = TRUE)
aggregate(RM$BA, list(as.factor(RM$INVYR)), sum, na.rm = TRUE)


# tapply(RM$BA, as.factor(RM$Prd), sum, na.rm = TRUE)
rm <- aggregate(RM$BA, list(as.factor(RM$Prd)), sum, na.rm = TRUE) # for each period
# write.table(rm, file = "filename", sep = ",")


################################################################################
# Friday First Trial #
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

# Identifier for plot and year
dat$PLT_YR <- paste0(dat$PLT_CN, "_", dat$INVYR)

names(dat)
# Calculate Key Plot level attributes:
var <- c("PLT_YR", "INVYR", "PLT_CN", "STATUSCD", "DBH", "TPH", "LAT", "LON", "ELEV", "SPCD", "STATECD", "Prd" )
dat1 <- dat[ , var]

# Start working on Plot level attributes:
N <- tapply(dat1$TPH, dat1$PLT_YR, sum)
summary(N)

# Calculate total number of Red Maple by plot and year:
N_ar <- tapply(ifelse(dat1$SPCD == 316, dat1$TPH, 0), dat1$PLT_YR, sum)
summary(N_ar)

# Percentage of Red Maple:
Perc_ar <- tapply(ifelse(dat1$SPCD == 316, dat1$TPH, 0), dat1$PLT_YR, sum)*(100/N)
summary(Perc_ar)

# Removal percentage for Red maple
N_rem_perc_ar <- tapply(ifelse(dat1$STATUSCD == 3 & dat1$SPCD == 316, dat1$TPH, 0), dat1$PLT_YR,sum)/N_ar *100
summary(N_rem_perc_ar)

########################################
# Calculate total stand basal area:

B <- tapply(3.14*dat1$DBH^2/40000*dat1$TPH, dat1$PLT_YR, sum)
summary(B)

# Calculate total stand basal area for Red Maple
B_ar <- tapply(ifelse(dat1$SPCD == 316, 3.14*dat1$DBH^2/40000*dat1$TPH, 0), dat1$PLT_YR, sum)
summary(B_ar)


# Stand basal area removal percentage for Red Maple
B_rem_perc_ar <- tapply(ifelse(dat1$STATUSCD == 3 & dat1$SPCD == 316, 3.14*dat1$DBH^2/40000*dat1$TPH, 0), dat1$PLT_YR, sum)/B_ar *100
summary(B_rem_perc_ar)

# Plot coordinates
Coords_y <- tapply(dat1$LAT,dat1$PLT_YR,mean)
Coords_x <- tapply(dat1$LON,dat1$PLT_YR,mean)

# Plot
PLT <- tapply(dat1$PLT_CN, dat1$PLT_YR, mean)

# Year
YR <- tapply(dat1$INVYR, dat1$PLT_YR, mean)


# STATE <- tapply(dat1$STATECD, dat1$PLT_YR, mean, na.rm = TRUE)


# Merge plot-level data
PLT_df <- cbind.data.frame(PLT, YR, Coords_x, Coords_y, N, B, N_ar, B_ar, B_rem_perc_ar,
                           N_rem_perc_ar, STATE)


PLT_df$PLT_YR <- paste0(PLT_df$PLT,"_",PLT_df$YR)

# Remove plots where Red Maple is absent
PLT_df1 <- PLT_df %>% drop_na(B_ar)
PLT_df2 <- filter(PLT_df1, B_ar !=0)
PLT_df2 <- PLT_df2 %>% drop_na(Prd)

write.csv(PLT_df2, "Test.csv", row.names = FALSE)


dat$Prd <- NA
dat$Prd[between(dat$INVYR, 1990, 1999)] <- "P1"
dat$Prd[between(dat$INVYR, 2000, 2010)] <- "P2"
dat$Prd[dat$INVYR>=2010] <- "P3"


PLT_df$Prd <- NA
PLT_df$Prd[between(PLT_df$YR, 1990, 1999)] <- "P1"
PLT_df$Prd[between(PLT_df$YR, 2000, 2010)] <- "P2"
PLT_df$Prd[PLT_df$YR > 2010] <- "P3"


PLT_df$STATE <- NULL

write.csv(PLT_df, "Maple_Plots.csv", row.names = FALSE)

################################################################################



