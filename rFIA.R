library(tidyverse)
library(rFIA)
mi_rfia <- getFIA(states = 'MI')
tpa_mi <- tpa(mi_rfia, bySpecies = TRUE)
str(tpa_mi)

redmap <- filter(tpa_mi, SCIENTIFIC_NAME == 'Acer rubrum')
sugmap <- filter(tpa_mi, SCIENTIFIC_NAME == 'Acer saccharum')

write.csv(sugmap, "MI_SM_ba_rfia", row.names = FALSE)
write.csv(redmap, "MI_RM_ba_rfia", row.names = FALSE)


# plotFIA(tpa_mi)
# # Other states test:
# test_rfia <- getFIA(states = c('IN', 'VT', 'NY'))
# 
# res2 <- tpa(test_rfia, bySpecies = TRUE)

vol <- volume(mi_rfia, bySpecies = TRUE, treeType = "all", volType = "gross",
       totals = TRUE, variance = TRUE)

# volume(db, grpBy = NULL, polys = NULL, returnSpatial = FALSE,
#        bySpecies = FALSE, bySizeClass = FALSE, landType = "forest",
#        treeType = "live", volType = "net", method = "TI",
#        lambda = 0.5, treeDomain = NULL, areaDomain = NULL,
#        totals = FALSE, variance = FALSE, byPlot = FALSE, nCores = 1)

rmvrmvol <- filter(vol, SCIENTIFIC_NAME == 'Acer rubrum')
smvol <- filter(vol, SPCD == 318)

write.csv(smvol, "MI_SM_vol_rfia", row.names = FALSE)
write.csv(rmvol, "MI_RM_vol_rfia", row.names = FALSE)


################################################################################
in_rfia <- getFIA(states = 'IN')
tpa_in <- tpa(in_rfia, bySpecies = TRUE)
str(tpa_in)

redmap <- filter(tpa_in, SCIENTIFIC_NAME == 'Acer rubrum')
sugmap <- filter(tpa_in, SCIENTIFIC_NAME == 'Acer saccharum')

write.csv(sugmap, "IN_SM_ba_rfia", row.names = FALSE)
write.csv(redmap, "IN_RM_ba_rfia", row.names = FALSE)

vol <- volume(in_rfia, bySpecies = TRUE, treeType = "all", volType = "gross",
              totals = TRUE, variance = TRUE)


# tpa(in_rfia, grpBy = STATECD) # use grpBy = STATECD for calculating state data 
# from whole US dataset





