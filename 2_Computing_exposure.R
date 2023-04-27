##### 2. COMPUTING EXPOSURE #####





#### LOADING PACKAGES ----
library(raster)
library(dplyr)
library(tidyr)
library(rgdal)
library(ade4)
library(factoextra)
library(geosphere)



#### LOCAL CLIMATE CHANGE INTENSITY (LC) ----

### Computation 
Mask <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/Zone_etude_15m_laea.tif")
SSP <- c("ssp126", "ssp245", "ssp370", "ssp585")
GCM <- c("BCC-CSM2-MR", "CanESM5", "CNRM-CM6-1", "CNRM-ESM2-1", "IPSL-CM6A-LR", "MIROC6", "MIROC-ES2L", "MRI-ESM2-0")

for (i in SSP) {
  for (j in GCM) {
    Axis1_nc <- mask(raster(paste0("Axis_1_BCA_1970_2000_", i, "_", j, "_laea.tif")), Mask)
    Axis2_nc <- mask(raster(paste0("Axis_2_BCA_1970_2000_", i, "_", j, "_laea.tif")), Mask)
    Axis1_90 <- mask(raster(paste0("Axis_1_BCA_2090_", i, "_", j, "_laea.tif")), Mask)
    Axis2_90 <- mask(raster(paste0("Axis_2_BCA_2090_", i, "_", j, "_laea.tif")), Mask)
    I2 <- sqrt(((Axis1_nc - Axis1_90)^2) + ((Axis2_nc - Axis2_90)^2))
    writeRaster(I2, filename = paste0("Local_CC_intensity_", i, "_", j, "_2axes_laea.tif"))
    print(j)
  }
  print(i)
}

### Mean model and standard deviation between GCMs
SSP <- c("ssp126", "ssp245", "ssp370", "ssp585")

for (i in SSP) {
  R1 <- raster(paste0("Local_CC_intensity_", i, "_BCC-CSM2-MR_2axes_laea.tif"))
  R2 <- raster(paste0("Local_CC_intensity_", i, "_CanESM5_2axes_laea.tif"))
  R3 <- raster(paste0("Local_CC_intensity_", i, "_CNRM-CM6-1_2axes_laea.tif"))
  R4 <- raster(paste0("Local_CC_intensity_", i, "_CNRM-ESM2-1_2axes_laea.tif"))
  R5 <- raster(paste0("Local_CC_intensity_", i, "_IPSL-CM6A-LR_2axes_laea.tif"))
  R6 <- raster(paste0("Local_CC_intensity_", i, "_MIROC6_2axes_laea.tif"))
  R7 <- raster(paste0("Local_CC_intensity_", i, "_MIROC-ES2L_2axes_laea.tif"))
  R8 <- raster(paste0("Local_CC_intensity_", i, "_MRI-ESM2-0_2axes_laea.tif"))
  M <- (R1 + R2 + R3 + R4 + R5 + R6 + R7 + R8)/8
  E <- sqrt((((R1 - M)^2) + ((R2 - M)^2) + ((R3 - M)^2) + ((R4 - M)^2) + ((R5 - M)^2) + ((R6 - M)^2) + ((R7 - M)^2) + ((R8 - M)^2))/8)
  writeRaster(M, filename = paste0("Local_CC_intensity_", i, "_", "mean_model_2axes_laea.tif"))
  writeRaster(E, filename = paste0("Local_CC_intensity_", i, "_", "SD_2axes_laea.tif"))
  print(i)
}



#### LOCAL ARTIFICIALNESS CHANGE INTENSITY (LA) ----

### Computation
Mask <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/Zone_etude_15m_laea.tif")
R585 <- mask(raster("LUH2_2090_ssp585_artificialness_laea.tif"), Mask)
R370 <- mask(raster("LUH2_2090_ssp370_artificialness_laea.tif"), Mask)
R245 <- mask(raster("LUH2_2090_ssp245_artificialness_laea.tif"), Mask)
R126 <- mask(raster("LUH2_2090_ssp126_artificialness_laea.tif"), Mask)
R_histo <- mask(raster("LUH2_1985_artificialness_laea.tif"), Mask)
D585 <- R585 - R_histo
D370 <- R370 - R_histo
D245 <- R245 - R_histo
D126 <- R126 - R_histo
writeRaster(D585, filename = "LUH2_Delta_2090-1985_ssp585_artificialness_laea.tif")
writeRaster(D370, filename = "LUH2_Delta_2090-1985_ssp370_artificialness_laea.tif")
writeRaster(D245, filename = "LUH2_Delta_2090-1985_ssp245_artificialness_laea.tif")
writeRaster(D126, filename = "LUH2_Delta_2090-1985_ssp126_artificialness_laea.tif")



#### CLIMATE VELOCITY (RC) AND ARTIFICIALNESS CHANGE INTENSITY CAUSED BY CLIMATE RELOCATION (RA) ----

### Computation - exemple here only for one scenario and one GCM
GCM <- "BCC-CSM2-MR"
SSP <- "ssp126"
S <- 0.45 # Threshold defined after a sensitivity analysis
ZE <- raster("Zone_etude_15m_laea.tif")
ZE_500 <- raster("Zone_etude_500k_15m_laea.tif")
KBA <- raster("KBA_IBA_40_15m_laea.tif")
Coord_x84 <- raster("Coord_ZE_x_wgs84_laea.tif")
Coord_y84 <- raster("Coord_ZE_y_wgs84_laea.tif")
Axis1_f <- raster(paste0("./WORLD_CLIM/BCA/Axis_1_BCA_2090_", SSP, "_", GCM, "_laea.tif"))
Axis2_f <- raster(paste0("./WORLD_CLIM/BCA/Axis_2_BCA_2090_", SSP, "_", GCM, "_laea.tif"))
Axis1_p <- raster(paste0("./WORLD_CLIM/BCA/Axis_1_BCA_1970_2000_", SSP, "_", GCM, "_laea.tif"))
Axis2_p <- raster(paste0("./WORLD_CLIM/BCA/Axis_2_BCA_1970_2000_", SSP, "_", GCM, "_laea.tif"))
Arti_p <- raster("./LUH2/LUH2_1985_artificialness_laea.tif")
Arti_f <- raster(paste0("./LUH2/LUH2_2090_", SSP, "_artificialness_laea.tif"))
Distance <- ZE
Distance[Distance == 1] <- NA
Refuge <- ZE_500
Refuge[Refuge == 1] <- 0
Arti <- ZE
Arti[Arti == 1] <- NA

dist_euc <- function(x, y) {
  sqrt(((Axis1_f[y] - Axis1_p[x])^2) + ((Axis2_f[y] - Axis2_p[x])^2))
}

ID <- 1:length(Distance)
Cell_list <- as.data.frame(ID)
Cell_list$ZE <- getValues(ZE)
Cell_list$ZE[is.na(Cell_list$ZE) == TRUE] <- 0

for (i in Cell_list$ID[Cell_list$ZE == 1]) {
  Analog_df <- as.data.frame(ID)
  Analog_df <- subset(Analog_df, dist_euc(i, Analog_df$ID) < S)
  if (dim(Analog_df)[1] < 1) {
    Distance[i] <- -9999
  }
  else {
    Analog_df$x84 <- Coord_x84[Analog_df$ID]
    Analog_df$y84 <- Coord_y84[Analog_df$ID]
    Analog_df$Dist <- distVincentyEllipsoid(cbind(Coord_x84[i],
                                                  Coord_y84[i]), 
                                            Analog_df[ , 2:3])
    Match <- Analog_df[which.min(Analog_df$Dist), ]
    Distance[i] <- Match$Dist
    Arti[i] <- Arti_f[Match$ID] - Arti_p[i]
  }
}

Distance <- Distance/1000
Velocity <- Distance/105
writeRaster(Distance, 
            filename = paste0("Distance_", SSP, "_", GCM, "_", S, ".tif"))
writeRaster(Velocity, 
            filename = paste0("Velocity_", SSP, "_", GCM, "_", S, ".tif"))
writeRaster(Arti, 
            filename = paste0("Delta_artificialness_velocity_2090_", SSP, "_", GCM, "_", S, ".tif"))


### Mean model and standard deviation between GCMs
SSP <- c("ssp126", "ssp245", "ssp370", "ssp585")

for (i in SSP) {
  R1 <- raster(paste0("Velocity_", i, "_BCC-CSM2-MR_0.45.tif"))
  R2 <- raster(paste0("Velocity_", i, "_CanESM5_0.45.tif"))
  R3 <- raster(paste0("Velocity_", i, "_CNRM-CM6-1_0.45.tif"))
  R4 <- raster(paste0("Velocity_", i, "_CNRM-ESM2-1_0.45.tif"))
  R5 <- raster(paste0("Velocity_", i, "_IPSL-CM6A-LR_0.45.tif"))
  R6 <- raster(paste0("Velocity_", i, "_MIROC6_0.45.tif"))
  R7 <- raster(paste0("Velocity_", i, "_MIROC-ES2L_0.45.tif"))
  R8 <- raster(paste0("Velocity_", i, "_MRI-ESM2-0_0.45.tif"))
  N1 <- R1
  N1[R1 >= 0] <- 0
  N1[R1 < 0] <- 1
  N2 <- R2
  N2[R2 >= 0] <- 0
  N2[R2 < 0] <- 1
  N3 <- R3
  N3[R3 >= 0] <- 0
  N3[R3 < 0] <- 1
  N4 <- R4
  N4[R4 >= 0] <- 0
  N4[R4 < 0] <- 1
  N5 <- R5
  N5[R5 >= 0] <- 0
  N5[R5 < 0] <- 1
  N6 <- R6
  N6[R6 >= 0] <- 0
  N6[R6 < 0] <- 1
  N7 <- R7
  N7[R7 >= 0] <- 0
  N7[R7 < 0] <- 1
  N8 <- R8
  N8[R8 >= 0] <- 0
  N8[R8 < 0] <- 1
  N <- N1 + N2 + N3 + N4 + N5 + N6 + N7 + N8
  R1[R1 < 0] <- NA
  R2[R2 < 0] <- NA
  R3[R3 < 0] <- NA
  R4[R4 < 0] <- NA
  R5[R5 < 0] <- NA
  R6[R6 < 0] <- NA
  R7[R7 < 0] <- NA
  R8[R8 < 0] <- NA
  R_avg <- mean(R1, R2, R3, R4, R5, R6, R7, R8, na.rm = TRUE)
  R_avg[N >= 4] <- -9999
  writeRaster(R_avg, filename = paste0("Velocity_", i,"_mean_model_0.45.tif"))
}

for (i in SSP) {
  V <- raster(paste0("Velocity_", i,"_mean_model_0.45.tif"))
  R1 <- raster(paste0("Delta_artificialness_velocity_2090_", i, "_BCC-CSM2-MR_0.45.tif"))
  R2 <- raster(paste0("Delta_artificialness_velocity_2090_", i, "_CanESM5_0.45.tif"))
  R3 <- raster(paste0("Delta_artificialness_velocity_2090_", i, "_CNRM-CM6-1_0.45.tif"))
  R4 <- raster(paste0("Delta_artificialness_velocity_2090_", i, "_CNRM-ESM2-1_0.45.tif"))
  R5 <- raster(paste0("Delta_artificialness_velocity_2090_", i, "_IPSL-CM6A-LR_0.45.tif"))
  R6 <- raster(paste0("Delta_artificialness_velocity_2090_", i, "_MIROC6_0.45.tif"))
  R7 <- raster(paste0("Delta_artificialness_velocity_2090_", i, "_MIROC-ES2L_0.45.tif"))
  R8 <- raster(paste0("Delta_artificialness_velocity_2090_", i, "_MRI-ESM2-0_0.45.tif"))
  R_avg <- mean(R1, R2, R3, R4, R5, R6, R7, R8, na.rm = TRUE)
  R_avg[V == -9999] <- -9999
  writeRaster(R_avg, filename = paste0("Delta_artificialness_velocity_2090_", i,"_mean_model_0.45.tif"))
}



#### EXPOSURE INDICES (LEI, REI AND EI) ----

### Computation
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/KBA_IBA_40_15m_laea.tif")
SSP <- c("ssp126", "ssp245", "ssp370", "ssp585")
GCM <- c("BCC-CSM2-MR", "CanESM5", "CNRM-CM6-1", "CNRM-ESM2-1", "IPSL-CM6A-LR", "MIROC6", "MIROC-ES2L", "MRI-ESM2-0")

for (i in SSP) {
  for (j in GCM) {
    LCCI <- mask(raster(paste0("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/WORLD_CLIM/BCA/LOCAL_INTENSITY/GCM/Local_CC_intensity_", i, "_", j, "_2axes_laea.tif")), KBA)
    FV <- mask(raster(paste0("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/WORLD_CLIM/BCA/FORWARD_VELOCITY/GCM/Velocity_", i, "_", j, "_0.45.tif")), KBA)
    LACI <- mask(raster(paste0("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/LUH2/DELTA_LOCAL/ARTIFICIALNESS/LUH2_Delta_2090-1985_", i, "_artificialness_laea.tif")), KBA)
    AV <- mask(raster(paste0("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/LUH2/DELTA_VELOCITY/GCM/Delta_artificialness_velocity_2090_", i, "_", j, "_0.45.tif")), KBA)
    AVq <- AV
    AVq[(FV < 0)] <- maxValue(AV)
    FVq <- FV
    FVq[(FV < 0)] <- maxValue(FV)
    LCCIq <- cut(LCCI, breaks = unique(quantile(LCCI, probs = seq(0,1, by = 0.01), na.rm = TRUE)), include.lowest = TRUE)
    FVq <- cut(FVq, breaks = unique(quantile(FVq, probs = seq(0,1, by = 0.01), na.rm = TRUE)), include.lowest = TRUE)
    LACIq <- cut(LACI, breaks = unique(quantile(LACI, probs = seq(0,1, by = 0.01), na.rm = TRUE)), include.lowest = TRUE)
    AVq <- cut(AVq, breaks = unique(quantile(AVq, probs = seq(0,1, by = 0.01), na.rm = TRUE)), include.lowest = TRUE)
    LCCIq <- (LCCIq * (99/(maxValue(LCCIq) - 1))) - ((99/(maxValue(LCCIq) - 1)) - 1)
    FVq <- (FVq * (99/(maxValue(FVq) - 1))) - ((99/(maxValue(FVq) - 1)) - 1)
    LACIq <- (LACIq * (99/(maxValue(LACIq) - 1))) - ((99/(maxValue(LACIq) - 1)) - 1)
    AVq <- (AVq * (99/(maxValue(AVq) - 1))) - ((99/(maxValue(AVq) - 1)) - 1)
    EI <- mean(LCCIq, FVq, LACIq, AVq)
    LE <- mean(LCCIq, LACIq)
    FE <- mean(FVq, AVq)
    writeRaster(EI, filename = paste0("Exposure_index_", i, "_", j, ".tif"))
    writeRaster(LE, filename = paste0("Local_exposure_", i, "_", j, ".tif"))
    writeRaster(FE, filename = paste0("Forward_exposure_", i, "_", j, ".tif"))
  }
}


### Mean model and standard deviation between GCMs
SSP <- c("ssp126", "ssp245", "ssp370", "ssp585")

for (i in SSP) {
  EI1 <- raster(paste0("Exposure_index_", i, "_BCC-CSM2-MR.tif"))
  EI2 <- raster(paste0("Exposure_index_", i, "_CanESM5.tif"))
  EI3 <- raster(paste0("Exposure_index_", i, "_CNRM-CM6-1.tif"))
  EI4 <- raster(paste0("Exposure_index_", i, "_CNRM-ESM2-1.tif"))
  EI5 <- raster(paste0("Exposure_index_", i, "_IPSL-CM6A-LR.tif"))
  EI6 <- raster(paste0("Exposure_index_", i, "_MIROC6.tif"))
  EI7 <- raster(paste0("Exposure_index_", i, "_MIROC-ES2L.tif"))
  EI8 <- raster(paste0("Exposure_index_", i, "_MRI-ESM2-0.tif"))
  M <- (EI1 + EI2 + EI3 + EI4 + EI5 + EI6 + EI7 + EI8)/8
  E <- sqrt((((EI1 - M)^2) + ((EI2 - M)^2) + ((EI3 - M)^2) + ((EI4 - M)^2) + ((EI5 - M)^2) + ((EI6 - M)^2) + ((EI7 - M)^2) + ((EI8 - M)^2))/8)
  writeRaster(M, filename = paste0("Exposure_index_", i, "_", "mean_model.tif"))
  writeRaster(E, filename = paste0("Exposure_index_", i, "_", "SD.tif"))
  cat("\r", i)
}

for (i in SSP) {
  EI1 <- raster(paste0("Local_exposure_", i, "_BCC-CSM2-MR.tif"))
  EI2 <- raster(paste0("Local_exposure_", i, "_CanESM5.tif"))
  EI3 <- raster(paste0("Local_exposure_", i, "_CNRM-CM6-1.tif"))
  EI4 <- raster(paste0("Local_exposure_", i, "_CNRM-ESM2-1.tif"))
  EI5 <- raster(paste0("Local_exposure_", i, "_IPSL-CM6A-LR.tif"))
  EI6 <- raster(paste0("Local_exposure_", i, "_MIROC6.tif"))
  EI7 <- raster(paste0("Local_exposure_", i, "_MIROC-ES2L.tif"))
  EI8 <- raster(paste0("Local_exposure_", i, "_MRI-ESM2-0.tif"))
  M <- (EI1 + EI2 + EI3 + EI4 + EI5 + EI6 + EI7 + EI8)/8
  E <- sqrt((((EI1 - M)^2) + ((EI2 - M)^2) + ((EI3 - M)^2) + ((EI4 - M)^2) + ((EI5 - M)^2) + ((EI6 - M)^2) + ((EI7 - M)^2) + ((EI8 - M)^2))/8)
  writeRaster(M, filename = paste0("Local_exposure_", i, "_", "mean_model.tif"))
  writeRaster(E, filename = paste0("Local_exposure_", i, "_", "SD.tif"))
  cat("\r", i)
}

for (i in SSP) {
  EI1 <- raster(paste0("Forward_exposure_", i, "_BCC-CSM2-MR.tif"))
  EI2 <- raster(paste0("Forward_exposure_", i, "_CanESM5.tif"))
  EI3 <- raster(paste0("Forward_exposure_", i, "_CNRM-CM6-1.tif"))
  EI4 <- raster(paste0("Forward_exposure_", i, "_CNRM-ESM2-1.tif"))
  EI5 <- raster(paste0("Forward_exposure_", i, "_IPSL-CM6A-LR.tif"))
  EI6 <- raster(paste0("Forward_exposure_", i, "_MIROC6.tif"))
  EI7 <- raster(paste0("Forward_exposure_", i, "_MIROC-ES2L.tif"))
  EI8 <- raster(paste0("Forward_exposure_", i, "_MRI-ESM2-0.tif"))
  M <- (EI1 + EI2 + EI3 + EI4 + EI5 + EI6 + EI7 + EI8)/8
  E <- sqrt((((EI1 - M)^2) + ((EI2 - M)^2) + ((EI3 - M)^2) + ((EI4 - M)^2) + ((EI5 - M)^2) + ((EI6 - M)^2) + ((EI7 - M)^2) + ((EI8 - M)^2))/8)
  writeRaster(M, filename = paste0("Forward_exposure_", i, "_", "mean_model.tif"))
  writeRaster(E, filename = paste0("Forward_exposure_", i, "_", "SD.tif"))
  cat("\r", i)
}





##### END OF SCRIPT #####
