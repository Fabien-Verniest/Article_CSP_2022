##### 1. PREPARING DATA #####





#### LOADING PACKAGES ----
library(raster)
library(rgdal)
library(sf)
library(nngeo)
library(beepr)
library(dplyr)
library(tidyr)
library(ade4)
library(factoextra)
library(geosphere)



#### CLIMATE DATA ----

### Aggregation to land-use resolution (15 arc-min)
for (j in 1:19) {
  R <- raster(paste0("wc2.1_5m_bio_", j, ".tif"))
  Rd <- aggregate(R, fact = 3, fun = mean, na.rm = TRUE)
  writeRaster(Rd, filename = paste0("wc2.1_15m_bio_", j, ".tif"))
  print(j)
}

SSP <- c("ssp126", "ssp245", "ssp370", "ssp585")
GCM <- c("BCC-CSM2-MR", "CanESM5", "CNRM-CM6-1", "CNRM-ESM2-1", "IPSL-CM6A-LR", "MIROC6", "MIROC-ES2L", "MRI-ESM2-0")

for (j in GCM) {
  for (k in SSP) {
    for (l in 1:19) {
      R <- raster(paste0("wc2.1_5m_bio", l, "_", j, "_", k, "_2041-2060.tif"))
      Rd <- aggregate(R, fact = 3, fun = mean, na.rm = TRUE)
      writeRaster(Rd, filename = paste0("wc2.1_15m_bio", l, "_", j, "_", k, "_2041-2060.tif"))
      print(k)
    }
  }  
}

### Projection in Lambert Azimuthal Equal Area
Emprise_500 <- as(extent(-19,52,12,57), 'SpatialPolygons')
crs(Emprise_500) <- "+proj=longlat +datum=WGS84 +no_defs"
Proj <- "+proj=laea +lat_0=37.3 +lon_0=13.3"

for (j in 1:19) {
  R <- crop(raster(paste0("wc2.1_15m_bio_", j, ".tif")), Emprise_500)
  Rp <- projectRaster(R, crs = Proj, method = 'bilinear')
  writeRaster(Rp, filename = paste0("wc2.1_15m_bio_", j, "_laea.tif"))
  print(j)
}

SSP <- c("ssp126", "ssp245", "ssp370", "ssp585")
GCM <- c("BCC-CSM2-MR", "CanESM5", "CNRM-CM6-1", "CNRM-ESM2-1", "IPSL-CM6A-LR", "MIROC6", "MIROC-ES2L", "MRI-ESM2-0")

for (j in GCM) {
  for (k in SSP) {
    for (l in 1:19) {
      R <- crop(raster(paste0("wc2.1_15m_bio", l, "_", j, "_", k, "_2081-2100.tif")), Emprise_500)
      Rp <- projectRaster(R, crs = Proj, method = 'bilinear')
      writeRaster(Rp, filename = paste0("wc2.1_15m_bio", l, "_", j, "_", k, "_2081-2100_laea.tif"))
      print(k)
    }
  }
}



#### LAND-USE DATA ----

### Projection in Lambert Azimuthal Equal Area
Emprise_500 <- as(extent(-19,52,12,57), 'SpatialPolygons')
crs(Emprise_500) <- "+proj=longlat +datum=WGS84 +no_defs"
Proj <- "+proj=laea +lat_0=37.3 +lon_0=13.3"
Variable <- c("primf", "primn","secdf", "secdn", "pastr", "range", "urban", "c3ann", "c3per", "c4ann", "c4per", "c3nfx")
Scenar <- c("1985", "2090_ssp126", "2090_ssp245", "2090_ssp370", "2090_ssp585")

for (i in Scenar) {
  for (j in Variable) {
    R <- crop(raster(paste0("LUH2_", i, "_", j,".tif")), Emprise_500)
    R[is.na(R) == TRUE] <- 0
    R_proj <- projectRaster(R, crs = Proj, method = "bilinear")
    writeRaster(R_proj, filename = paste0("LUH2_", i,"_", j,"_laea.tif"))
    print(j)
  }
  print(i)
}

R <- crop(raster(paste0("LUH2_ice_water.tif")), Emprise_500)
R_proj <- projectRaster(R, crs = Proj, method = "bilinear")
writeRaster(R_proj, filename = paste0("LUH2_ice_water_laea.tif"))



#### ELEVATION DATA ----

### Aggregation to land-use resolution (15 arc-min)
R <- raster("wc2.1_5m_elev.tif")
Rd <- aggregate(R, fact = 3, fun = mean, na.rm = TRUE)
writeRaster(Rd, filename = "wc2.1_15m_elev.tif")


### Projection in Lambert Azimuthal Equal Area
Emprise_500 <- as(extent(-19,52,12,57), 'SpatialPolygons')
crs(Emprise_500) <- "+proj=longlat +datum=WGS84 +no_defs"
Proj <- "+proj=laea +lat_0=37.3 +lon_0=13.3"
R <- crop(raster("wc2.1_15m_elev.tif"), Emprise_500)
Rp <- projectRaster(R, crs = Proj, method = 'bilinear')
writeRaster(Rp, filename = "wc2.1_15m_elev_laea.tif")



#### STUDY ZONE ----

### Projection in Lambert Azimuthal Equal Area
Proj <- "+proj=laea +lat_0=37.3 +lon_0=13.3"
ZE <- readOGR("~/_THESE/SIG/SHAPES/Zone_etude_wgs84.shp")
ZE_laea <- spTransform(ZE, Proj)
writeOGR(obj = ZE_laea, "tempdir", "Zone_etude_laea", driver = "ESRI Shapefile")

### Rasterisation
Med <- readOGR("~/_THESE/SIG/SHAPES/Zone_etude_laea.shp")
R_WC <- raster("~/_THESE/SIG/RASTERS/WORLD_CLIM/wc2.1_15m_bio_1_laea.tif")
R_LUH <- raster("~/_THESE/SIG/RASTERS/LUH2/LUH2_ice_water_laea.tif")
R_Med <- rasterize(Med, R_WC, getCover = TRUE)
R_Med_full <- R_Med
R_Med_full[R_Med > 0] <- 1
R_Med_full[R_Med == 0] <- NA
R_Med_full[is.na(R_WC) == TRUE] <- NA
R_Med_full[R_LUH == 1] <- NA
writeRaster(R_Med_full, filename = "Zone_etude_15m_laea.tif")
R_Med_500 <- buffer(R_Med_full, 500000, doEdge = TRUE)
R_Med_500[is.na(R_WC) == TRUE] <- NA
R_Med_500[R_LUH == 1] <- NA
writeRaster(R_Med_500, filename = "Zone_etude_500k_15m_laea.tif")

### Coordinates WGS84 in a raster format
Proj_LAEA <- "+proj=laea +lat_0=37.3 +lon_0=13.3"
Proj_WGS84 <- "+proj=longlat"
ZE <- raster("Zone_etude_15m_laea.tif")
Coord <- as.data.frame(coordinates(ZE))
Points_laea <- SpatialPointsDataFrame(coords = cbind(Coord$x, Coord$y),
                                      data = as.data.frame(Coord$x),
                                      proj4string = CRS(Proj_LAEA))
Points_WGS84 <- spTransform(Points_laea, Proj_WGS84)
Coord$x_84 <- as.data.frame(Points_WGS84)$coords.x1
Coord$y_84 <- as.data.frame(Points_WGS84)$coords.x2
R_x84 <- ZE
R_x84[] <- Coord$x_84
R_y84 <- ZE
R_y84[] <- Coord$y_84
writeRaster(R_x84, filename = "Coord_ZE_x_wgs84_laea.tif")
writeRaster(R_y84, filename = "Coord_ZE_y_wgs84_laea.tif")



#### COUNTRIES ----

### Rasterisation
Med <- raster("~/_THESE/SIG/RASTERS/ARTICLE_1/Zone_etude_15m_laea.tif")
Med_shp <- readOGR("Zone_etude_boundaries_laea.shp")
for (i in 1:30) {
  R <- rasterize(Med_shp[i, ], Med, getCover = TRUE)
  R[is.na(Med) == TRUE] <- NA
  if (i == 1) {
    Rtot <- stack(R)
  }
  else {
    Rtot <- addLayer(Rtot, R)
  }
  print(i)
}
Rpays <- Med
Rpays[Med == 1] <- 0

for (i in 1:30) {
  Rpays[Rtot[[i]] == max(Rtot)] <- i
}

writeRaster(Rpays, filename = "~/_THESE/SIG/RASTERS/ARTICLE_1/Countries_zone_etude_15m_laea.tif")



#### KEY BIODIVERSITY AREAS ----

### Projection in Lambert Azimuthal Equal Area
R_ref <- raster("Zone_etude_15m_laea.tif")
KBA <- rgdal::readOGR("~/_THESE/SIG/SHAPES/AIRES_BIODIVERSITE/KBA/KBA_IBA_Zone_etude_fusion_wgs84.shp")
Proj <- "+proj=laea +lat_0=37.3 +lon_0=13.3"
KBA_laea <- spTransform(KBA, Proj)
writeOGR(obj = KBA_laea, "tempdir", "KBA_IBA_Zone_etude_fusion_laea", driver = "ESRI Shapefile")

### Rasterisation
R_KBA <- rasterize(KBA_laea, R_ref, getCover = TRUE)
R_KBA_40 <- R_KBA
R_KBA_40[R_KBA >= 0.4] <- 1
R_KBA_40[R_KBA < 0.4] <- NA
R_KBA_40[is.na(R_ref) == TRUE] <- NA
writeRaster(R_KBA_40, filename = "KBA_IBA_40_15m_laea.tif")



#### PROTECTED AREAS ----

### Rasterisation
R_ref <- raster("Zone_etude_500k_15m_laea.tif")
PA_OZHM_fort <- readOGR("~/_THESE/SIG/SHAPES/AIRES_BIODIVERSITE/BD_PA_OZHM/ARTICLE_1/PAs_Zone_etude_I_IV_fusion_laea.shp")
R_fort <- rasterize(PA_OZHM_fort, R_ref, getCover = TRUE)
R_fort[is.na(R_ref) == TRUE] <- NA
R_fort[R_fort > 1] <- 1
writeRaster(R_fort, filename = "PA_I_IV_15m_laea.tif")

### Protected and non-protected KBAs
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/ARTICLE_1/KBA_IBA_40_15m_laea.tif")
PA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/ARTICLE_1/PA_I_IV_15m_laea.tif")
KBA_PA <- mask(PA, KBA)
KBA_PA_part <- KBA_PA
KBA_PA_part[KBA_PA_part > 0] <- 1
KBA_n_PA <- KBA_PA
KBA_n_PA[KBA_PA_part == 0] <- 1
KBA_n_PA[KBA_PA_part == 1] <- 0
writeRaster(KBA_PA_part, filename = "KBA_IBA_40_part_PA_15m_laea.tif")
writeRaster(KBA_n_PA, filename = "KBA_IBA_40_nPA_15m_laea.tif")



#### BETWEEN-CLASS ANALYSIS ----

Mask <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/Zone_etude_500k_15m_laea.tif")
Bio1_nc <- mask(raster("wc2.1_15m_bio_1_laea.tif"), Mask)
Bio5_nc <- mask(raster("wc2.1_15m_bio_5_laea.tif"), Mask)
Bio6_nc <- mask(raster("wc2.1_15m_bio_6_laea.tif"), Mask)
Bio7_nc <- mask(raster("wc2.1_15m_bio_7_laea.tif"), Mask)
Bio12_nc <- mask(raster("wc2.1_15m_bio_12_laea.tif"), Mask)
Bio16_nc <- mask(raster("wc2.1_15m_bio_16_laea.tif"), Mask)
Bio17_nc <- mask(raster("wc2.1_15m_bio_17_laea.tif"), Mask)
SSP <- c("ssp126", "ssp245", "ssp370", "ssp585")
GCM <- c("BCC-CSM2-MR", "CanESM5", "CNRM-CM6-1", "CNRM-ESM2-1", "IPSL-CM6A-LR", "MIROC6", "MIROC-ES2L", "MRI-ESM2-0")
GCM_2 <- c("BCC.CSM2.MR", "CanESM5", "CNRM.CM6.1", "CNRM.ESM2.1", "IPSL.CM6A.LR", "MIROC6", "MIROC.ES2L", "MRI.ESM2.0")

for (i in SSP) {
  for (j in 1:8) {
    Bio1_50 <- mask(raster(paste0("wc2.1_15m_bio1_", GCM[j], "_", i, "_2041-2060_laea.tif")), Mask)
    Bio5_50 <- mask(raster(paste0("wc2.1_15m_bio5_", GCM[j], "_", i, "_2041-2060_laea.tif")), Mask)
    Bio6_50 <- mask(raster(paste0("wc2.1_15m_bio6_", GCM[j], "_", i, "_2041-2060_laea.tif")), Mask)
    Bio7_50 <- mask(raster(paste0("wc2.1_15m_bio7_", GCM[j], "_", i, "_2041-2060_laea.tif")), Mask)
    Bio12_50 <- mask(raster(paste0("wc2.1_15m_bio12_", GCM[j], "_", i, "_2041-2060_laea.tif")), Mask)
    Bio16_50 <- mask(raster(paste0("wc2.1_15m_bio16_", GCM[j], "_", i, "_2041-2060_laea.tif")), Mask)
    Bio17_50 <- mask(raster(paste0("wc2.1_15m_bio17_", GCM[j], "_", i, "_2041-2060_laea.tif")), Mask)
    Bio1_90 <- mask(raster(paste0("wc2.1_15m_bio1_", GCM[j], "_", i, "_2081-2100_laea.tif")), Mask)
    Bio5_90 <- mask(raster(paste0("wc2.1_15m_bio5_", GCM[j], "_", i, "_2081-2100_laea.tif")), Mask)
    Bio6_90 <- mask(raster(paste0("wc2.1_15m_bio6_", GCM[j], "_", i, "_2081-2100_laea.tif")), Mask)
    Bio7_90 <- mask(raster(paste0("wc2.1_15m_bio7_", GCM[j], "_", i, "_2081-2100_laea.tif")), Mask)
    Bio12_90 <- mask(raster(paste0("wc2.1_15m_bio12_", GCM[j], "_", i, "_2081-2100_laea.tif")), Mask)
    Bio16_90 <- mask(raster(paste0("wc2.1_15m_bio16_", GCM[j], "_", i, "_2081-2100_laea.tif")), Mask)
    Bio17_90 <- mask(raster(paste0("wc2.1_15m_bio17_", GCM[j], "_", i, "_2081-2100_laea.tif")), Mask)
    
    Data_nc <- cbind(as.data.frame(Bio1_nc), as.data.frame(Bio5_nc), as.data.frame(Bio6_nc), as.data.frame(Bio7_nc), as.data.frame(Bio12_nc), as.data.frame(Bio16_nc), as.data.frame(Bio17_nc))
    Data_50 <- cbind(as.data.frame(Bio1_50), as.data.frame(Bio5_50), as.data.frame(Bio6_50), as.data.frame(Bio7_50), as.data.frame(Bio12_50), as.data.frame(Bio16_50), as.data.frame(Bio17_50))
    Data_90 <- cbind(as.data.frame(Bio1_90), as.data.frame(Bio5_90), as.data.frame(Bio6_90), as.data.frame(Bio7_90), as.data.frame(Bio12_90), as.data.frame(Bio16_90), as.data.frame(Bio17_90))
    
    Source_nc <- "near-current"
    Data_nc <- cbind(Data_nc, Source_nc)
    Source_50 <- "2050"
    Data_50 <- cbind(Data_50, Source_50)
    Source_90 <- "2090"
    Data_90 <- cbind(Data_90, Source_90)
    
    Data_nc <- rename(Data_nc, Bio1 = wc2.1_15m_bio_1_laea, Bio5 = wc2.1_15m_bio_5_laea, Bio6 = wc2.1_15m_bio_6_laea, Bio7 = wc2.1_15m_bio_7_laea, Bio12 = wc2.1_15m_bio_12_laea, Bio16 = wc2.1_15m_bio_16_laea, Bio17 = wc2.1_15m_bio_17_laea, Source = Source_nc)
    Data_50 <- rename(Data_50, Bio1 = paste0("wc2.1_15m_bio1_", GCM_2[j], "_", i, "_2041.2060_laea"), Bio5 = paste0("wc2.1_15m_bio5_", GCM_2[j], "_", i, "_2041.2060_laea"), Bio6 = paste0("wc2.1_15m_bio6_", GCM_2[j], "_", i, "_2041.2060_laea"), Bio7 = paste0("wc2.1_15m_bio7_", GCM_2[j], "_", i, "_2041.2060_laea"), Bio12 = paste0("wc2.1_15m_bio12_", GCM_2[j], "_", i, "_2041.2060_laea"), Bio16 = paste0("wc2.1_15m_bio16_", GCM_2[j], "_", i, "_2041.2060_laea"), Bio17 = paste0("wc2.1_15m_bio17_", GCM_2[j], "_", i, "_2041.2060_laea"), Source = Source_50)
    Data_90 <- rename(Data_90, Bio1 = paste0("wc2.1_15m_bio1_", GCM_2[j], "_", i, "_2081.2100_laea"), Bio5 = paste0("wc2.1_15m_bio5_", GCM_2[j], "_", i, "_2081.2100_laea"), Bio6 = paste0("wc2.1_15m_bio6_", GCM_2[j], "_", i, "_2081.2100_laea"), Bio7 = paste0("wc2.1_15m_bio7_", GCM_2[j], "_", i, "_2081.2100_laea"), Bio12 = paste0("wc2.1_15m_bio12_", GCM_2[j], "_", i, "_2081.2100_laea"), Bio16 = paste0("wc2.1_15m_bio16_", GCM_2[j], "_", i, "_2081.2100_laea"), Bio17 = paste0("wc2.1_15m_bio17_", GCM_2[j], "_", i, "_2081.2100_laea"), Source = Source_90)

    Data_nc <- drop_na(Data_nc)
    Data_50 <- drop_na(Data_50)
    Data_90 <- drop_na(Data_90)

    Data_PCA <- rbind(Data_nc, Data_50, Data_90)
    
    res_PCA <- dudi.pca(Data_PCA[,1:7], center = TRUE, scale = TRUE, scannf = FALSE, nf = 7)
    P <- fviz_pca_var(res_PCA, repel = TRUE, title = paste0("Variables - PCA - ", i, " ", GCM[j]))
    plot(P)
    P <- fviz_pca_ind(res_PCA, geom.ind = "point", habillage = Data_PCA[,8], title = paste0("Individuals - PCA - ", i, " ", GCM[j]))
    plot(P)
    res_BCA <- bca(res_PCA, Data_PCA[,8], scannf = FALSE, nf = 2)
    summary(res_BCA)
    plot(res_BCA)
    
    Data_PCA <- cbind(Data_PCA, res_BCA$ls)
    Data_PCA_nc <- Data_PCA[1:17554,]
    Data_PCA_50 <- Data_PCA[17555:35108,]
    Data_PCA_90 <- Data_PCA[35109:52662,]
    
    PC1_nc <- Bio1_nc
    PC1_nc[is.na(PC1_nc) == FALSE] <- Data_PCA_nc$CS1
    PC2_nc <- Bio1_nc
    PC2_nc[is.na(PC2_nc) == FALSE] <- Data_PCA_nc$CS2
    PC1_50 <- Bio1_50
    PC1_50[is.na(PC1_50) == FALSE] <- Data_PCA_50$CS1
    PC2_50 <- Bio1_50
    PC2_50[is.na(PC2_50) == FALSE] <- Data_PCA_50$CS2
    PC1_90 <- Bio1_90
    PC1_90[is.na(PC1_90) == FALSE] <- Data_PCA_90$CS1
    PC2_90 <- Bio1_90
    PC2_90[is.na(PC2_90) == FALSE] <- Data_PCA_90$CS2
    
    writeRaster(PC1_nc, filename = paste0("Axis_1_BCA_1970_2000_", i, "_", GCM[j], "_laea.tif"))
    writeRaster(PC2_nc, filename = paste0("Axis_2_BCA_1970_2000_", i, "_", GCM[j], "_laea.tif"))
    writeRaster(PC1_50, filename = paste0("Axis_1_BCA_2050_", i, "_", GCM[j], "_laea.tif"))
    writeRaster(PC2_50, filename = paste0("Axis_2_BCA_2050_", i, "_", GCM[j], "_laea.tif"))
    writeRaster(PC1_90, filename = paste0("Axis_1_BCA_2090_", i, "_", GCM[j], "_laea.tif"))
    writeRaster(PC2_90, filename = paste0("Axis_2_BCA_2090_", i, "_", GCM[j], "_laea.tif"))
    
    print(j)
  }
  print(i)
}



#### ARTIFICIALNESS INDEX ----

Scenar <- c("1985", "2090_ssp126", "2090_ssp245", "2090_ssp370", "2090_ssp585", "2050_ssp126", "2050_ssp245", "2050_ssp370", "2050_ssp585")
Mask <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/ARTICLE_1/Zone_etude_500k_15m_laea.tif")
R_icwtr <- mask(raster("LUH2_ice_water_laea.tif"), Mask)

for (i in Scenar) {
  # Import des donnees
  R1 <- mask(raster(paste0("LUH2_", i, "_c3ann_laea.tif")), Mask)
  R2 <- mask(raster(paste0("LUH2_", i, "_c3per_laea.tif")), Mask)
  R3 <- mask(raster(paste0("LUH2_", i, "_c3nfx_laea.tif")), Mask)
  R4 <- mask(raster(paste0("LUH2_", i, "_c4ann_laea.tif")), Mask)
  R5 <- mask(raster(paste0("LUH2_", i, "_c4per_laea.tif")), Mask)
  R6 <- mask(raster(paste0("LUH2_", i, "_pastr_laea.tif")), Mask)
  R7 <- mask(raster(paste0("LUH2_", i, "_range_laea.tif")), Mask)
  R8 <- mask(raster(paste0("LUH2_", i, "_primf_laea.tif")), Mask)
  R9 <- mask(raster(paste0("LUH2_", i, "_primn_laea.tif")), Mask)
  R10 <- mask(raster(paste0("LUH2_", i, "_secdf_laea.tif")), Mask)
  R11 <- mask(raster(paste0("LUH2_", i, "_secdn_laea.tif")), Mask)
  R12 <- mask(raster(paste0("LUH2_", i, "_urban_laea.tif")), Mask)
  
  # Conversion en part de milieux terrestres
  R1 <- R1/(1 - R_icwtr)
  R2 <- R2/(1 - R_icwtr)
  R3 <- R3/(1 - R_icwtr)
  R4 <- R4/(1 - R_icwtr)
  R5 <- R5/(1 - R_icwtr)
  R6 <- R6/(1 - R_icwtr)
  R7 <- R7/(1 - R_icwtr)
  R8 <- R8/(1 - R_icwtr)
  R9 <- R9/(1 - R_icwtr)
  R10 <- R10/(1 - R_icwtr)
  R11 <- R11/(1 - R_icwtr)
  R12 <- R12/(1 - R_icwtr)
  
  Artificialness <- (9*R12) + (7 * (R1 + R3 + R4)) + (6 * (R2 + R5)) +  (5.5 * R6) + (3.5 * R7) + (2 * (R10 + R11)) + (0.5 * (R8 + R9))
  writeRaster(Artificialness, filename = paste0("LUH2_", i, "_artificialness_laea.tif"))

  print(i)
}





##### END OF SCRIPT #####