##### BETWEEN CLASS ANALYSIS #####





#### LOADING PACKAGES ----
library(raster)
library(dplyr)
library(tidyr)
library(rgdal)
library(ade4)
library(factoextra)



#### EXEMPLE WITH ONE GCM AND ONE SCENARIO ----

## Working directory
setwd("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/WORLD_CLIM")


## Study zone mask
Mask <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/Zone_etude_500k_15m_laea.tif")


## Import data
# Near-current
Bio1_nc <- mask(raster("wc2.1_15m_bio_1_laea.tif"), Mask)
Bio5_nc <- mask(raster("wc2.1_15m_bio_5_laea.tif"), Mask)
Bio6_nc <- mask(raster("wc2.1_15m_bio_6_laea.tif"), Mask)
Bio7_nc <- mask(raster("wc2.1_15m_bio_7_laea.tif"), Mask)
Bio12_nc <- mask(raster("wc2.1_15m_bio_12_laea.tif"), Mask)
Bio16_nc <- mask(raster("wc2.1_15m_bio_16_laea.tif"), Mask)
Bio17_nc <- mask(raster("wc2.1_15m_bio_17_laea.tif"), Mask)

# 2050
Bio1_50 <- mask(raster("wc2.1_15m_bio1_MIROC6_ssp585_2041-2060_laea.tif"), Mask)
Bio5_50 <- mask(raster("wc2.1_15m_bio5_MIROC6_ssp585_2041-2060_laea.tif"), Mask)
Bio6_50 <- mask(raster("wc2.1_15m_bio6_MIROC6_ssp585_2041-2060_laea.tif"), Mask)
Bio7_50 <- mask(raster("wc2.1_15m_bio7_MIROC6_ssp585_2041-2060_laea.tif"), Mask)
Bio12_50 <- mask(raster("wc2.1_15m_bio12_MIROC6_ssp585_2041-2060_laea.tif"), Mask)
Bio16_50 <- mask(raster("wc2.1_15m_bio16_MIROC6_ssp585_2041-2060_laea.tif"), Mask)
Bio17_50 <- mask(raster("wc2.1_15m_bio17_MIROC6_ssp585_2041-2060_laea.tif"), Mask)

# 2090
Bio1_90 <- mask(raster("wc2.1_15m_bio1_MIROC6_ssp585_2081-2100_laea.tif"), Mask)
Bio5_90 <- mask(raster("wc2.1_15m_bio5_MIROC6_ssp585_2081-2100_laea.tif"), Mask)
Bio6_90 <- mask(raster("wc2.1_15m_bio6_MIROC6_ssp585_2081-2100_laea.tif"), Mask)
Bio7_90 <- mask(raster("wc2.1_15m_bio7_MIROC6_ssp585_2081-2100_laea.tif"), Mask)
Bio12_90 <- mask(raster("wc2.1_15m_bio12_MIROC6_ssp585_2081-2100_laea.tif"), Mask)
Bio16_90 <- mask(raster("wc2.1_15m_bio16_MIROC6_ssp585_2081-2100_laea.tif"), Mask)
Bio17_90 <- mask(raster("wc2.1_15m_bio17_MIROC6_ssp585_2081-2100_laea.tif"), Mask)


## Data treatment
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
Data_50 <- rename(Data_50, Bio1 = wc2.1_15m_bio1_MIROC6_ssp585_2041.2060_laea, Bio5 = wc2.1_15m_bio5_MIROC6_ssp585_2041.2060_laea, Bio6 = wc2.1_15m_bio6_MIROC6_ssp585_2041.2060_laea, Bio7 = wc2.1_15m_bio7_MIROC6_ssp585_2041.2060_laea, Bio12 = wc2.1_15m_bio12_MIROC6_ssp585_2041.2060_laea, Bio16 = wc2.1_15m_bio16_MIROC6_ssp585_2041.2060_laea, Bio17 = wc2.1_15m_bio17_MIROC6_ssp585_2041.2060_laea, Source = Source_50)
Data_90 <- rename(Data_90, Bio1 = wc2.1_15m_bio1_MIROC6_ssp585_2081.2100_laea, Bio5 = wc2.1_15m_bio5_MIROC6_ssp585_2081.2100_laea, Bio6 = wc2.1_15m_bio6_MIROC6_ssp585_2081.2100_laea, Bio7 = wc2.1_15m_bio7_MIROC6_ssp585_2081.2100_laea, Bio12 = wc2.1_15m_bio12_MIROC6_ssp585_2081.2100_laea, Bio16 = wc2.1_15m_bio16_MIROC6_ssp585_2081.2100_laea, Bio17 = wc2.1_15m_bio17_MIROC6_ssp585_2081.2100_laea, Source = Source_90)

Data_nc <- drop_na(Data_nc)
Data_50 <- drop_na(Data_50)
Data_90 <- drop_na(Data_90)

Data_PCA <- rbind(Data_nc, Data_50, Data_90)


## PCA with all axes using package ade4
res_PCA <- dudi.pca(Data_PCA[,1:7], center = TRUE, scale = TRUE, scannf = FALSE, nf = 7)

# Percentage of variance
fviz_eig(res_PCA)
get_eigenvalue(res_PCA)

# Correlation circle
fviz_pca_var(res_PCA, repel = TRUE, title = "Variables - PCA - SSP5-8.5 MIROC6")

# Coordinates of variables on the axes
res.var <- get_pca_var(res_PCA)
res.var$coord

# Individuals plot
fviz_pca_ind(res_PCA, geom.ind = "point", habillage = Data_PCA[,8], title = "Individuals - PCA - SSP5-8.5 MIROC6")

# Coordinates of individuals on the axes
res.ind <- get_pca_ind(res_PCA)
res.ind$coord


## BCA on the PCA
res_BCA <- bca(res_PCA, Data_PCA[ ,8], scannf = FALSE, nf = 2)

# BCA summary
summary(res_BCA)

# Graph
plot(res_BCA)


## Adding coordinates to initial data
Data_PCA <- cbind(Data_PCA, res_BCA$ls)
Data_PCA_nc <- Data_PCA[1:17554,]
Data_PCA_50 <- Data_PCA[17555:35108,]
Data_PCA_90 <- Data_PCA[35109:52662,]


## Transforming into rasters
# Near current
PC1_nc <- Bio1_nc
PC1_nc[is.na(PC1_nc) == FALSE] <- Data_PCA_nc$CS1
PC2_nc <- Bio1_nc
PC2_nc[is.na(PC2_nc) == FALSE] <- Data_PCA_nc$CS2

# 2050
PC1_50 <- Bio1_50
PC1_50[is.na(PC1_50) == FALSE] <- Data_PCA_50$CS1
PC2_50 <- Bio1_50
PC2_50[is.na(PC2_50) == FALSE] <- Data_PCA_50$CS2

# 2090
PC1_90 <- Bio1_90
PC1_90[is.na(PC1_90) == FALSE] <- Data_PCA_90$CS1
PC2_90 <- Bio1_90
PC2_90[is.na(PC2_90) == FALSE] <- Data_PCA_90$CS2


## Rasters export
writeRaster(PC1_nc, filename = "PC1_1970_2000_SSP585_CanESM5_laea.tif")
writeRaster(PC2_nc, filename = "PC2_1970_2000_SSP585_CanESM5_laea.tif")
writeRaster(PC1_50, filename = "PC1_2050_SSP585_CanESM5_laea.tif")
writeRaster(PC2_50, filename = "PC2_2050_SSP585_CanESM5_laea.tif")
writeRaster(PC1_90, filename = "PC1_2090_SSP585_CanESM5_laea.tif")
writeRaster(PC2_90, filename = "PC2_2090_SSP585_CanESM5_laea.tif")





##### END OF THE SCRIPT #####