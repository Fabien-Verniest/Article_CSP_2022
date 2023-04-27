##### 3. DATA ANALYSIS #####





#### LOADING PACKAGES ----
library(raster) # Manipulation de rasters
library(dplyr) # Manipulation de donnees
library(tidyr) # Manipulation de donnees
library(nlme) # Ajustement de modeles lineaires mixtes
library(piecewiseSEM) # Calcul de R2 pour modeles lineaires mixtes



#### 3.1. DIFFERENCES IN EXPOSURE BETWEEN KBAS AND NON-KBAS ----

### Elevation
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/ARTICLE_1/KBA_IBA_40_15m_laea.tif")
ZE <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/ARTICLE_1/Zone_etude_15m_laea.tif")
NKBA <- ZE
NKBA[KBA == 1] <- NA
SRTM <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/ARTICLE_1/SRTM/wc2.1_15m_elev_laea.tif")
M_KBA <- as.data.frame(SRTM[KBA == 1])
M_NKBA <- as.data.frame(SRTM[NKBA == 1])
wilcox.test(M_KBA$`SRTM[KBA == 1]`, M_NKBA$`SRTM[NKBA == 1]`, paired = FALSE, conf.int = TRUE)

### Near-current artificialness index
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/ARTICLE_1/KBA_IBA_40_15m_laea.tif")
ZE <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/ARTICLE_1/Zone_etude_15m_laea.tif")
NKBA <- ZE
NKBA[KBA == 1] <- NA
Arti <- raster("LUH2_1985_artificialness_zone_etude_laea.tif")
M_KBA <- as.data.frame(Arti[KBA == 1])
M_NKBA <- as.data.frame(Arti[NKBA == 1])
wilcox.test(M_KBA$`Arti[KBA == 1]`, M_NKBA$`Arti[NKBA == 1]`, paired = FALSE, conf.int = TRUE)

### Near-current bioclimatic variables
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/ARTICLE_1/KBA_IBA_40_15m_laea.tif")
ZE <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/ARTICLE_1/Zone_etude_15m_laea.tif")
NKBA <- ZE
NKBA[KBA == 1] <- NA
Bio1 <- raster("./PROCESSED/LAEA/CLIMATOLOGIES/wc2.1_15m_bio_1_laea.tif")
Bio5 <- raster("./PROCESSED/LAEA/CLIMATOLOGIES/wc2.1_15m_bio_5_laea.tif")
Bio6 <- raster("./PROCESSED/LAEA/CLIMATOLOGIES/wc2.1_15m_bio_6_laea.tif")
Bio7 <- raster("./PROCESSED/LAEA/CLIMATOLOGIES/wc2.1_15m_bio_7_laea.tif")
Bio12 <- raster("./PROCESSED/LAEA/CLIMATOLOGIES/wc2.1_15m_bio_12_laea.tif")
Bio16 <- raster("./PROCESSED/LAEA/CLIMATOLOGIES/wc2.1_15m_bio_16_laea.tif")
Bio17 <- raster("./PROCESSED/LAEA/CLIMATOLOGIES/wc2.1_15m_bio_17_laea.tif")
M_KBABio1 <- as.data.frame(Bio1[KBA == 1])
M_NKBABio1 <- as.data.frame(Bio1[NKBA == 1])
M_KBABio5 <- as.data.frame(Bio5[KBA == 1])
M_NKBABio5 <- as.data.frame(Bio5[NKBA == 1])
M_KBABio6 <- as.data.frame(Bio6[KBA == 1])
M_NKBABio6 <- as.data.frame(Bio6[NKBA == 1])
M_KBABio7 <- as.data.frame(Bio7[KBA == 1])
M_NKBABio7 <- as.data.frame(Bio7[NKBA == 1])
M_KBABio12 <- as.data.frame(Bio12[KBA == 1])
M_NKBABio12 <- as.data.frame(Bio12[NKBA == 1])
M_KBABio16 <- as.data.frame(Bio16[KBA == 1])
M_NKBABio16 <- as.data.frame(Bio16[NKBA == 1])
M_KBABio17 <- as.data.frame(Bio17[KBA == 1])
M_NKBABio17 <- as.data.frame(Bio17[NKBA == 1])
wilcox.test(M_KBABio1$`Bio1[KBA == 1]`, M_NKBABio1$`Bio1[NKBA == 1]`, paired = FALSE, conf.int = TRUE)
wilcox.test(M_KBABio5$`Bio5[KBA == 1]`, M_NKBABio5$`Bio5[NKBA == 1]`, paired = FALSE, conf.int = TRUE)
wilcox.test(M_KBABio6$`Bio6[KBA == 1]`, M_NKBABio6$`Bio6[NKBA == 1]`, paired = FALSE, conf.int = TRUE)
wilcox.test(M_KBABio7$`Bio7[KBA == 1]`, M_NKBABio7$`Bio7[NKBA == 1]`, paired = FALSE, conf.int = TRUE)
wilcox.test(M_KBABio12$`Bio12[KBA == 1]`, M_NKBABio12$`Bio12[NKBA == 1]`, paired = FALSE, conf.int = TRUE)
wilcox.test(M_KBABio16$`Bio16[KBA == 1]`, M_NKBABio16$`Bio16[NKBA == 1]`, paired = FALSE, conf.int = TRUE)
wilcox.test(M_KBABio17$`Bio17[KBA == 1]`, M_NKBABio17$`Bio17[NKBA == 1]`, paired = FALSE, conf.int = TRUE)

### LC
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/ARTICLE_1/KBA_IBA_40_15m_laea.tif")
ZE <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/ARTICLE_1/Zone_etude_15m_laea.tif")
NKBA <- ZE
NKBA[KBA == 1] <- NA
M126_KBA <- mask(raster("Local_CC_intensity_ssp126_mean_model_2axes_laea.tif"), KBA)
M245_KBA <- mask(raster("Local_CC_intensity_ssp245_mean_model_2axes_laea.tif"), KBA)
M370_KBA <- mask(raster("Local_CC_intensity_ssp370_mean_model_2axes_laea.tif"), KBA)
M585_KBA <- mask(raster("Local_CC_intensity_ssp585_mean_model_2axes_laea.tif"), KBA)
M126_NKBA <- mask(raster("Local_CC_intensity_ssp126_mean_model_2axes_laea.tif"), NKBA)
M245_NKBA <- mask(raster("Local_CC_intensity_ssp245_mean_model_2axes_laea.tif"), NKBA)
M370_NKBA <- mask(raster("Local_CC_intensity_ssp370_mean_model_2axes_laea.tif"), NKBA)
M585_NKBA <- mask(raster("Local_CC_intensity_ssp585_mean_model_2axes_laea.tif"), NKBA)
M_126_KBA <- as.data.frame(M126_KBA)
M_126_KBA <- drop_na(M_126_KBA)
M_245_KBA <- as.data.frame(M245_KBA)
M_245_KBA <- drop_na(M_245_KBA)
M_370_KBA <- as.data.frame(M370_KBA)
M_370_KBA <- drop_na(M_370_KBA)
M_585_KBA <- as.data.frame(M585_KBA)
M_585_KBA <- drop_na(M_585_KBA)
M_126_NKBA <- as.data.frame(M126_NKBA)
M_126_NKBA <- drop_na(M_126_NKBA)
M_245_NKBA <- as.data.frame(M245_NKBA)
M_245_NKBA <- drop_na(M_245_NKBA)
M_370_NKBA <- as.data.frame(M370_NKBA)
M_370_NKBA <- drop_na(M_370_NKBA)
M_585_NKBA <- as.data.frame(M585_NKBA)
M_585_NKBA <- drop_na(M_585_NKBA)
wilcox.test(M_126_KBA$Local_CC_intensity_ssp126_mean_model_2axes_laea, M_126_NKBA$Local_CC_intensity_ssp126_mean_model_2axes_laea, paired = FALSE, conf.int = TRUE)
wilcox.test(M_245_KBA$Local_CC_intensity_ssp245_mean_model_2axes_laea, M_245_NKBA$Local_CC_intensity_ssp245_mean_model_2axes_laea, paired = FALSE, conf.int = TRUE)
wilcox.test(M_370_KBA$Local_CC_intensity_ssp370_mean_model_2axes_laea, M_370_NKBA$Local_CC_intensity_ssp370_mean_model_2axes_laea, paired = FALSE, conf.int = TRUE)
wilcox.test(M_585_KBA$Local_CC_intensity_ssp585_mean_model_2axes_laea, M_585_NKBA$Local_CC_intensity_ssp585_mean_model_2axes_laea, paired = FALSE, conf.int = TRUE)

### RC
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/KBA_IBA_40_15m_laea.tif")
ZE <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/Zone_etude_15m_laea.tif")
NKBA <- ZE
NKBA[KBA == 1] <- NA
M126_KBA <- mask(raster("Velocity_ssp126_mean_model_0.45.tif"), KBA)
M245_KBA <- mask(raster("Velocity_ssp245_mean_model_0.45.tif"), KBA)
M370_KBA <- mask(raster("Velocity_ssp370_mean_model_0.45.tif"), KBA)
M585_KBA <- mask(raster("Velocity_ssp585_mean_model_0.45.tif"), KBA)
M126_NKBA <- mask(raster("Velocity_ssp126_mean_model_0.45.tif"), NKBA)
M245_NKBA <- mask(raster("Velocity_ssp245_mean_model_0.45.tif"), NKBA)
M370_NKBA <- mask(raster("Velocity_ssp370_mean_model_0.45.tif"), NKBA)
M585_NKBA <- mask(raster("Velocity_ssp585_mean_model_0.45.tif"), NKBA)
M126_KBA_no <- M126_KBA
M126_KBA_no[M126_KBA == -9999] <- NA
M245_KBA_no <- M245_KBA
M245_KBA_no[M245_KBA == -9999] <- NA
M370_KBA_no <- M370_KBA
M370_KBA_no[M370_KBA == -9999] <- NA
M585_KBA_no <- M585_KBA
M585_KBA_no[M585_KBA == -9999] <- NA
M126_NKBA_no <- M126_NKBA
M126_NKBA_no[M126_NKBA == -9999] <- NA
M245_NKBA_no <- M245_NKBA
M245_NKBA_no[M245_NKBA == -9999] <- NA
M370_NKBA_no <- M370_NKBA
M370_NKBA_no[M370_NKBA == -9999] <- NA
M585_NKBA_no <- M585_NKBA
M585_NKBA_no[M585_NKBA == -9999] <- NA
M_126_KBA <- as.data.frame(M126_KBA_no)
M_126_KBA <- drop_na(M_126_KBA)
M_245_KBA <- as.data.frame(M245_KBA_no)
M_245_KBA <- drop_na(M_245_KBA)
M_370_KBA <- as.data.frame(M370_KBA_no)
M_370_KBA <- drop_na(M_370_KBA)
M_585_KBA <- as.data.frame(M585_KBA_no)
M_585_KBA <- drop_na(M_585_KBA)
M_126_NKBA <- as.data.frame(M126_NKBA_no)
M_126_NKBA <- drop_na(M_126_NKBA)
M_245_NKBA <- as.data.frame(M245_NKBA_no)
M_245_NKBA <- drop_na(M_245_NKBA)
M_370_NKBA <- as.data.frame(M370_NKBA_no)
M_370_NKBA <- drop_na(M_370_NKBA)
M_585_NKBA <- as.data.frame(M585_NKBA_no)
M_585_NKBA <- drop_na(M_585_NKBA)
wilcox.test(M_126_KBA$Velocity_ssp126_mean_model_0.45, M_126_NKBA$Velocity_ssp126_mean_model_0.45, paired = FALSE, conf.int = TRUE)
wilcox.test(M_245_KBA$Velocity_ssp245_mean_model_0.45, M_245_NKBA$Velocity_ssp245_mean_model_0.45, paired = FALSE, conf.int = TRUE)
wilcox.test(M_370_KBA$Velocity_ssp370_mean_model_0.45, M_370_NKBA$Velocity_ssp370_mean_model_0.45, paired = FALSE, conf.int = TRUE)
wilcox.test(M_585_KBA$Velocity_ssp585_mean_model_0.45, M_585_NKBA$Velocity_ssp585_mean_model_0.45, paired = FALSE, conf.int = TRUE)

### LA
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/KBA_IBA_40_15m_laea.tif")
ZE <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/Zone_etude_15m_laea.tif")
NKBA <- ZE
NKBA[KBA == 1] <- NA
M126_KBA <- mask(raster("LUH2_Delta_2090-1985_ssp126_artificialness_laea.tif"), KBA)
M245_KBA <- mask(raster("LUH2_Delta_2090-1985_ssp245_artificialness_laea.tif"), KBA)
M370_KBA <- mask(raster("LUH2_Delta_2090-1985_ssp370_artificialness_laea.tif"), KBA)
M585_KBA <- mask(raster("LUH2_Delta_2090-1985_ssp585_artificialness_laea.tif"), KBA)
M126_NKBA <- mask(raster("LUH2_Delta_2090-1985_ssp126_artificialness_laea.tif"), NKBA)
M245_NKBA <- mask(raster("LUH2_Delta_2090-1985_ssp245_artificialness_laea.tif"), NKBA)
M370_NKBA <- mask(raster("LUH2_Delta_2090-1985_ssp370_artificialness_laea.tif"), NKBA)
M585_NKBA <- mask(raster("LUH2_Delta_2090-1985_ssp585_artificialness_laea.tif"), NKBA)
M_126_KBA <- as.data.frame(M126_KBA)
M_126_KBA <- drop_na(M_126_KBA)
M_245_KBA <- as.data.frame(M245_KBA)
M_245_KBA <- drop_na(M_245_KBA)
M_370_KBA <- as.data.frame(M370_KBA)
M_370_KBA <- drop_na(M_370_KBA)
M_585_KBA <- as.data.frame(M585_KBA)
M_585_KBA <- drop_na(M_585_KBA)
M_126_NKBA <- as.data.frame(M126_NKBA)
M_126_NKBA <- drop_na(M_126_NKBA)
M_245_NKBA <- as.data.frame(M245_NKBA)
M_245_NKBA <- drop_na(M_245_NKBA)
M_370_NKBA <- as.data.frame(M370_NKBA)
M_370_NKBA <- drop_na(M_370_NKBA)
M_585_NKBA <- as.data.frame(M585_NKBA)
M_585_NKBA <- drop_na(M_585_NKBA)
wilcox.test(M_126_KBA$LUH2_Delta_2090.1985_ssp126_artificialness_laea, M_126_NKBA$LUH2_Delta_2090.1985_ssp126_artificialness_laea, paired = FALSE, conf.int = TRUE)
wilcox.test(M_245_KBA$LUH2_Delta_2090.1985_ssp245_artificialness_laea, M_245_NKBA$LUH2_Delta_2090.1985_ssp245_artificialness_laea, paired = FALSE, conf.int = TRUE)
wilcox.test(M_370_KBA$LUH2_Delta_2090.1985_ssp370_artificialness_laea, M_370_NKBA$LUH2_Delta_2090.1985_ssp370_artificialness_laea, paired = FALSE, conf.int = TRUE)
wilcox.test(M_585_KBA$LUH2_Delta_2090.1985_ssp585_artificialness_laea, M_585_NKBA$LUH2_Delta_2090.1985_ssp585_artificialness_laea, paired = FALSE, conf.int = TRUE)

### RA
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/KBA_IBA_40_15m_laea.tif")
ZE <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/Zone_etude_15m_laea.tif")
NKBA <- ZE
NKBA[KBA == 1] <- NA
M126_KBA <- mask(raster("Delta_artificialness_velocity_2090_ssp126_mean_model_0.45.tif"), KBA)
M245_KBA <- mask(raster("Delta_artificialness_velocity_2090_ssp245_mean_model_0.45.tif"), KBA)
M370_KBA <- mask(raster("Delta_artificialness_velocity_2090_ssp370_mean_model_0.45.tif"), KBA)
M585_KBA <- mask(raster("Delta_artificialness_velocity_2090_ssp585_mean_model_0.45.tif"), KBA)
M126_NKBA <- mask(raster("Delta_artificialness_velocity_2090_ssp126_mean_model_0.45.tif"), NKBA)
M245_NKBA <- mask(raster("Delta_artificialness_velocity_2090_ssp245_mean_model_0.45.tif"), NKBA)
M370_NKBA <- mask(raster("Delta_artificialness_velocity_2090_ssp370_mean_model_0.45.tif"), NKBA)
M585_NKBA <- mask(raster("Delta_artificialness_velocity_2090_ssp585_mean_model_0.45.tif"), NKBA)
M126_KBA_no <- M126_KBA
M126_KBA_no[M126_KBA == -9999] <- NA
M245_KBA_no <- M245_KBA
M245_KBA_no[M245_KBA == -9999] <- NA
M370_KBA_no <- M370_KBA
M370_KBA_no[M370_KBA == -9999] <- NA
M585_KBA_no <- M585_KBA
M585_KBA_no[M585_KBA == -9999] <- NA
M126_NKBA_no <- M126_NKBA
M126_NKBA_no[M126_NKBA == -9999] <- NA
M245_NKBA_no <- M245_NKBA
M245_NKBA_no[M245_NKBA == -9999] <- NA
M370_NKBA_no <- M370_NKBA
M370_NKBA_no[M370_NKBA == -9999] <- NA
M585_NKBA_no <- M585_NKBA
M585_NKBA_no[M585_NKBA == -9999] <- NA
M_126_KBA <- as.data.frame(M126_KBA_no)
M_126_KBA <- drop_na(M_126_KBA)
M_245_KBA <- as.data.frame(M245_KBA_no)
M_245_KBA <- drop_na(M_245_KBA)
M_370_KBA <- as.data.frame(M370_KBA_no)
M_370_KBA <- drop_na(M_370_KBA)
M_585_KBA <- as.data.frame(M585_KBA_no)
M_585_KBA <- drop_na(M_585_KBA)
M_126_NKBA <- as.data.frame(M126_NKBA_no)
M_126_NKBA <- drop_na(M_126_NKBA)
M_245_NKBA <- as.data.frame(M245_NKBA_no)
M_245_NKBA <- drop_na(M_245_NKBA)
M_370_NKBA <- as.data.frame(M370_NKBA_no)
M_370_NKBA <- drop_na(M_370_NKBA)
M_585_NKBA <- as.data.frame(M585_NKBA_no)
M_585_NKBA <- drop_na(M_585_NKBA)
wilcox.test(M_126_KBA$Delta_artificialness_velocity_2090_ssp126_mean_model_0.45, M_126_NKBA$Delta_artificialness_velocity_2090_ssp126_mean_model_0.45, paired = FALSE, conf.int = TRUE)
wilcox.test(M_245_KBA$Delta_artificialness_velocity_2090_ssp245_mean_model_0.45, M_245_NKBA$Delta_artificialness_velocity_2090_ssp245_mean_model_0.45, paired = FALSE, conf.int = TRUE)
wilcox.test(M_370_KBA$Delta_artificialness_velocity_2090_ssp370_mean_model_0.45, M_370_NKBA$Delta_artificialness_velocity_2090_ssp370_mean_model_0.45, paired = FALSE, conf.int = TRUE)
wilcox.test(M_585_KBA$Delta_artificialness_velocity_2090_ssp585_mean_model_0.45, M_585_NKBA$Delta_artificialness_velocity_2090_ssp585_mean_model_0.45, paired = FALSE, conf.int = TRUE)



#### 3.2 DIFFERENCES IN EXPOSURE BETWEEN METRICS AND INDICES ----

### SSP 126
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/KBA_IBA_40_15m_laea.tif")
Intensity_126 <- mask(raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/WORLD_CLIM/BCA/LOCAL_INTENSITY/Local_CC_intensity_ssp126_mean_model_2axes_laea.tif"), KBA)
Velocity_126 <- mask(raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/WORLD_CLIM/BCA/FORWARD_VELOCITY/Velocity_ssp126_mean_model_0.45.tif"), KBA)
Delta_local_126 <- mask(raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/LUH2/DELTA_LOCAL/ARTIFICIALNESS/LUH2_Delta_2090-1985_ssp126_artificialness_laea.tif"), KBA)
Delta_velocity_126 <- mask(raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/LUH2/DELTA_VELOCITY/Delta_artificialness_velocity_2090_ssp126_mean_model_0.45.tif"), KBA)
Local_exposure_126 <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/EXPOSURE_INDEX/Local_exposure_ssp126_mean_model.tif")
Forward_exposure_126 <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/EXPOSURE_INDEX/Forward_exposure_ssp126_mean_model.tif")
Velocity_126[(Velocity_126 == -9999)] <- maxValue(Velocity_126)
Delta_velocity_126[(Delta_velocity_126 == -9999)] <- maxValue(Delta_velocity_126)
Intensity_126 <- as.data.frame(Intensity_126)
Velocity_126 <- as.data.frame(Velocity_126)
Delta_local_126 <- as.data.frame(Delta_local_126)
Delta_velocity_126 <- as.data.frame(Delta_velocity_126)
Local_exposure_126 <- as.data.frame(Local_exposure_126)
Forward_exposure_126 <- as.data.frame(Forward_exposure_126)
cor.test(Intensity_126$Local_CC_intensity_ssp126_mean_model_2axes_laea, Velocity_126$Velocity_ssp126_mean_model_0.45, method = "kendall")
cor.test(Intensity_126$Local_CC_intensity_ssp126_mean_model_2axes_laea, Delta_local_126$LUH2_Delta_2090.1985_ssp126_artificialness_laea, method = "kendall")
cor.test(Intensity_126$Local_CC_intensity_ssp126_mean_model_2axes_laea, Delta_velocity_126$Delta_artificialness_velocity_2090_ssp126_mean_model_0.45, method = "kendall")
cor.test(Velocity_126$Velocity_ssp126_mean_model_0.45, Delta_local_126$LUH2_Delta_2090.1985_ssp126_artificialness_laea, method = "kendall")
cor.test(Velocity_126$Velocity_ssp126_mean_model_0.45, Delta_velocity_126$Delta_artificialness_velocity_2090_ssp126_mean_model_0.45, method = "kendall")
cor.test(Delta_local_126$LUH2_Delta_2090.1985_ssp126_artificialness_laea, Delta_velocity_126$Delta_artificialness_velocity_2090_ssp126_mean_model_0.45, method = "kendall")
cor.test(Local_exposure_126$Local_exposure_ssp126_mean_model, Forward_exposure_126$Forward_exposure_ssp126_mean_model, method = "kendall")

### SSP 245
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/KBA_IBA_40_15m_laea.tif")
Intensity_245 <- mask(raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/WORLD_CLIM/BCA/LOCAL_INTENSITY/Local_CC_intensity_ssp_245_mean_model_2axes_laea.tif"), KBA)
Velocity_245 <- mask(raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/WORLD_CLIM/BCA/FORWARD_VELOCITY/Velocity_ssp_245_mean_model_0.45.tif"), KBA)
Delta_local_245 <- mask(raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/LUH2/DELTA_LOCAL/ARTIFICIALNESS/LUH2_Delta_2090-1985_ssp_245_artificialness_laea.tif"), KBA)
Delta_velocity_245 <- mask(raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/LUH2/DELTA_VELOCITY/Delta_artificialness_velocity_2090_ssp_245_mean_model_0.45.tif"), KBA)
Local_exposure_245 <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/EXPOSURE_INDEX/Local_exposure_ssp_245_mean_model.tif")
Forward_exposure_245 <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/EXPOSURE_INDEX/Forward_exposure_ssp_245_mean_model.tif")
Velocity_245[(Velocity_245 == -9999)] <- maxValue(Velocity_245)
Delta_velocity_245[(Delta_velocity_245 == -9999)] <- maxValue(Delta_velocity_245)
Intensity_245 <- as.data.frame(Intensity_245)
Velocity_245 <- as.data.frame(Velocity_245)
Delta_local_245 <- as.data.frame(Delta_local_245)
Delta_velocity_245 <- as.data.frame(Delta_velocity_245)
Local_exposure_245 <- as.data.frame(Local_exposure_245)
Forward_exposure_245 <- as.data.frame(Forward_exposure_245)
cor.test(Intensity_245$Local_CC_intensity_ssp_245_mean_model_2axes_laea, Velocity_245$Velocity_ssp_245_mean_model_0.45, method = "kendall")
cor.test(Intensity_245$Local_CC_intensity_ssp_245_mean_model_2axes_laea, Delta_local_245$LUH2_Delta_2090.1985_ssp_245_artificialness_laea, method = "kendall")
cor.test(Intensity_245$Local_CC_intensity_ssp_245_mean_model_2axes_laea, Delta_velocity_245$Delta_artificialness_velocity_2090_ssp_245_mean_model_0.45, method = "kendall")
cor.test(Velocity_245$Velocity_ssp_245_mean_model_0.45, Delta_local_245$LUH2_Delta_2090.1985_ssp_245_artificialness_laea, method = "kendall")
cor.test(Velocity_245$Velocity_ssp_245_mean_model_0.45, Delta_velocity_245$Delta_artificialness_velocity_2090_ssp_245_mean_model_0.45, method = "kendall")
cor.test(Delta_local_245$LUH2_Delta_2090.1985_ssp_245_artificialness_laea, Delta_velocity_245$Delta_artificialness_velocity_2090_ssp_245_mean_model_0.45, method = "kendall")
cor.test(Local_exposure_245$Local_exposure_ssp_245_mean_model, Forward_exposure_245$Forward_exposure_ssp_245_mean_model, method = "kendall")

### SSP 370
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/KBA_IBA_40_15m_laea.tif")
Intensity_370 <- mask(raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/WORLD_CLIM/BCA/LOCAL_INTENSITY/Local_CC_intensity_ssp_370_mean_model_2axes_laea.tif"), KBA)
Velocity_370 <- mask(raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/WORLD_CLIM/BCA/FORWARD_VELOCITY/Velocity_ssp_370_mean_model_0.45.tif"), KBA)
Delta_local_370 <- mask(raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/LUH2/DELTA_LOCAL/ARTIFICIALNESS/LUH2_Delta_2090-1985_ssp_370_artificialness_laea.tif"), KBA)
Delta_velocity_370 <- mask(raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/LUH2/DELTA_VELOCITY/Delta_artificialness_velocity_2090_ssp_370_mean_model_0.45.tif"), KBA)
Local_exposure_370 <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/EXPOSURE_INDEX/Local_exposure_ssp_370_mean_model.tif")
Forward_exposure_370 <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/EXPOSURE_INDEX/Forward_exposure_ssp_370_mean_model.tif")
Velocity_370[(Velocity_370 == -9999)] <- maxValue(Velocity_370)
Delta_velocity_370[(Delta_velocity_370 == -9999)] <- maxValue(Delta_velocity_370)
Intensity_370 <- as.data.frame(Intensity_370)
Velocity_370 <- as.data.frame(Velocity_370)
Delta_local_370 <- as.data.frame(Delta_local_370)
Delta_velocity_370 <- as.data.frame(Delta_velocity_370)
Local_exposure_370 <- as.data.frame(Local_exposure_370)
Forward_exposure_370 <- as.data.frame(Forward_exposure_370)
cor.test(Intensity_370$Local_CC_intensity_ssp_370_mean_model_2axes_laea, Velocity_370$Velocity_ssp_370_mean_model_0.45, method = "kendall")
cor.test(Intensity_370$Local_CC_intensity_ssp_370_mean_model_2axes_laea, Delta_local_370$LUH2_Delta_2090.1985_ssp_370_artificialness_laea, method = "kendall")
cor.test(Intensity_370$Local_CC_intensity_ssp_370_mean_model_2axes_laea, Delta_velocity_370$Delta_artificialness_velocity_2090_ssp_370_mean_model_0.45, method = "kendall")
cor.test(Velocity_370$Velocity_ssp_370_mean_model_0.45, Delta_local_370$LUH2_Delta_2090.1985_ssp_370_artificialness_laea, method = "kendall")
cor.test(Velocity_370$Velocity_ssp_370_mean_model_0.45, Delta_velocity_370$Delta_artificialness_velocity_2090_ssp_370_mean_model_0.45, method = "kendall")
cor.test(Delta_local_370$LUH2_Delta_2090.1985_ssp_370_artificialness_laea, Delta_velocity_370$Delta_artificialness_velocity_2090_ssp_370_mean_model_0.45, method = "kendall")
cor.test(Local_exposure_370$Local_exposure_ssp_370_mean_model, Forward_exposure_370$Forward_exposure_ssp_370_mean_model, method = "kendall")

### SSP 585
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/KBA_IBA_40_15m_laea.tif")
Intensity_585 <- mask(raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/WORLD_CLIM/BCA/LOCAL_INTENSITY/Local_CC_intensity_ssp_585_mean_model_2axes_laea.tif"), KBA)
Velocity_585 <- mask(raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/WORLD_CLIM/BCA/FORWARD_VELOCITY/Velocity_ssp_585_mean_model_0.45.tif"), KBA)
Delta_local_585 <- mask(raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/LUH2/DELTA_LOCAL/ARTIFICIALNESS/LUH2_Delta_2090-1985_ssp_585_artificialness_laea.tif"), KBA)
Delta_velocity_585 <- mask(raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/LUH2/DELTA_VELOCITY/Delta_artificialness_velocity_2090_ssp_585_mean_model_0.45.tif"), KBA)
Local_exposure_585 <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/EXPOSURE_INDEX/Local_exposure_ssp_585_mean_model.tif")
Forward_exposure_585 <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/EXPOSURE_INDEX/Forward_exposure_ssp_585_mean_model.tif")
Velocity_585[(Velocity_585 == -9999)] <- maxValue(Velocity_585)
Delta_velocity_585[(Delta_velocity_585 == -9999)] <- maxValue(Delta_velocity_585)
Intensity_585 <- as.data.frame(Intensity_585)
Velocity_585 <- as.data.frame(Velocity_585)
Delta_local_585 <- as.data.frame(Delta_local_585)
Delta_velocity_585 <- as.data.frame(Delta_velocity_585)
Local_exposure_585 <- as.data.frame(Local_exposure_585)
Forward_exposure_585 <- as.data.frame(Forward_exposure_585)
cor.test(Intensity_585$Local_CC_intensity_ssp_585_mean_model_2axes_laea, Velocity_585$Velocity_ssp_585_mean_model_0.45, method = "kendall")
cor.test(Intensity_585$Local_CC_intensity_ssp_585_mean_model_2axes_laea, Delta_local_585$LUH2_Delta_2090.1985_ssp_585_artificialness_laea, method = "kendall")
cor.test(Intensity_585$Local_CC_intensity_ssp_585_mean_model_2axes_laea, Delta_velocity_585$Delta_artificialness_velocity_2090_ssp_585_mean_model_0.45, method = "kendall")
cor.test(Velocity_585$Velocity_ssp_585_mean_model_0.45, Delta_local_585$LUH2_Delta_2090.1985_ssp_585_artificialness_laea, method = "kendall")
cor.test(Velocity_585$Velocity_ssp_585_mean_model_0.45, Delta_velocity_585$Delta_artificialness_velocity_2090_ssp_585_mean_model_0.45, method = "kendall")
cor.test(Delta_local_585$LUH2_Delta_2090.1985_ssp_585_artificialness_laea, Delta_velocity_585$Delta_artificialness_velocity_2090_ssp_585_mean_model_0.45, method = "kendall")
cor.test(Local_exposure_585$Local_exposure_ssp_585_mean_model, Forward_exposure_585$Forward_exposure_ssp_585_mean_model, method = "kendall")



#### 3.3. DIFFERENCES IN AVERAGE EXPOSURE BETWEEN SCENARIOS ----

### LC
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/KBA_IBA_40_15m_laea.tif")
M126 <- mask(raster("Local_CC_intensity_ssp126_mean_model_2axes_laea.tif"), KBA)
M245 <- mask(raster("Local_CC_intensity_ssp245_mean_model_2axes_laea.tif"), KBA)
M370 <- mask(raster("Local_CC_intensity_ssp370_mean_model_2axes_laea.tif"), KBA)
M585 <- mask(raster("Local_CC_intensity_ssp585_mean_model_2axes_laea.tif"), KBA)
M_126 <- as.data.frame(M126)
M_126 <- drop_na(M_126)
M_245 <- as.data.frame(M245)
M_245 <- drop_na(M_245)
M_370 <- as.data.frame(M370)
M_370 <- drop_na(M_370)
M_585 <- as.data.frame(M585)
M_585 <- drop_na(M_585)
wilcox.test(M_126$Local_CC_intensity_ssp126_mean_model_2axes_laea, M_245$Local_CC_intensity_ssp245_mean_model_2axes_laea, paired = TRUE, conf.int = TRUE)
wilcox.test(M_126$Local_CC_intensity_ssp126_mean_model_2axes_laea, M_370$Local_CC_intensity_ssp370_mean_model_2axes_laea, paired = TRUE, conf.int = TRUE)
wilcox.test(M_126$Local_CC_intensity_ssp126_mean_model_2axes_laea, M_585$Local_CC_intensity_ssp585_mean_model_2axes_laea, paired = TRUE, conf.int = TRUE)
wilcox.test(M_245$Local_CC_intensity_ssp245_mean_model_2axes_laea, M_370$Local_CC_intensity_ssp370_mean_model_2axes_laea, paired = TRUE, conf.int = TRUE)
wilcox.test(M_245$Local_CC_intensity_ssp245_mean_model_2axes_laea, M_585$Local_CC_intensity_ssp585_mean_model_2axes_laea, paired = TRUE, conf.int = TRUE)
wilcox.test(M_370$Local_CC_intensity_ssp370_mean_model_2axes_laea, M_585$Local_CC_intensity_ssp585_mean_model_2axes_laea, paired = TRUE, conf.int = TRUE)

### RC
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/KBA_IBA_40_15m_laea.tif")
M126 <- mask(raster("Velocity_ssp126_mean_model_0.45.tif"), KBA)
M245 <- mask(raster("Velocity_ssp245_mean_model_0.45.tif"), KBA)
M370 <- mask(raster("Velocity_ssp370_mean_model_0.45.tif"), KBA)
M585 <- mask(raster("Velocity_ssp585_mean_model_0.45.tif"), KBA)
M126_no <- M126
M126_no[(M126 < 0) | (M245 < 0) | (M370 < 0) | (M585 < 0)] <- NA
M245_no <- M245
M245_no[(M126 < 0) | (M245 < 0) | (M370 < 0) | (M585 < 0)] <- NA
M370_no <- M370
M370_no[(M126 < 0) | (M245 < 0) | (M370 < 0) | (M585 < 0)] <- NA
M585_no <- M585
M585_no[(M126 < 0) | (M245 < 0) | (M370 < 0) | (M585 < 0)] <- NA
M_126 <- as.data.frame(M126_no)
M_126 <- drop_na(M_126)
M_245 <- as.data.frame(M245_no)
M_245 <- drop_na(M_245)
M_370 <- as.data.frame(M370_no)
M_370 <- drop_na(M_370)
M_585 <- as.data.frame(M585_no)
M_585 <- drop_na(M_585)
wilcox.test(M_126$Velocity_ssp126_mean_model_0.45, M_245$Velocity_ssp245_mean_model_0.45, paired = TRUE, conf.int = TRUE)
wilcox.test(M_126$Velocity_ssp126_mean_model_0.45, M_370$Velocity_ssp370_mean_model_0.45, paired = TRUE, conf.int = TRUE)
wilcox.test(M_126$Velocity_ssp126_mean_model_0.45, M_585$Velocity_ssp585_mean_model_0.45, paired = TRUE, conf.int = TRUE)
wilcox.test(M_245$Velocity_ssp245_mean_model_0.45, M_370$Velocity_ssp370_mean_model_0.45, paired = TRUE, conf.int = TRUE)
wilcox.test(M_245$Velocity_ssp245_mean_model_0.45, M_585$Velocity_ssp585_mean_model_0.45, paired = TRUE, conf.int = TRUE)
wilcox.test(M_370$Velocity_ssp370_mean_model_0.45, M_585$Velocity_ssp585_mean_model_0.45, paired = TRUE, conf.int = TRUE)

### LA
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/KBA_IBA_40_15m_laea.tif")
M126 <- mask(raster("LUH2_Delta_2090-1985_ssp126_artificialness_laea.tif"), KBA)
M245 <- mask(raster("LUH2_Delta_2090-1985_ssp245_artificialness_laea.tif"), KBA)
M370 <- mask(raster("LUH2_Delta_2090-1985_ssp370_artificialness_laea.tif"), KBA)
M585 <- mask(raster("LUH2_Delta_2090-1985_ssp585_artificialness_laea.tif"), KBA)
M_126 <- as.data.frame(M126)
M_126 <- drop_na(M_126)
M_245 <- as.data.frame(M245)
M_245 <- drop_na(M_245)
M_370 <- as.data.frame(M370)
M_370 <- drop_na(M_370)
M_585 <- as.data.frame(M585)
M_585 <- drop_na(M_585)
wilcox.test(M_126$LUH2_Delta_2090.1985_ssp126_artificialness_laea, M_245$LUH2_Delta_2090.1985_ssp245_artificialness_laea, paired = TRUE, conf.int = TRUE)
wilcox.test(M_126$LUH2_Delta_2090.1985_ssp126_artificialness_laea, M_370$LUH2_Delta_2090.1985_ssp370_artificialness_laea, paired = TRUE, conf.int = TRUE)
wilcox.test(M_126$LUH2_Delta_2090.1985_ssp126_artificialness_laea, M_585$LUH2_Delta_2090.1985_ssp585_artificialness_laea, paired = TRUE, conf.int = TRUE)
wilcox.test(M_245$LUH2_Delta_2090.1985_ssp245_artificialness_laea, M_370$LUH2_Delta_2090.1985_ssp370_artificialness_laea, paired = TRUE, conf.int = TRUE)
wilcox.test(M_245$LUH2_Delta_2090.1985_ssp245_artificialness_laea, M_585$LUH2_Delta_2090.1985_ssp585_artificialness_laea, paired = TRUE, conf.int = TRUE)
wilcox.test(M_370$LUH2_Delta_2090.1985_ssp370_artificialness_laea, M_585$LUH2_Delta_2090.1985_ssp585_artificialness_laea, paired = TRUE, conf.int = TRUE)

### RA
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/KBA_IBA_40_15m_laea.tif")
M126 <- mask(raster("Delta_artificialness_velocity_2090_ssp126_mean_model_0.45.tif"), KBA)
M245 <- mask(raster("Delta_artificialness_velocity_2090_ssp245_mean_model_0.45.tif"), KBA)
M370 <- mask(raster("Delta_artificialness_velocity_2090_ssp370_mean_model_0.45.tif"), KBA)
M585 <- mask(raster("Delta_artificialness_velocity_2090_ssp585_mean_model_0.45.tif"), KBA)
M126_no <- M126
M126_no[(M126 == -9999) | (M245 == -9999) | (M370 == -9999) | (M585 == -9999)] <- NA
M245_no <- M245
M245_no[(M126 == -9999) | (M245 == -9999) | (M370 == -9999) | (M585 == -9999)] <- NA
M370_no <- M370
M370_no[(M126 == -9999) | (M245 == -9999) | (M370 == -9999) | (M585 == -9999)] <- NA
M585_no <- M585
M585_no[(M126 == -9999) | (M245 == -9999) | (M370 == -9999) | (M585 == -9999)] <- NA
M_126 <- as.data.frame(M126_no)
M_126 <- drop_na(M_126)
M_245 <- as.data.frame(M245_no)
M_245 <- drop_na(M_245)
M_370 <- as.data.frame(M370_no)
M_370 <- drop_na(M_370)
M_585 <- as.data.frame(M585_no)
M_585 <- drop_na(M_585)
wilcox.test(M_126$Delta_artificialness_velocity_2090_ssp126_mean_model_0.45, M_245$Delta_artificialness_velocity_2090_ssp245_mean_model_0.45, paired = TRUE, conf.int = TRUE)
wilcox.test(M_126$Delta_artificialness_velocity_2090_ssp126_mean_model_0.45, M_370$Delta_artificialness_velocity_2090_ssp370_mean_model_0.45, paired = TRUE, conf.int = TRUE)
wilcox.test(M_126$Delta_artificialness_velocity_2090_ssp126_mean_model_0.45, M_585$Delta_artificialness_velocity_2090_ssp585_mean_model_0.45, paired = TRUE, conf.int = TRUE)
wilcox.test(M_245$Delta_artificialness_velocity_2090_ssp245_mean_model_0.45, M_370$Delta_artificialness_velocity_2090_ssp370_mean_model_0.45, paired = TRUE, conf.int = TRUE)
wilcox.test(M_245$Delta_artificialness_velocity_2090_ssp245_mean_model_0.45, M_585$Delta_artificialness_velocity_2090_ssp585_mean_model_0.45, paired = TRUE, conf.int = TRUE)
wilcox.test(M_370$Delta_artificialness_velocity_2090_ssp370_mean_model_0.45, M_585$Delta_artificialness_velocity_2090_ssp585_mean_model_0.45, paired = TRUE, conf.int = TRUE)



#### 3.4. DIFFERENCES IN GEOGRAPHIC PATTERNS OF EXPOSURE BETWEEN SCENARIOS ----

### LC
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/KBA_IBA_40_15m_laea.tif")
M126 <- mask(raster("Local_CC_intensity_ssp126_mean_model_2axes_laea.tif"), KBA)
M245 <- mask(raster("Local_CC_intensity_ssp245_mean_model_2axes_laea.tif"), KBA)
M370 <- mask(raster("Local_CC_intensity_ssp370_mean_model_2axes_laea.tif"), KBA)
M585 <- mask(raster("Local_CC_intensity_ssp585_mean_model_2axes_laea.tif"), KBA)
M_126 <- as.data.frame(M126)
M_126 <- drop_na(M_126)
M_245 <- as.data.frame(M245)
M_245 <- drop_na(M_245)
M_370 <- as.data.frame(M370)
M_370 <- drop_na(M_370)
M_585 <- as.data.frame(M585)
M_585 <- drop_na(M_585)
cor.test(M_126$Local_CC_intensity_ssp126_mean_model_2axes_laea, M_245$Local_CC_intensity_ssp245_mean_model_2axes_laea, method = "kendall")
cor.test(M_126$Local_CC_intensity_ssp126_mean_model_2axes_laea, M_370$Local_CC_intensity_ssp370_mean_model_2axes_laea, method = "kendall")
cor.test(M_126$Local_CC_intensity_ssp126_mean_model_2axes_laea, M_585$Local_CC_intensity_ssp585_mean_model_2axes_laea, method = "kendall")
cor.test(M_245$Local_CC_intensity_ssp245_mean_model_2axes_laea, M_370$Local_CC_intensity_ssp370_mean_model_2axes_laea, method = "kendall")
cor.test(M_245$Local_CC_intensity_ssp245_mean_model_2axes_laea, M_585$Local_CC_intensity_ssp585_mean_model_2axes_laea, method = "kendall")
cor.test(M_370$Local_CC_intensity_ssp370_mean_model_2axes_laea, M_585$Local_CC_intensity_ssp585_mean_model_2axes_laea, method = "kendall")

### RC
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/KBA_IBA_40_15m_laea.tif")
M126 <- mask(raster("Velocity_ssp126_mean_model_0.45.tif"), KBA)
M245 <- mask(raster("Velocity_ssp245_mean_model_0.45.tif"), KBA)
M370 <- mask(raster("Velocity_ssp370_mean_model_0.45.tif"), KBA)
M585 <- mask(raster("Velocity_ssp585_mean_model_0.45.tif"), KBA)
M126_no <- M126
M126_no[(M126 < 0) | (M245 < 0) | (M370 < 0) | (M585 < 0)] <- NA
M245_no <- M245
M245_no[(M126 < 0) | (M245 < 0) | (M370 < 0) | (M585 < 0)] <- NA
M370_no <- M370
M370_no[(M126 < 0) | (M245 < 0) | (M370 < 0) | (M585 < 0)] <- NA
M585_no <- M585
M585_no[(M126 < 0) | (M245 < 0) | (M370 < 0) | (M585 < 0)] <- NA
M_126 <- as.data.frame(M126_no)
M_126 <- drop_na(M_126)
M_245 <- as.data.frame(M245_no)
M_245 <- drop_na(M_245)
M_370 <- as.data.frame(M370_no)
M_370 <- drop_na(M_370)
M_585 <- as.data.frame(M585_no)
M_585 <- drop_na(M_585)
cor.test(M_126$Velocity_ssp126_mean_model_0.45, M_245$Velocity_ssp245_mean_model_0.45, method = "kendall")
cor.test(M_126$Velocity_ssp126_mean_model_0.45, M_370$Velocity_ssp370_mean_model_0.45, method = "kendall")
cor.test(M_126$Velocity_ssp126_mean_model_0.45, M_585$Velocity_ssp585_mean_model_0.45, method = "kendall")
cor.test(M_245$Velocity_ssp245_mean_model_0.45, M_370$Velocity_ssp370_mean_model_0.45, method = "kendall")
cor.test(M_245$Velocity_ssp245_mean_model_0.45, M_585$Velocity_ssp585_mean_model_0.45, method = "kendall")
cor.test(M_370$Velocity_ssp370_mean_model_0.45, M_585$Velocity_ssp585_mean_model_0.45, method = "kendall")

### LA
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/KBA_IBA_40_15m_laea.tif")
M126 <- mask(raster("LUH2_Delta_2090-1985_ssp126_artificialness_laea.tif"), KBA)
M245 <- mask(raster("LUH2_Delta_2090-1985_ssp245_artificialness_laea.tif"), KBA)
M370 <- mask(raster("LUH2_Delta_2090-1985_ssp370_artificialness_laea.tif"), KBA)
M585 <- mask(raster("LUH2_Delta_2090-1985_ssp585_artificialness_laea.tif"), KBA)
M_126 <- as.data.frame(M126)
M_126 <- drop_na(M_126)
M_245 <- as.data.frame(M245)
M_245 <- drop_na(M_245)
M_370 <- as.data.frame(M370)
M_370 <- drop_na(M_370)
M_585 <- as.data.frame(M585)
M_585 <- drop_na(M_585)
cor.test(M_126$LUH2_Delta_2090.1985_ssp126_artificialness_laea, M_245$LUH2_Delta_2090.1985_ssp245_artificialness_laea, method = "kendall")
cor.test(M_126$LUH2_Delta_2090.1985_ssp126_artificialness_laea, M_370$LUH2_Delta_2090.1985_ssp370_artificialness_laea, method = "kendall")
cor.test(M_126$LUH2_Delta_2090.1985_ssp126_artificialness_laea, M_585$LUH2_Delta_2090.1985_ssp585_artificialness_laea, method = "kendall")
cor.test(M_245$LUH2_Delta_2090.1985_ssp245_artificialness_laea, M_370$LUH2_Delta_2090.1985_ssp370_artificialness_laea, method = "kendall")
cor.test(M_245$LUH2_Delta_2090.1985_ssp245_artificialness_laea, M_585$LUH2_Delta_2090.1985_ssp585_artificialness_laea, method = "kendall")
cor.test(M_370$LUH2_Delta_2090.1985_ssp370_artificialness_laea, M_585$LUH2_Delta_2090.1985_ssp585_artificialness_laea, method = "kendall")

### RA
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/KBA_IBA_40_15m_laea.tif")
M126 <- mask(raster("Delta_artificialness_velocity_2090_ssp126_mean_model_0.45.tif"), KBA)
M245 <- mask(raster("Delta_artificialness_velocity_2090_ssp245_mean_model_0.45.tif"), KBA)
M370 <- mask(raster("Delta_artificialness_velocity_2090_ssp370_mean_model_0.45.tif"), KBA)
M585 <- mask(raster("Delta_artificialness_velocity_2090_ssp585_mean_model_0.45.tif"), KBA)
M126_no <- M126
M126_no[(M126 == -9999) | (M245 == -9999) | (M370 == -9999) | (M585 == -9999)] <- NA
M245_no <- M245
M245_no[(M126 == -9999) | (M245 == -9999) | (M370 == -9999) | (M585 == -9999)] <- NA
M370_no <- M370
M370_no[(M126 == -9999) | (M245 == -9999) | (M370 == -9999) | (M585 == -9999)] <- NA
M585_no <- M585
M585_no[(M126 == -9999) | (M245 == -9999) | (M370 == -9999) | (M585 == -9999)] <- NA
M_126 <- as.data.frame(M126_no)
M_126 <- drop_na(M_126)
M_245 <- as.data.frame(M245_no)
M_245 <- drop_na(M_245)
M_370 <- as.data.frame(M370_no)
M_370 <- drop_na(M_370)
M_585 <- as.data.frame(M585_no)
M_585 <- drop_na(M_585)
cor.test(M_126$Delta_artificialness_velocity_2090_ssp126_mean_model_0.45, M_245$Delta_artificialness_velocity_2090_ssp245_mean_model_0.45, method = "kendall")
cor.test(M_126$Delta_artificialness_velocity_2090_ssp126_mean_model_0.45, M_370$Delta_artificialness_velocity_2090_ssp370_mean_model_0.45, method = "kendall")
cor.test(M_126$Delta_artificialness_velocity_2090_ssp126_mean_model_0.45, M_585$Delta_artificialness_velocity_2090_ssp585_mean_model_0.45, method = "kendall")
cor.test(M_245$Delta_artificialness_velocity_2090_ssp245_mean_model_0.45, M_370$Delta_artificialness_velocity_2090_ssp370_mean_model_0.45, method = "kendall")
cor.test(M_245$Delta_artificialness_velocity_2090_ssp245_mean_model_0.45, M_585$Delta_artificialness_velocity_2090_ssp585_mean_model_0.45, method = "kendall")
cor.test(M_370$Delta_artificialness_velocity_2090_ssp370_mean_model_0.45, M_585$Delta_artificialness_velocity_2090_ssp585_mean_model_0.45, method = "kendall")

### EI
M126 <- raster("Exposure_index_ssp126_mean_model.tif")
M245 <- raster("Exposure_index_ssp245_mean_model.tif")
M370 <- raster("Exposure_index_ssp370_mean_model.tif")
M585 <- raster("Exposure_index_ssp585_mean_model.tif")
M_126 <- as.data.frame(M126)
M_126 <- drop_na(M_126)
M_245 <- as.data.frame(M245)
M_245 <- drop_na(M_245)
M_370 <- as.data.frame(M370)
M_370 <- drop_na(M_370)
M_585 <- as.data.frame(M585)
M_585 <- drop_na(M_585)
cor.test(M_126$Exposure_index_ssp126_mean_model, M_245$Exposure_index_ssp245_mean_model, method = "kendall")
cor.test(M_126$Exposure_index_ssp126_mean_model, M_370$Exposure_index_ssp370_mean_model, method = "kendall")
cor.test(M_126$Exposure_index_ssp126_mean_model, M_585$Exposure_index_ssp585_mean_model, method = "kendall")
cor.test(M_245$Exposure_index_ssp245_mean_model, M_370$Exposure_index_ssp370_mean_model, method = "kendall")
cor.test(M_245$Exposure_index_ssp245_mean_model, M_585$Exposure_index_ssp585_mean_model, method = "kendall")
cor.test(M_370$Exposure_index_ssp370_mean_model, M_585$Exposure_index_ssp585_mean_model, method = "kendall")

### LEI
M126 <- raster("Local_exposure_ssp126_mean_model.tif")
M245 <- raster("Local_exposure_ssp245_mean_model.tif")
M370 <- raster("Local_exposure_ssp370_mean_model.tif")
M585 <- raster("Local_exposure_ssp585_mean_model.tif")
M_126 <- as.data.frame(M126)
M_126 <- drop_na(M_126)
M_245 <- as.data.frame(M245)
M_245 <- drop_na(M_245)
M_370 <- as.data.frame(M370)
M_370 <- drop_na(M_370)
M_585 <- as.data.frame(M585)
M_585 <- drop_na(M_585)
cor.test(M_126$Local_exposure_ssp126_mean_model, M_245$Local_exposure_ssp245_mean_model, method = "kendall")
cor.test(M_126$Local_exposure_ssp126_mean_model, M_370$Local_exposure_ssp370_mean_model, method = "kendall")
cor.test(M_126$Local_exposure_ssp126_mean_model, M_585$Local_exposure_ssp585_mean_model, method = "kendall")
cor.test(M_245$Local_exposure_ssp245_mean_model, M_370$Local_exposure_ssp370_mean_model, method = "kendall")
cor.test(M_245$Local_exposure_ssp245_mean_model, M_585$Local_exposure_ssp585_mean_model, method = "kendall")
cor.test(M_370$Local_exposure_ssp370_mean_model, M_585$Local_exposure_ssp585_mean_model, method = "kendall")

### REI
M126 <- raster("Forward_exposure_ssp126_mean_model.tif")
M245 <- raster("Forward_exposure_ssp245_mean_model.tif")
M370 <- raster("Forward_exposure_ssp370_mean_model.tif")
M585 <- raster("Forward_exposure_ssp585_mean_model.tif")
M_126 <- as.data.frame(M126)
M_126 <- drop_na(M_126)
M_245 <- as.data.frame(M245)
M_245 <- drop_na(M_245)
M_370 <- as.data.frame(M370)
M_370 <- drop_na(M_370)
M_585 <- as.data.frame(M585)
M_585 <- drop_na(M_585)
cor.test(M_126$Forward_exposure_ssp126_mean_model, M_245$Forward_exposure_ssp245_mean_model, method = "kendall")
cor.test(M_126$Forward_exposure_ssp126_mean_model, M_370$Forward_exposure_ssp370_mean_model, method = "kendall")
cor.test(M_126$Forward_exposure_ssp126_mean_model, M_585$Forward_exposure_ssp585_mean_model, method = "kendall")
cor.test(M_245$Forward_exposure_ssp245_mean_model, M_370$Forward_exposure_ssp370_mean_model, method = "kendall")
cor.test(M_245$Forward_exposure_ssp245_mean_model, M_585$Forward_exposure_ssp585_mean_model, method = "kendall")
cor.test(M_370$Forward_exposure_ssp370_mean_model, M_585$Forward_exposure_ssp585_mean_model, method = "kendall")



#### 3.5. EXPOSURE OF NON-PROTECTED AREAS ----

### LC
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/KBA_IBA_40_15m_laea.tif")
M126 <- mask(raster("Local_CC_intensity_ssp126_mean_model_2axes_laea.tif"), KBA)
M245 <- mask(raster("Local_CC_intensity_ssp245_mean_model_2axes_laea.tif"), KBA)
M370 <- mask(raster("Local_CC_intensity_ssp370_mean_model_2axes_laea.tif"), KBA)
M585 <- mask(raster("Local_CC_intensity_ssp585_mean_model_2axes_laea.tif"), KBA)
PA <- mask(raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/PA_I_IV_15m_laea.tif"), KBA)
M_126 <- as.data.frame(M126)
M_126 <- drop_na(M_126)
M_245 <- as.data.frame(M245)
M_245 <- drop_na(M_245)
M_370 <- as.data.frame(M370)
M_370 <- drop_na(M_370)
M_585 <- as.data.frame(M585)
M_585 <- drop_na(M_585)
Data_PA <- as.data.frame(PA)
Data_PA <- drop_na(Data_PA)
M126_nPA <- M126
M126_nPA[PA > 0] <- NA
M_126_nPA <- drop_na(as.data.frame(M126_nPA))
M126_PA <- M126
M126_PA[PA == 0] <- NA
M_126_PA <- drop_na(as.data.frame(M126_PA))
M245_nPA <- M245
M245_nPA[PA > 0] <- NA
M_245_nPA <- drop_na(as.data.frame(M245_nPA))
M245_PA <- M245
M245_PA[PA == 0] <- NA
M_245_PA <- drop_na(as.data.frame(M245_PA))
M370_nPA <- M370
M370_nPA[PA > 0] <- NA
M_370_nPA <- drop_na(as.data.frame(M370_nPA))
M370_PA <- M370
M370_PA[PA == 0] <- NA
M_370_PA <- drop_na(as.data.frame(M370_PA))
M585_nPA <- M585
M585_nPA[PA > 0] <- NA
M_585_nPA <- drop_na(as.data.frame(M585_nPA))
M585_PA <- M585
M585_PA[PA == 0] <- NA
M_585_PA <- drop_na(as.data.frame(M585_PA))
wilcox.test(M_126_nPA$Local_CC_intensity_ssp126_mean_model_2axes_laea, M_126_PA$Local_CC_intensity_ssp126_mean_model_2axes_laea, paired = FALSE, conf.int = TRUE)
wilcox.test(M_245_nPA$Local_CC_intensity_ssp245_mean_model_2axes_laea, M_245_PA$Local_CC_intensity_ssp245_mean_model_2axes_laea, paired = FALSE, conf.int = TRUE)
wilcox.test(M_370_nPA$Local_CC_intensity_ssp370_mean_model_2axes_laea, M_370_PA$Local_CC_intensity_ssp370_mean_model_2axes_laea, paired = FALSE, conf.int = TRUE)
wilcox.test(M_585_nPA$Local_CC_intensity_ssp585_mean_model_2axes_laea, M_585_PA$Local_CC_intensity_ssp585_mean_model_2axes_laea, paired = FALSE, conf.int = TRUE)

### RC
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/KBA_IBA_40_15m_laea.tif")
M126 <- mask(raster("Velocity_ssp126_mean_model_0.45.tif"), KBA)
M245 <- mask(raster("Velocity_ssp245_mean_model_0.45.tif"), KBA)
M370 <- mask(raster("Velocity_ssp370_mean_model_0.45.tif"), KBA)
M585 <- mask(raster("Velocity_ssp585_mean_model_0.45.tif"), KBA)
PA <- mask(raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/PA_I_IV_15m_laea.tif"), KBA)
M126_no <- M126
M126_no[(M126 == -9999)] <- maxValue(M126)
M245_no <- M245
M245_no[(M245 == -9999)] <- maxValue(M245)
M370_no <- M370
M370_no[(M370 == -9999)] <- maxValue(M370)
M585_no <- M585
M585_no[(M585 == -9999)] <- maxValue(M585)
M_126 <- as.data.frame(M126_no)
M_126 <- drop_na(M_126)
M_245 <- as.data.frame(M245_no)
M_245 <- drop_na(M_245)
M_370 <- as.data.frame(M370_no)
M_370 <- drop_na(M_370)
M_585 <- as.data.frame(M585_no)
M_585 <- drop_na(M_585)
Data_PA <- as.data.frame(PA)
Data_PA <- drop_na(Data_PA)
M126_nPA <- M126_no
M126_nPA[PA > 0] <- NA
M_126_nPA <- drop_na(as.data.frame(M126_nPA))
M126_PA <- M126_no
M126_PA[PA == 0] <- NA
M_126_PA <- drop_na(as.data.frame(M126_PA))
M245_nPA <- M245_no
M245_nPA[PA > 0] <- NA
M_245_nPA <- drop_na(as.data.frame(M245_nPA))
M245_PA <- M245_no
M245_PA[PA == 0] <- NA
M_245_PA <- drop_na(as.data.frame(M245_PA))
M370_nPA <- M370_no
M370_nPA[PA > 0] <- NA
M_370_nPA <- drop_na(as.data.frame(M370_nPA))
M370_PA <- M370_no
M370_PA[PA == 0] <- NA
M_370_PA <- drop_na(as.data.frame(M370_PA))
M585_nPA <- M585_no
M585_nPA[PA > 0] <- NA
M_585_nPA <- drop_na(as.data.frame(M585_nPA))
M585_PA <- M585_no
M585_PA[PA == 0] <- NA
M_585_PA <- drop_na(as.data.frame(M585_PA))
wilcox.test(M_126_nPA$Velocity_ssp126_mean_model_0.45, M_126_PA$Velocity_ssp126_mean_model_0.45, paired = FALSE, conf.int = TRUE)
wilcox.test(M_245_nPA$Velocity_ssp245_mean_model_0.45, M_245_PA$Velocity_ssp245_mean_model_0.45, paired = FALSE, conf.int = TRUE)
wilcox.test(M_370_nPA$Velocity_ssp370_mean_model_0.45, M_370_PA$Velocity_ssp370_mean_model_0.45, paired = FALSE, conf.int = TRUE)
wilcox.test(M_585_nPA$Velocity_ssp585_mean_model_0.45, M_585_PA$Velocity_ssp585_mean_model_0.45, paired = FALSE, conf.int = TRUE)

### LA
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/KBA_IBA_40_15m_laea.tif")
M126 <- mask(raster("LUH2_Delta_2090-1985_ssp126_artificialness_laea.tif"), KBA)
M245 <- mask(raster("LUH2_Delta_2090-1985_ssp245_artificialness_laea.tif"), KBA)
M370 <- mask(raster("LUH2_Delta_2090-1985_ssp370_artificialness_laea.tif"), KBA)
M585 <- mask(raster("LUH2_Delta_2090-1985_ssp585_artificialness_laea.tif"), KBA)
PA <- mask(raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/PA_I_IV_15m_laea.tif"), KBA)
M_126 <- as.data.frame(M126)
M_126 <- drop_na(M_126)
M_245 <- as.data.frame(M245)
M_245 <- drop_na(M_245)
M_370 <- as.data.frame(M370)
M_370 <- drop_na(M_370)
M_585 <- as.data.frame(M585)
M_585 <- drop_na(M_585)
Data_PA <- as.data.frame(PA)
Data_PA <- drop_na(Data_PA)
M126_nPA <- M126
M126_nPA[PA > 0] <- NA
M_126_nPA <- drop_na(as.data.frame(M126_nPA))
M126_PA <- M126
M126_PA[PA == 0] <- NA
M_126_PA <- drop_na(as.data.frame(M126_PA))
M245_nPA <- M245
M245_nPA[PA > 0] <- NA
M_245_nPA <- drop_na(as.data.frame(M245_nPA))
M245_PA <- M245
M245_PA[PA == 0] <- NA
M_245_PA <- drop_na(as.data.frame(M245_PA))
M370_nPA <- M370
M370_nPA[PA > 0] <- NA
M_370_nPA <- drop_na(as.data.frame(M370_nPA))
M370_PA <- M370
M370_PA[PA == 0] <- NA
M_370_PA <- drop_na(as.data.frame(M370_PA))
M585_nPA <- M585
M585_nPA[PA > 0] <- NA
M_585_nPA <- drop_na(as.data.frame(M585_nPA))
M585_PA <- M585
M585_PA[PA == 0] <- NA
M_585_PA <- drop_na(as.data.frame(M585_PA))
wilcox.test(M_126_nPA$LUH2_Delta_2090.1985_ssp126_artificialness_laea, M_126_PA$LUH2_Delta_2090.1985_ssp126_artificialness_laea, paired = FALSE, conf.int = TRUE)
wilcox.test(M_245_nPA$LUH2_Delta_2090.1985_ssp245_artificialness_laea, M_245_PA$LUH2_Delta_2090.1985_ssp245_artificialness_laea, paired = FALSE, conf.int = TRUE)
wilcox.test(M_370_nPA$LUH2_Delta_2090.1985_ssp370_artificialness_laea, M_370_PA$LUH2_Delta_2090.1985_ssp370_artificialness_laea, paired = FALSE, conf.int = TRUE)
wilcox.test(M_585_nPA$LUH2_Delta_2090.1985_ssp585_artificialness_laea, M_585_PA$LUH2_Delta_2090.1985_ssp585_artificialness_laea, paired = FALSE, conf.int = TRUE)

### RA
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/KBA_IBA_40_15m_laea.tif")
M126 <- mask(raster("Delta_artificialness_velocity_2090_ssp126_mean_model_0.45.tif"), KBA)
M245 <- mask(raster("Delta_artificialness_velocity_2090_ssp245_mean_model_0.45.tif"), KBA)
M370 <- mask(raster("Delta_artificialness_velocity_2090_ssp370_mean_model_0.45.tif"), KBA)
M585 <- mask(raster("Delta_artificialness_velocity_2090_ssp585_mean_model_0.45.tif"), KBA)
PA <- mask(raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/PA_I_IV_15m_laea.tif"), KBA)
M126_no <- M126
M126_no[(M126 == -9999)] <- maxValue(M126)
M245_no <- M245
M245_no[(M245 == -9999)] <- maxValue(M245)
M370_no <- M370
M370_no[(M370 == -9999)] <- maxValue(M370)
M585_no <- M585
M585_no[(M585 == -9999)] <- maxValue(M585)
M_126 <- as.data.frame(M126_no)
M_126 <- drop_na(M_126)
M_245 <- as.data.frame(M245_no)
M_245 <- drop_na(M_245)
M_370 <- as.data.frame(M370_no)
M_370 <- drop_na(M_370)
M_585 <- as.data.frame(M585_no)
M_585 <- drop_na(M_585)
Data_PA <- as.data.frame(PA)
Data_PA <- drop_na(Data_PA)
M126_nPA <- M126_no
M126_nPA[PA > 0] <- NA
M_126_nPA <- drop_na(as.data.frame(M126_nPA))
M126_PA <- M126_no
M126_PA[PA == 0] <- NA
M_126_PA <- drop_na(as.data.frame(M126_PA))
M245_nPA <- M245_no
M245_nPA[PA > 0] <- NA
M_245_nPA <- drop_na(as.data.frame(M245_nPA))
M245_PA <- M245_no
M245_PA[PA == 0] <- NA
M_245_PA <- drop_na(as.data.frame(M245_PA))
M370_nPA <- M370_no
M370_nPA[PA > 0] <- NA
M_370_nPA <- drop_na(as.data.frame(M370_nPA))
M370_PA <- M370_no
M370_PA[PA == 0] <- NA
M_370_PA <- drop_na(as.data.frame(M370_PA))
M585_nPA <- M585_no
M585_nPA[PA > 0] <- NA
M_585_nPA <- drop_na(as.data.frame(M585_nPA))
M585_PA <- M585_no
M585_PA[PA == 0] <- NA
M_585_PA <- drop_na(as.data.frame(M585_PA))
wilcox.test(M_126_nPA$Delta_artificialness_velocity_2090_ssp126_mean_model_0.45, M_126_PA$Delta_artificialness_velocity_2090_ssp126_mean_model_0.45, paired = FALSE, conf.int = TRUE)
wilcox.test(M_245_nPA$Delta_artificialness_velocity_2090_ssp245_mean_model_0.45, M_245_PA$Delta_artificialness_velocity_2090_ssp245_mean_model_0.45, paired = FALSE, conf.int = TRUE)
wilcox.test(M_370_nPA$Delta_artificialness_velocity_2090_ssp370_mean_model_0.45, M_370_PA$Delta_artificialness_velocity_2090_ssp370_mean_model_0.45, paired = FALSE, conf.int = TRUE)
wilcox.test(M_585_nPA$Delta_artificialness_velocity_2090_ssp585_mean_model_0.45, M_585_PA$Delta_artificialness_velocity_2090_ssp585_mean_model_0.45, paired = FALSE, conf.int = TRUE)

### EI
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/ARTICLE_1/KBA_IBA_40_15m_laea.tif")
PA <- mask(raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/ARTICLE_1/PA_I_IV_15m_laea.tif"), KBA)
M126 <- raster("Exposure_index_ssp126_mean_model.tif")
M245 <- raster("Exposure_index_ssp245_mean_model.tif")
M370 <- raster("Exposure_index_ssp370_mean_model.tif")
M585 <- raster("Exposure_index_ssp585_mean_model.tif")
M_126 <- as.data.frame(M126)
M_126 <- drop_na(M_126)
M_245 <- as.data.frame(M245)
M_245 <- drop_na(M_245)
M_370 <- as.data.frame(M370)
M_370 <- drop_na(M_370)
M_585 <- as.data.frame(M585)
M_585 <- drop_na(M_585)
Data_PA <- as.data.frame(PA)
Data_PA <- drop_na(Data_PA)
M126_nPA <- M126
M126_nPA[PA > 0] <- NA
M_126_nPA <- drop_na(as.data.frame(M126_nPA))
M126_PA <- M126
M126_PA[PA == 0] <- NA
M_126_PA <- drop_na(as.data.frame(M126_PA))
M245_nPA <- M245
M245_nPA[PA > 0] <- NA
M_245_nPA <- drop_na(as.data.frame(M245_nPA))
M245_PA <- M245
M245_PA[PA == 0] <- NA
M_245_PA <- drop_na(as.data.frame(M245_PA))
M370_nPA <- M370
M370_nPA[PA > 0] <- NA
M_370_nPA <- drop_na(as.data.frame(M370_nPA))
M370_PA <- M370
M370_PA[PA == 0] <- NA
M_370_PA <- drop_na(as.data.frame(M370_PA))
M585_nPA <- M585
M585_nPA[PA > 0] <- NA
M_585_nPA <- drop_na(as.data.frame(M585_nPA))
M585_PA <- M585
M585_PA[PA == 0] <- NA
M_585_PA <- drop_na(as.data.frame(M585_PA))
wilcox.test(M_126_nPA$Exposure_index_ssp126_mean_model, M_126_PA$Exposure_index_ssp126_mean_model, paired = FALSE, conf.int = TRUE)
wilcox.test(M_245_nPA$Exposure_index_ssp245_mean_model, M_245_PA$Exposure_index_ssp245_mean_model, paired = FALSE, conf.int = TRUE)
wilcox.test(M_370_nPA$Exposure_index_ssp370_mean_model, M_370_PA$Exposure_index_ssp370_mean_model, paired = FALSE, conf.int = TRUE)
wilcox.test(M_585_nPA$Exposure_index_ssp585_mean_model, M_585_PA$Exposure_index_ssp585_mean_model, paired = FALSE, conf.int = TRUE)

### LEI
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/KBA_IBA_40_15m_laea.tif")
PA <- mask(raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/PA_I_IV_15m_laea.tif"), KBA)
M126 <- raster("Local_exposure_ssp126_mean_model.tif")
M245 <- raster("Local_exposure_ssp245_mean_model.tif")
M370 <- raster("Local_exposure_ssp370_mean_model.tif")
M585 <- raster("Local_exposure_ssp585_mean_model.tif")
M_126 <- as.data.frame(M126)
M_126 <- drop_na(M_126)
M_245 <- as.data.frame(M245)
M_245 <- drop_na(M_245)
M_370 <- as.data.frame(M370)
M_370 <- drop_na(M_370)
M_585 <- as.data.frame(M585)
M_585 <- drop_na(M_585)
Data_PA <- as.data.frame(PA)
Data_PA <- drop_na(Data_PA)
M126_nPA <- M126
M126_nPA[PA > 0] <- NA
M_126_nPA <- drop_na(as.data.frame(M126_nPA))
M126_PA <- M126
M126_PA[PA == 0] <- NA
M_126_PA <- drop_na(as.data.frame(M126_PA))
M245_nPA <- M245
M245_nPA[PA > 0] <- NA
M_245_nPA <- drop_na(as.data.frame(M245_nPA))
M245_PA <- M245
M245_PA[PA == 0] <- NA
M_245_PA <- drop_na(as.data.frame(M245_PA))
M370_nPA <- M370
M370_nPA[PA > 0] <- NA
M_370_nPA <- drop_na(as.data.frame(M370_nPA))
M370_PA <- M370
M370_PA[PA == 0] <- NA
M_370_PA <- drop_na(as.data.frame(M370_PA))
M585_nPA <- M585
M585_nPA[PA > 0] <- NA
M_585_nPA <- drop_na(as.data.frame(M585_nPA))
M585_PA <- M585
M585_PA[PA == 0] <- NA
M_585_PA <- drop_na(as.data.frame(M585_PA))
wilcox.test(M_126_nPA$Local_exposure_ssp126_mean_model, M_126_PA$Local_exposure_ssp126_mean_model, paired = FALSE, conf.int = TRUE)
wilcox.test(M_245_nPA$Local_exposure_ssp245_mean_model, M_245_PA$Local_exposure_ssp245_mean_model, paired = FALSE, conf.int = TRUE)
wilcox.test(M_370_nPA$Local_exposure_ssp370_mean_model, M_370_PA$Local_exposure_ssp370_mean_model, paired = FALSE, conf.int = TRUE)
wilcox.test(M_585_nPA$Local_exposure_ssp585_mean_model, M_585_PA$Local_exposure_ssp585_mean_model, paired = FALSE, conf.int = TRUE)

### REI
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/KBA_IBA_40_15m_laea.tif")
PA <- mask(raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/PA_I_IV_15m_laea.tif"), KBA)
M126 <- raster("Forward_exposure_ssp126_mean_model.tif")
M245 <- raster("Forward_exposure_ssp245_mean_model.tif")
M370 <- raster("Forward_exposure_ssp370_mean_model.tif")
M585 <- raster("Forward_exposure_ssp585_mean_model.tif")
M_126 <- as.data.frame(M126)
M_126 <- drop_na(M_126)
M_245 <- as.data.frame(M245)
M_245 <- drop_na(M_245)
M_370 <- as.data.frame(M370)
M_370 <- drop_na(M_370)
M_585 <- as.data.frame(M585)
M_585 <- drop_na(M_585)
Data_PA <- as.data.frame(PA)
Data_PA <- drop_na(Data_PA)
M126_nPA <- M126
M126_nPA[PA > 0] <- NA
M_126_nPA <- drop_na(as.data.frame(M126_nPA))
M126_PA <- M126
M126_PA[PA == 0] <- NA
M_126_PA <- drop_na(as.data.frame(M126_PA))
M245_nPA <- M245
M245_nPA[PA > 0] <- NA
M_245_nPA <- drop_na(as.data.frame(M245_nPA))
M245_PA <- M245
M245_PA[PA == 0] <- NA
M_245_PA <- drop_na(as.data.frame(M245_PA))
M370_nPA <- M370
M370_nPA[PA > 0] <- NA
M_370_nPA <- drop_na(as.data.frame(M370_nPA))
M370_PA <- M370
M370_PA[PA == 0] <- NA
M_370_PA <- drop_na(as.data.frame(M370_PA))
M585_nPA <- M585
M585_nPA[PA > 0] <- NA
M_585_nPA <- drop_na(as.data.frame(M585_nPA))
M585_PA <- M585
M585_PA[PA == 0] <- NA
M_585_PA <- drop_na(as.data.frame(M585_PA))
wilcox.test(M_126_nPA$Forward_exposure_ssp126_mean_model, M_126_PA$Forward_exposure_ssp126_mean_model, paired = FALSE, conf.int = TRUE)
wilcox.test(M_245_nPA$Forward_exposure_ssp245_mean_model, M_245_PA$Forward_exposure_ssp245_mean_model, paired = FALSE, conf.int = TRUE)
wilcox.test(M_370_nPA$Forward_exposure_ssp370_mean_model, M_370_PA$Forward_exposure_ssp370_mean_model, paired = FALSE, conf.int = TRUE)
wilcox.test(M_585_nPA$Forward_exposure_ssp585_mean_model, M_585_PA$Forward_exposure_ssp585_mean_model, paired = FALSE, conf.int = TRUE)



#### CORRELATION BETWEEN ELEVATION AND EXPOSURE ----

### LC
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/KBA_IBA_40_15m_laea.tif")
M126 <- mask(raster("Local_CC_intensity_ssp126_mean_model_2axes_laea.tif"), KBA)
M245 <- mask(raster("Local_CC_intensity_ssp245_mean_model_2axes_laea.tif"), KBA)
M370 <- mask(raster("Local_CC_intensity_ssp370_mean_model_2axes_laea.tif"), KBA)
M585 <- mask(raster("Local_CC_intensity_ssp585_mean_model_2axes_laea.tif"), KBA)
SRTM <- mask(raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/SRTM/wc2.1_15m_elev_laea.tif"), KBA)
M_126 <- as.data.frame(M126)
M_126 <- drop_na(M_126)
M_245 <- as.data.frame(M245)
M_245 <- drop_na(M_245)
M_370 <- as.data.frame(M370)
M_370 <- drop_na(M_370)
M_585 <- as.data.frame(M585)
M_585 <- drop_na(M_585)
M_SRTM <- as.data.frame(SRTM)
M_SRTM <- drop_na(M_SRTM)
cor.test(M_SRTM$wc2.1_15m_elev_laea, M_126$Local_CC_intensity_ssp126_mean_model_2axes_laea, method = "kendall")
cor.test(M_SRTM$wc2.1_15m_elev_laea, M_245$Local_CC_intensity_ssp245_mean_model_2axes_laea, method = "kendall")
cor.test(M_SRTM$wc2.1_15m_elev_laea, M_370$Local_CC_intensity_ssp370_mean_model_2axes_laea, method = "kendall")
cor.test(M_SRTM$wc2.1_15m_elev_laea, M_585$Local_CC_intensity_ssp585_mean_model_2axes_laea, method = "kendall")

## RC
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/KBA_IBA_40_15m_laea.tif")
M126 <- mask(raster("Velocity_ssp126_mean_model_0.45.tif"), KBA)
M245 <- mask(raster("Velocity_ssp245_mean_model_0.45.tif"), KBA)
M370 <- mask(raster("Velocity_ssp370_mean_model_0.45.tif"), KBA)
M585 <- mask(raster("Velocity_ssp585_mean_model_0.45.tif"), KBA)
SRTM <- mask(raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/SRTM/wc2.1_15m_elev_laea.tif"), KBA)
M126_no <- M126
M126_no[(M126 == -9999)] <- maxValue(M126)
M245_no <- M245
M245_no[(M245 == -9999)] <- maxValue(M245)
M370_no <- M370
M370_no[(M370 == -9999)] <- maxValue(M370)
M585_no <- M585
M585_no[(M585 == -9999)] <- maxValue(M585)
M_126 <- as.data.frame(M126_no)
M_126 <- drop_na(M_126)
M_245 <- as.data.frame(M245_no)
M_245 <- drop_na(M_245)
M_370 <- as.data.frame(M370_no)
M_370 <- drop_na(M_370)
M_585 <- as.data.frame(M585_no)
M_585 <- drop_na(M_585)
M_SRTM <- as.data.frame(SRTM)
M_SRTM <- drop_na(M_SRTM)
cor.test(M_SRTM$wc2.1_15m_elev_laea, M_126$Velocity_ssp126_mean_model_0.45, method = "kendall")
cor.test(M_SRTM$wc2.1_15m_elev_laea, M_245$Velocity_ssp245_mean_model_0.45, method = "kendall")
cor.test(M_SRTM$wc2.1_15m_elev_laea, M_370$Velocity_ssp370_mean_model_0.45, method = "kendall")
cor.test(M_SRTM$wc2.1_15m_elev_laea, M_585$Velocity_ssp585_mean_model_0.45, method = "kendall")

## LA
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/KBA_IBA_40_15m_laea.tif")
Mhisto <- mask(raster("LUH2_1985_artificialness_laea.tif"), KBA)
M126 <- mask(raster("LUH2_Delta_2090-1985_ssp126_artificialness_laea.tif"), KBA)
M245 <- mask(raster("LUH2_Delta_2090-1985_ssp245_artificialness_laea.tif"), KBA)
M370 <- mask(raster("LUH2_Delta_2090-1985_ssp370_artificialness_laea.tif"), KBA)
M585 <- mask(raster("LUH2_Delta_2090-1985_ssp585_artificialness_laea.tif"), KBA)
SRTM <- mask(raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/SRTM/wc2.1_15m_elev_laea.tif"), KBA)
M_histo <- as.data.frame(Mhisto)
M_histo <- drop_na(M_histo)
M_126 <- as.data.frame(M126)
M_126 <- drop_na(M_126)
M_245 <- as.data.frame(M245)
M_245 <- drop_na(M_245)
M_370 <- as.data.frame(M370)
M_370 <- drop_na(M_370)
M_585 <- as.data.frame(M585)
M_585 <- drop_na(M_585)
M_SRTM <- as.data.frame(SRTM)
M_SRTM <- drop_na(M_SRTM)
cor.test(M_SRTM$wc2.1_15m_elev_laea, M_histo$LUH2_1985_artificialness_laea, method = "kendall")
cor.test(M_SRTM$wc2.1_15m_elev_laea, M_126$LUH2_Delta_2090.1985_ssp126_artificialness_laea, method = "kendall")
cor.test(M_SRTM$wc2.1_15m_elev_laea, M_245$LUH2_Delta_2090.1985_ssp245_artificialness_laea, method = "kendall")
cor.test(M_SRTM$wc2.1_15m_elev_laea, M_370$LUH2_Delta_2090.1985_ssp370_artificialness_laea, method = "kendall")
cor.test(M_SRTM$wc2.1_15m_elev_laea, M_585$LUH2_Delta_2090.1985_ssp585_artificialness_laea, method = "kendall")

## RA
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/KBA_IBA_40_15m_laea.tif")
M126 <- mask(raster("Delta_artificialness_velocity_2090_ssp126_mean_model_0.45.tif"), KBA)
M245 <- mask(raster("Delta_artificialness_velocity_2090_ssp245_mean_model_0.45.tif"), KBA)
M370 <- mask(raster("Delta_artificialness_velocity_2090_ssp370_mean_model_0.45.tif"), KBA)
M585 <- mask(raster("Delta_artificialness_velocity_2090_ssp585_mean_model_0.45.tif"), KBA)
SRTM <- mask(raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/SRTM/wc2.1_15m_elev_laea.tif"), KBA)
M126_no <- M126
M126_no[(M126 == -9999)] <- maxValue(M126)
M245_no <- M245
M245_no[(M245 == -9999)] <- maxValue(M245)
M370_no <- M370
M370_no[(M370 == -9999)] <- maxValue(M370)
M585_no <- M585
M585_no[(M585 == -9999)] <- maxValue(M585)
M_126 <- as.data.frame(M126_no)
M_126 <- drop_na(M_126)
M_245 <- as.data.frame(M245_no)
M_245 <- drop_na(M_245)
M_370 <- as.data.frame(M370_no)
M_370 <- drop_na(M_370)
M_585 <- as.data.frame(M585_no)
M_585 <- drop_na(M_585)
M_SRTM <- as.data.frame(SRTM)
M_SRTM <- drop_na(M_SRTM)
cor.test(M_SRTM$wc2.1_15m_elev_laea, M_126$Delta_artificialness_velocity_2090_ssp126_mean_model_0.45, method = "kendall")
cor.test(M_SRTM$wc2.1_15m_elev_laea, M_245$Delta_artificialness_velocity_2090_ssp245_mean_model_0.45, method = "kendall")
cor.test(M_SRTM$wc2.1_15m_elev_laea, M_370$Delta_artificialness_velocity_2090_ssp370_mean_model_0.45, method = "kendall")
cor.test(M_SRTM$wc2.1_15m_elev_laea, M_585$Delta_artificialness_velocity_2090_ssp585_mean_model_0.45, method = "kendall")

### EI
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/KBA_IBA_40_15m_laea.tif")
M126 <- raster("Exposure_index_ssp126_mean_model.tif")
M245 <- raster("Exposure_index_ssp245_mean_model.tif")
M370 <- raster("Exposure_index_ssp370_mean_model.tif")
M585 <- raster("Exposure_index_ssp585_mean_model.tif")
SRTM <- mask(raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/SRTM/wc2.1_15m_elev_laea.tif"), KBA)
M_126 <- as.data.frame(M126)
M_126 <- drop_na(M_126)
M_245 <- as.data.frame(M245)
M_245 <- drop_na(M_245)
M_370 <- as.data.frame(M370)
M_370 <- drop_na(M_370)
M_585 <- as.data.frame(M585)
M_585 <- drop_na(M_585)
M_SRTM <- as.data.frame(SRTM)
M_SRTM <- drop_na(M_SRTM)
cor.test(M_SRTM$wc2.1_15m_elev_laea, M_126$Exposure_index_ssp126_mean_model, method = "kendall")
cor.test(M_SRTM$wc2.1_15m_elev_laea, M_245$Exposure_index_ssp245_mean_model, method = "kendall")
cor.test(M_SRTM$wc2.1_15m_elev_laea, M_370$Exposure_index_ssp370_mean_model, method = "kendall")
cor.test(M_SRTM$wc2.1_15m_elev_laea, M_585$Exposure_index_ssp585_mean_model, method = "kendall")

### LEI
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/KBA_IBA_40_15m_laea.tif")
M126 <- raster("Local_Exposure_ssp126_mean_model.tif")
M245 <- raster("Local_Exposure_ssp245_mean_model.tif")
M370 <- raster("Local_Exposure_ssp370_mean_model.tif")
M585 <- raster("Local_Exposure_ssp585_mean_model.tif")
SRTM <- mask(raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/SRTM/wc2.1_15m_elev_laea.tif"), KBA)
M_126 <- as.data.frame(M126)
M_126 <- drop_na(M_126)
M_245 <- as.data.frame(M245)
M_245 <- drop_na(M_245)
M_370 <- as.data.frame(M370)
M_370 <- drop_na(M_370)
M_585 <- as.data.frame(M585)
M_585 <- drop_na(M_585)
M_SRTM <- as.data.frame(SRTM)
M_SRTM <- drop_na(M_SRTM)
cor.test(M_SRTM$wc2.1_15m_elev_laea, M_126$Local_exposure_ssp126_mean_model, method = "kendall")
cor.test(M_SRTM$wc2.1_15m_elev_laea, M_245$Local_exposure_ssp245_mean_model, method = "kendall")
cor.test(M_SRTM$wc2.1_15m_elev_laea, M_370$Local_exposure_ssp370_mean_model, method = "kendall")
cor.test(M_SRTM$wc2.1_15m_elev_laea, M_585$Local_exposure_ssp585_mean_model, method = "kendall")

### REI
KBA <- raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/KBA_IBA_40_15m_laea.tif")
M126 <- raster("Forward_Exposure_ssp126_mean_model.tif")
M245 <- raster("Forward_Exposure_ssp245_mean_model.tif")
M370 <- raster("Forward_Exposure_ssp370_mean_model.tif")
M585 <- raster("Forward_Exposure_ssp585_mean_model.tif")
SRTM <- mask(raster("C:/Users/Fabien VERNIEST/Documents/_THESE/SIG/RASTERS/SRTM/wc2.1_15m_elev_laea.tif"), KBA)
M_126 <- as.data.frame(M126)
M_126 <- drop_na(M_126)
M_245 <- as.data.frame(M245)
M_245 <- drop_na(M_245)
M_370 <- as.data.frame(M370)
M_370 <- drop_na(M_370)
M_585 <- as.data.frame(M585)
M_585 <- drop_na(M_585)
M_SRTM <- as.data.frame(SRTM)
M_SRTM <- drop_na(M_SRTM)
cor.test(M_SRTM$wc2.1_15m_elev_laea, M_126$Forward_exposure_ssp126_mean_model, method = "kendall")
cor.test(M_SRTM$wc2.1_15m_elev_laea, M_245$Forward_exposure_ssp245_mean_model, method = "kendall")
cor.test(M_SRTM$wc2.1_15m_elev_laea, M_370$Forward_exposure_ssp370_mean_model, method = "kendall")
cor.test(M_SRTM$wc2.1_15m_elev_laea, M_585$Forward_exposure_ssp585_mean_model, method = "kendall")





##### END OF SCRIPT #####