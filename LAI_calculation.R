################### Script for calculating the LAI #############################

library(terra)
library(lidR)

setwd("E:/Sonja/Msc_Thesis/data/8_preprocessedData/")

las_bp <- readLAS("bp/backpack_feb5_subsampled_0_05_Normalize by Ground Points.las")
las_uav <- readLAS("uav/UAV_feb2_shifted_clipped.las")
las_als <- readLAS("als/ALS_first_last_merged_shifted.las")
las_tls <- readLAS("tls/TLS_Hain_20_21_subsample_0_05_Normalize by Ground Points.las")

summary(las)
plot(las_als)

# height threshold = 2m (ground/canopy)

# compute metrics per cell (ALS metrics: 0.6x0.6m)
# Define function to compute LAI metric
lai_fun <- function(z)
{
  above <- sum(z > 2)   # points above 2 m
  total <- length(z)    # all returns
  lai <- ifelse(total > 0, above / total, NA)
  return(lai)
}

# Compute raster of LAI values
lai_bp <- grid_metrics(las_bp, lai_fun(Z), res = 0.6)
lai_uav <- grid_metrics(las_uav, lai_fun(Z), res = 0.6)
lai_als <- grid_metrics(las_als, lai_fun(Z), res = 0.6)
lai_tls <- grid_metrics(las_tls, lai_fun(Z), res = 0.6)

plot(lai_bp, main="LAI backpack (fraction of returns > 2 m)")
plot(lai_uav, main="LAI UAV (fraction of returns > 2 m)")
plot(lai_als, main="LAI ALS (fraction of returns > 2 m)")
plot(lai_tls, main="LAI TLS (fraction of returns > 2 m)")

lai_als_clipped <- crop(lai_als, lai_uav)
lai_uav_clipped <- crop(lai_uav, lai_als_clipped)
lai_bp_clipped <- crop(lai_bp, lai_als_clipped)
lai_tls_clipped <- crop(lai_tls, lai_als_clipped)

lai_uav_clipped <- crop(lai_uav_clipped, lai_tls_clipped)
lai_als_clipped <- crop(lai_als_clipped, lai_tls_clipped)

plot(lai_tls_clipped)

writeRaster(lai_als_clipped, "lai_als.tif", overwrite=TRUE)
writeRaster(lai_uav_clipped, "lai_uav.tif", overwrite=TRUE)
writeRaster(lai_bp_clipped, "lai_bp.tif", overwrite=TRUE)
writeRaster(lai_tls_clipped, "lai_tls.tif", overwrite=TRUE)
