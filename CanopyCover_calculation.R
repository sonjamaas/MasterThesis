############### Script to calculate canopy cover from LiDAR data ###############

library(lidR)

# read files
setwd("E:/Sonja/Msc_Thesis/data/8_preprocessedData/")

las_bp <- readLAS("bp/backpack_feb5_subsampled_0_05_Normalize by Ground Points.las")
las_uav <- readLAS("uav/UAV_feb2_shifted_clipped.las")
las_als <- readLAS("als/ALS_first_last_merged_shifted.las")
las_tls <- readLAS("tls/TLS_Hain_20_21_subsample_0_05_Normalize by Ground Points.las")


myCanopyCover <- function(z, rn) {
  first = rn == 1L
  zfirst = z[first]
  nfirst = length(zfirst)
  above2 = sum(zfirst > 10)
  cover = (above2 / nfirst) * 100
  return(cover)
}

cover_grid_bp <- grid_metrics(las_bp, ~myCanopyCover(Z, ReturnNumber), res = 0.6)
plot(cover_grid_als)

cover_grid_uav <- grid_metrics(las_uav, ~myCanopyCover(Z, ReturnNumber), res = 0.6)
cover_grid_als <- grid_metrics(las_als, ~myCanopyCover(Z, ReturnNumber), res = 0.6)
cover_grid_tls <- grid_metrics(las_tls, ~myCanopyCover(Z, ReturnNumber), res = 0.6)

cover_als_clipped <- crop(cover_grid_als, cover_grid_uav)
cover_uav_clipped <- crop(cover_grid_uav, cover_als_clipped)
cover_bp_clipped <- crop(cover_grid_bp, cover_als_clipped)
cover_tls_clipped <- crop(cover_grid_tls, cover_als_clipped)

cover_uav_clipped <- crop(cover_uav_clipped, cover_tls_clipped)
cover_als_clipped <- crop(cover_als_clipped, cover_tls_clipped)

writeRaster(cover_als_clipped, "cover_als.tif", overwrite=TRUE)
writeRaster(cover_uav_clipped, "cover_uav.tif", overwrite=TRUE)
writeRaster(cover_bp_clipped, "cover_bp.tif", overwrite=TRUE)
writeRaster(cover_tls_clipped, "cover_tls.tif", overwrite=TRUE)

plot(cover_als_clipped)


