############## Script for calculating the gap fraction #########################

library(lidR)


# Gap Fraction= Total number of returns/Number of ground returns

# read files
setwd("E:/Sonja/Msc_Thesis/data/8_preprocessedData/")

las_bp <- readLAS("bp/backpack_feb5_subsampled_0_05_Normalize by Ground Points.las")
las_uav <- readLAS("uav/UAV_feb2_shifted_clipped.las")
las_als <- readLAS("als/ALS_first_last_merged_shifted.las")
las_tls <- readLAS("tls/TLS_Hain_20_21_subsample_0_05_Normalize by Ground Points.las")


gap_fraction_fun <- function(z) {
  # z: vector of normalized heights (above ground)
  ground_returns <- sum(z <= 2)  # returns below or equal 2m considered ground
  total_returns <- length(z)
  gap_fraction <- ground_returns / total_returns
  return(gap_fraction)
}

gap_fraction_bp <- grid_metrics(las_bp, ~gap_fraction_fun(Z), res = 0.6)
gap_fraction_uav <- grid_metrics(las_uav, ~gap_fraction_fun(Z), res = 0.6)
gap_fraction_als <- grid_metrics(las_als, ~gap_fraction_fun(Z), res = 0.6)
gap_fraction_tls <- grid_metrics(las_tls, ~gap_fraction_fun(Z), res = 0.6)

plot(gap_fraction_bp, main = "Gap Fraction")

gf_als_clipped <- crop(gap_fraction_als, gap_fraction_uav)
gf_uav_clipped <- crop(gap_fraction_uav, gf_als_clipped)
gf_bp_clipped <- crop(gap_fraction_bp, gf_als_clipped)
gf_tls_clipped <- crop(gap_fraction_tls, gf_als_clipped)

gf_uav_clipped <- crop(gf_uav_clipped, gf_tls_clipped)
gf_als_clipped <- crop(gf_als_clipped, gf_tls_clipped)

plot(gf_als_clipped)
plot(gf_tls_clipped)
plot(gf_bp_clipped)
plot(gf_uav_clipped)


writeRaster(gf_als_clipped, "gf_als.tif", overwrite=TRUE)
writeRaster(gf_tls_clipped, "gf_tls.tif", overwrite=TRUE)
writeRaster(gf_uav_clipped, "gf_uav.tif", overwrite=TRUE)
writeRaster(gf_bp_clipped, "gf_bp.tif", overwrite=TRUE)







