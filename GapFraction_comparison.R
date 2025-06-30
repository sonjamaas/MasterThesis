# gap fraction
gf_als <- raster("als/forest_metrics_lidar360/ALS_first_last_merged_shifted_GapFraction.tif")
plot(gf_als)
gf_uav <- raster("uav/forest_metrics_LiDAR360/UAV_feb2_shifted_clipped_GapFraction.tif")
plot(gf_uav)

gf_uav_resampled <- resample(gf_uav, gf_als, method = "bilinear")
plot(gf_uav_resampled)

stack_metrics <- stack(gf_uav_resampled, gf_als)
cor_matrix <- layerStats(stack_metrics, 'pearson', na.rm = TRUE)
# pearson correlation coefficient
#         UAV GF       ALS GF
# UAV GF  1.0000000    0.5146394
# ALS GF  0.5146394    1.0000000
# 
# mean
# UAV GF       ALS GF 
# 0.5124771    0.4542397 

corr_coeff <- cor_matrix$`pearson correlation coefficient`[2,1]
# [1] 0.5146394

p_value <- corLocal(gf_uav_resampled, gf_als, test = TRUE)[[2]]
plot(p_value)

cellStats(abs(gf_als - gf_uav_resampled), mean)
# [1] 0.1773367

gf_uav_rast <- rast(gf_uav_resampled)
gf_als_rast <- rast(gf_als)
gf_difference <- raster.change(gf_als_rast, gf_uav_rast, s = 5, stat ="t.test")
plot(gf_difference)

lm_model <- lm(as.matrix(gf_als_rast) ~ as.matrix(gf_uav_rast))
summary(lm_model)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.55723 -0.14965 -0.03082  0.12317  0.76572 
# 
# Coefficients:
#                        Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            0.083750   0.006323   13.24   <2e-16 ***
# as.matrix(gf_uav_rast) 0.716713   0.011625   61.65   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2074 on 10414 degrees of freedom
# (958 Beobachtungen als fehlend gelöscht)
# Multiple R-squared:  0.2674,	Adjusted R-squared:  0.2673 
# F-statistic:  3801 on 1 and 10414 DF,  p-value: < 2.2e-16


