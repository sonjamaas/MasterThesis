############## Script for comparing canopy cover rasters ########################

setwd("E:/Sonja/Msc_Thesis/data/Metrics/CanopyCover/")


# LAI
cover_uav <- rast("cover_uav.tif")
cover_als <- rast("cover_als.tif")
cover_tls <- rast("cover_tls.tif")
cover_bp <- rast("cover_bp.tif")

plot(cover_als)

plot(cover_uav)
ggplot(data = as.data.frame(cover_uav, xy = TRUE), aes(x = x, y = y, fill = cover_uav))+
  geom_tile()+
  scale_fill_gradient(
    low = "white",
    high = "darkgreen",
    limits = c(0,100),
    name = "Canopy Cover [%]"
  )+
  coord_equal() +
  theme_minimal()+
  labs(title = "Canopy Cover derived from UAV data", )+
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 15))

plot(cover_als)
ggplot(data = as.data.frame(cover_als, xy = TRUE), aes(x = x, y = y, fill = cover_als))+
  geom_tile()+
  scale_fill_gradient(
    low = "white",
    high = "darkgreen",
    limits = c(0,100),
    name = "Canopy Cover [%]"
  )+
  coord_equal() +
  theme_minimal()+
  labs(title = "Canopy Cover derived from ALS data", )+
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 15))

plot(cover_tls)
ggplot(data = as.data.frame(cover_tls, xy = TRUE), aes(x = x, y = y, fill = cover_tls))+
  geom_tile()+
  scale_fill_gradient(
    low = "white",
    high = "darkgreen",    
    limits = c(0,100),
    name = "Canopy Cover [%]"
  )+
  coord_equal() +
  theme_minimal()+
  labs(title = "Canopy Cover derived from TLS data", )+
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 15))

plot(cover_bp)
ggplot(data = as.data.frame(cover_bp, xy = TRUE), aes(x = x, y = y, fill = cover_bp))+
  geom_tile()+
  scale_fill_gradient(
    low = "white",
    high = "darkgreen",
    limits = c(0,100),
    name = "Canopy Cover [%]"
  )+
  coord_equal() +
  theme_minimal()+
  labs(title = "Canopy Cover derived from Backpack data", )+
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 15))



# extract values
vals_uav <- values(cover_uav)
vals_als <- values(cover_als)
vals_tls <- values(cover_tls)
vals_bp <- values(cover_bp)
valid <- complete.cases(vals_uav, vals_als, vals_bp, vals_tls)

# plot values to check them
plot(vals_uav[valid], vals_als[valid],
     xlab="UAV Cover", ylab="ALS Cover",
     main="Pixel-wise Comparison (UAV + ALS)")
abline(0, 1, col="red")

plot(vals_uav[valid], vals_tls[valid],
     xlab="UAV Cover", ylab="TLS Cover",
     main="Pixel-wise Comparison (UAV + TLS)")
abline(0, 1, col="red")

plot(vals_uav[valid], vals_bp[valid],
     xlab="UAV Cover", ylab="Backpack Cover",
     main="Pixel-wise Comparison (UAV + Backpack)")
abline(0, 1, col="red")

plot(vals_als[valid], vals_tls[valid],
     xlab="ALS Cover", ylab="TLS Cover",
     main="Pixel-wise Comparison (ALS + TLS)")
abline(0, 1, col="red")

plot(vals_als[valid], vals_bp[valid],
     xlab="ALS Cover", ylab="Backpack Cover",
     main="Pixel-wise Comparison (ALS + Backpack)")
abline(0, 1, col="red")

plot(vals_tls[valid], vals_bp[valid],
     xlab="TLS Cover", ylab="Backpack Cover",
     main="Pixel-wise Comparison (TLS + Backpack)")
abline(0, 1, col="red")


# Layer summaries
metrics <- tibble(
  Metric = c("Mean", "Median", "SD", "Min", "Max"),
  UAV = c(
    global(cover_uav, mean, na.rm = TRUE)[[1]],
    global(cover_uav, median, na.rm = TRUE)[[1]],
    global(cover_uav, sd, na.rm = TRUE)[[1]],
    global(cover_uav, range, na.rm = TRUE)[1],
    global(cover_uav, range, na.rm = TRUE)[2]
  ),
  ALS = c(
    global(cover_als, mean, na.rm = TRUE)[[1]],
    global(cover_als, median, na.rm = TRUE)[[1]],
    global(cover_als, sd, na.rm = TRUE)[[1]],
    global(cover_als, range, na.rm = TRUE)[1],
    global(cover_als, range, na.rm = TRUE)[2]
  ),
  TLS = c(
    global(cover_tls, mean, na.rm = TRUE)[[1]],
    global(cover_tls, median, na.rm = TRUE)[[1]],
    global(cover_tls, sd, na.rm = TRUE)[[1]],
    global(cover_tls, range, na.rm = TRUE)[1],
    global(cover_tls, range, na.rm = TRUE)[2]
  ),
  BP = c(
    global(cover_bp, mean, na.rm = TRUE)[[1]],
    global(cover_bp, median, na.rm = TRUE)[[1]],
    global(cover_bp, sd, na.rm = TRUE)[[1]],
    global(cover_bp, range, na.rm = TRUE)[1],
    global(cover_bp, range, na.rm = TRUE)[2]
  )
)

print(metrics)

values1 <- values(cover_uav)
values2 <- values(cover_als)
values3 <- values(cover_tls)
values4 <- values(cover_bp)

# Exclude NAs
valid <- complete.cases(values1, values2, values3, values4)





# DIFFERENCE OF ALS - UAV

# run linear model to check relationship
lm_model_als_uav <- lm(vals_als[valid] ~ vals_uav[valid])
summary(lm_model_als_uav)

vals_df_als_uav <- data.frame(
  als = vals_als,
  uav = vals_uav
)

vals_df_als_uav <- na.omit(vals_df_als_uav)
# library(ggplot2)
ggplot(data = vals_df_als_uav, aes(x=cover_als , y = cover_uav))+
  geom_tile()

# difference map
cover_uav_resampled <- resample(cover_uav, cover_als, method = "bilinear")
cover_tls_resampled <- resample(cover_tls, cover_als, method = "bilinear")
cover_bp_resampled <- resample(cover_bp, cover_als, method = "bilinear")
cover_tls_resampled2 <- resample(cover_tls, cover_uav, method = "bilinear")
cover_bp_resampled2 <- resample(cover_bp, cover_uav, method = "bilinear")
cover_bp_resampled3 <- resample(cover_bp, cover_tls, method = "bilinear")



diff_raster_als_uav <- cover_als - cover_uav_resampled
diff_raster_als_tls <- cover_als - cover_tls_resampled
diff_raster_als_bp <- cover_als - cover_tls_resampled
diff_raster_uav_tls <- cover_uav - cover_tls_resampled2
diff_raster_uav_bp <- cover_uav - cover_bp_resampled2
diff_raster_tls_bp <- cover_tls - cover_bp_resampled3


x <- plot(diff_raster_als_uav, main = "ALS - UAV LAI")


global(diff_raster_als_uav, fun = mean, na.rm=TRUE)
global(diff_raster_als_uav, fun = sd, na.rm=TRUE)

# extract diff raster values
vals_diff_als_uav <- values(diff_raster_als_uav)
vals_diff_als_tls <- values(diff_raster_als_tls)
vals_diff_als_bp <- values(diff_raster_als_bp)
vals_diff_uav_tls <- values(diff_raster_uav_tls)
vals_diff_uav_bp <- values(diff_raster_uav_bp)
vals_diff_tls_bp <- values(diff_raster_tls_bp)

vals_diff_clean_als_uav <- vals_diff_als_uav[!is.na(vals_diff_als_uav)]
vals_diff_clean_als_tls <- vals_diff_als_tls[!is.na(vals_diff_als_tls)]
vals_diff_clean_als_bp <- vals_diff_als_bp[!is.na(vals_diff_als_bp)]
vals_diff_clean_uav_tls <- vals_diff_uav_tls[!is.na(vals_diff_uav_tls)]
vals_diff_clean_uav_bp <- vals_diff_uav_bp[!is.na(vals_diff_uav_bp)]
vals_diff_clean_tls_bp <- vals_diff_tls_bp[!is.na(vals_diff_tls_bp)]


metrics_combi <- tibble(
  Metric = c("Mean", "Median", "SD", "Range Min", "Range Max", "RSME", "MAE"),
  ALS_UAV = c(
    mean(vals_diff_clean_als_uav)[[1]],
    median(vals_diff_clean_als_uav)[[1]],
    sd(vals_diff_clean_als_uav)[[1]],
    range(vals_diff_clean_als_uav)[1],
    range(vals_diff_clean_als_uav)[2],
    sqrt(mean((vals_als[valid] - vals_uav[valid])^2)),
    mean(abs(vals_als[valid] - vals_uav[valid]))
  ),
  ALS_TLS = c(
    mean(vals_diff_clean_als_tls)[[1]],
    median(vals_diff_clean_als_tls)[[1]],
    sd(vals_diff_clean_als_tls)[[1]],
    range(vals_diff_clean_als_tls)[1],
    range(vals_diff_clean_als_tls)[2],
    sqrt(mean((vals_als[valid] - vals_tls[valid])^2)),
    mean(abs(vals_als[valid] - vals_tls[valid]))
  ),
  ALS_BP = c(
    mean(vals_diff_clean_als_bp)[[1]],
    median(vals_diff_clean_als_bp)[[1]],
    sd(vals_diff_clean_als_bp)[[1]],
    range(vals_diff_clean_als_bp)[1],
    range(vals_diff_clean_als_bp)[2],
    sqrt(mean((vals_als[valid] - vals_bp[valid])^2)),
    mean(abs(vals_als[valid] - vals_bp[valid]))
  ),
  UAV_TLS = c(
    mean(vals_diff_clean_uav_tls)[[1]],
    median(vals_diff_clean_uav_tls)[[1]],
    sd(vals_diff_clean_uav_tls)[[1]],
    range(vals_diff_clean_uav_tls)[1],
    range(vals_diff_clean_uav_tls)[2],
    sqrt(mean((vals_uav[valid] - vals_tls[valid])^2)),
    mean(abs(vals_uav[valid] - vals_tls[valid]))
  ),
  UAV_BP = c(
    mean(vals_diff_clean_uav_bp)[[1]],
    median(vals_diff_clean_uav_bp)[[1]],
    sd(vals_diff_clean_uav_bp)[[1]],
    range(vals_diff_clean_uav_bp)[1],
    range(vals_diff_clean_uav_bp)[2],
    sqrt(mean((vals_uav[valid] - vals_bp[valid])^2)),
    mean(abs(vals_uav[valid] - vals_bp[valid]))
  ),
  TLS_BP = c(
    mean(vals_diff_clean_tls_bp)[[1]],
    median(vals_diff_clean_tls_bp)[[1]],
    sd(vals_diff_clean_tls_bp)[[1]],
    range(vals_diff_clean_tls_bp)[1],
    range(vals_diff_clean_tls_bp)[2],
    sqrt(mean((vals_tls[valid] - vals_bp[valid])^2)),
    mean(abs(vals_tls[valid] - vals_bp[valid]))
  )
)

print(metrics)









# paired t-test
t.test(vals_als[valid], vals_uav[valid], paired=TRUE)
t.test(vals_als[valid], vals_tls[valid], paired=TRUE)
t.test(vals_als[valid], vals_bp[valid], paired=TRUE)
t.test(vals_uav[valid], vals_tls[valid], paired=TRUE)
t.test(vals_uav[valid], vals_bp[valid], paired=TRUE)
t.test(vals_tls[valid], vals_bp[valid], paired=TRUE)


# correlation 
lm_model <- lm(vals_als[valid] ~ vals_uav[valid])
summary(lm_model)


# histogram of differences
hist(vals_diff_clean,
     breaks = 50,
     main = "Histogram of Differences (ALS - UAV)",
     xlab = "Difference in LAI")

# density plot
plot(density(vals_diff_clean, na.rm=TRUE),
     main="Density of Differences",
     xlab="Difference in LAI")
abline(v=0, col="red", lty=2)

# scatter plot
plot(vals_uav[valid], vals_als[valid],
     xlab="Scaled UAV LAI",
     ylab="ALS LAI",
     main="Pixel-wise Comparison")
abline(0,1,col="red")

residuals <- vals_als[valid] - vals_uav[valid]

plot(residuals, main="Residuals (ALS - UAV)",
     ylab="Difference (LAI)")
abline(h=0, col="red", lty=2)








### make values df into long format
data <- data.frame()
data <- rbind(data, vals_als)
data <- cbind(data, vals_tls)
data <- cbind(data, vals_uav)
data <- cbind(data, vals_bp)

data_long <- pivot_longer(data, cols = c("cover_als", "cover_tls", "cover_uav", "cover_bp"), names_to = "Source", values_to = "Cover")


## Viz
ggplot(data_long, aes(x = Source, y = Cover))+
  geom_jitter(color = "grey",
              alpha = 0.7,
              size = 1)+
  geom_boxplot(color = "darkolivegreen",
               fill = "darkolivegreen4",
               alpha = 0.3,
               notch = TRUE,
               notchwidth = 0.8,
               outlier.colour="red",
               outlier.fill="red",
               outlier.size=2)+
  labs(title = "Canopy Cover Measurement Comparison", )+
  xlab("") +
  ylab("Canopy Cover [%]") +
  theme_minimal()+
  theme(plot.title = element_text(size = 15))

# export in 6.28 6.4, cubes quadratic



#### 4. pairwise comparison

common_scale <- scale_fill_gradient2(
  low = "blue",
  mid = "white",
  high = "red",
  midpoint = 0,
  limits = c(-100, 100),
  oob = scales::squish # avoids warnings from small differences
)
library(dplyr)
diff_raster_als_uav <- rename(as.data.frame(diff_raster_als_uav, xy = TRUE, na.rm = TRUE), canopy_diff = cover_als)
diff_raster_als_tls <- rename(as.data.frame(diff_raster_als_tls, xy = TRUE, na.rm = TRUE), canopy_diff = cover_als)
diff_raster_als_bp <- rename(as.data.frame(diff_raster_als_bp, xy = TRUE, na.rm = TRUE), canopy_diff = cover_als)
diff_raster_uav_tls <- rename(as.data.frame(diff_raster_uav_tls, xy = TRUE, na.rm = TRUE), canopy_diff = cover_uav  )
diff_raster_uav_bp <- rename(as.data.frame(diff_raster_uav_bp, xy = TRUE, na.rm = TRUE), canopy_diff = cover_uav  )
diff_raster_tls_bp <- rename(as.data.frame(diff_raster_tls_bp, xy = TRUE, na.rm = TRUE), canopy_diff = cover_tls)


a <- ggplot(data = diff_raster_als_uav, aes(x = x, y = y, fill = canopy_diff))+
  geom_tile()+
  coord_equal() +
  ylab("UAV") +
  xlab("")+
  common_scale +
  theme_minimal()+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0.5,0,0,0.5), "cm"),
        legend.position = "none")+ 
  annotate("text", x = 524020, y = 5537700, label = "ALS - UAV", hjust = "right")+ labs(fill = 'Canopy Cover [%]')

b <- ggplot(data = diff_raster_als_tls, aes(x = x, y = y, fill = canopy_diff))+
  geom_tile()+
  coord_equal() +
  ylab("TLS") +
  xlab("")+
  common_scale +
  theme_minimal()+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,0,0,0.5), "cm"),
        legend.position = "none")+ 
  annotate("text", x = 524020, y = 5537700, label = "ALS - TLS", hjust = "right")+ labs(fill = 'Canopy Cover [%]')

c <- ggplot(data = diff_raster_als_bp, aes(x = x, y = y, fill = canopy_diff))+
  geom_tile()+
  coord_equal() +
  ylab("Backpack") +
  xlab("ALS")+
  common_scale +
  theme_minimal()+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        plot.margin = unit(c(0,0,0,0.5), "cm"),
        legend.position = "none")+ 
  annotate("text", x = 524020, y = 5537700, label = "ALS - Backpack", hjust = "right")+ labs(fill = 'Canopy Cover [%]')


d <- ggplot(data = diff_raster_uav_tls, aes(x = x, y = y, fill = canopy_diff))+
  geom_tile()+
  coord_equal() +
  ylab("") +
  xlab("")+
  common_scale +
  theme_minimal()+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.position = "none")+ 
  annotate("text", x = 524020, y = 5537700, label = "UAV - TLS", hjust = "right")+ labs(fill = 'Canopy Cover [%]')

e <- ggplot(data = diff_raster_uav_bp, aes(x = x, y = y, fill = canopy_diff))+
  geom_tile()+
  coord_equal() +
  ylab("") +
  xlab("UAV")+
  common_scale +
  theme_minimal()+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.position = "none")+ 
  annotate("text", x = 524020, y = 5537700, label = "UAV - Backpack", hjust = "right")+ labs(fill = 'Canopy Cover [%]')

f <- ggplot(data = diff_raster_tls_bp, aes(x = x, y = y, fill = canopy_diff))+
  geom_tile()+
  coord_equal() +
  ylab("") +
  xlab("TLS")+
  common_scale +
  theme_minimal()+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0,0.5,0,0), "cm"),
        legend.position = "none")+ 
  annotate("text", x = 524020, y = 5537700, label = "TLS - Backpack", hjust = "right")+ labs(fill = 'Canopy Cover [%]')

# lay2 <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
# 
# grid.arrange(a, NULL, NULL, b, d, NULL, c, e, f, 
#              layout_matrix = lay2, widths = c(1,1,1), heights = c(1.01,0.94,1)
#              ,top = textGrob("Pairwise Comparison of LAI measurements", gp=gpar(fontsize =15))
# )
# export in 6.28 6.4, cubes quadratic


layout <- "
A##
BD#
CEF
"
combined <- (
  a + b + c + d + e + f +
    plot_layout(design = layout, guides = "collect") &
    theme(legend.position = "right") 
)

combined +
  plot_annotation(
    title = "Pairwise Comparison of Canopy Cover measurements"
  )


