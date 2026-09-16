library(dplyr)
library(ggplot2)
library(doMC); registerDoMC(cores = 40)
library(patchwork)

dir_main <- "analysis"
dir_rds <- file.path(Sys.getenv("DARNI_SCRATCH", file.path(Sys.getenv("HOME"), "storage", "scratch")), "tmp/dardar_flex_rds")

source("R/utils.r")
source("R/find_origin.r")
source("R/read_flexpart.r")
source("R/read_dardar.r")
source("R/load_data.r")
source("R/merge_flex_dardar.r")
source("R/write_dardar_flex.r")
source(paste0(dir_main, "/load_dardar_origin.r"))

dir_dardar <- file.path(Sys.getenv("DARNI_WORK", file.path(Sys.getenv("HOME"), "storage", "work")), "data/satellite/DARDAR-Nice/DARNI_L2_PRO.v2.0/2010")
lf_dardar <- list.files(dir_dardar, full.names = TRUE, recursive = TRUE, pattern = ".nc")

dir_dardar_flux <- file.path(Sys.getenv("DARNI_WORK", file.path(Sys.getenv("HOME"), "storage", "work")), "data/satellite/DARDAR-Nice/AUX/ecrad/dar2era_v1.10/output/2010")

dir_flex <- file.path(Sys.getenv("DARNI_SCRATCH", file.path(Sys.getenv("HOME"), "storage", "scratch")), "data/model/FLEXPART")
lf_flex <- list.files(dir_flex, full.names = TRUE, recursive = TRUE, pattern = ".nc")

dir_dardar_origin <- file.path(Sys.getenv("DARNI_SCRATCH", file.path(Sys.getenv("HOME"), "storage", "scratch")), "tmp/dardar_flex")

## null <- plyr::ldply(lf_flex[206], load_data, lf_dardar = lf_dardar, dir_out = dir_dardar_origin, overwrite = TRUE, .parallel = TRUE)

lf_dardar_origin <- list.files(dir_dardar_origin, full.names = TRUE, recursive = TRUE, pattern = ".nc")
lf_dardar_flux <- list.files(dir_dardar_flux, recursive = TRUE, full.names = TRUE, pattern = "output-cre")
df_2D <- plyr::ldply(lf_dardar_origin, load_dardar_origin_2D, lf_dardar = lf_dardar, lf_dardar_flux = lf_dardar_flux, .parallel = TRUE)

null <- plyr::ldply(lf_dardar_origin, load_dardar_origin_3D, lf_dardar = lf_dardar, dir_rds = dir_rds, .parallel = TRUE)
lf_3D <- list.files(paste0(dir_rds, "/3D"), full.names = TRUE)
season_3D <- baseutils::time2season(as.POSIXct(c(plyr::ldply(lf_3D, function(fn) {strsplit(basename(fn), "_", fixed = TRUE)}[[1]][5])$V1), format = "%Y%m%d%H%M%S"))
lf_3D <- lf_3D[which(season_3D %in% c("DJF","JJA"))]
df_3D <- plyr::ldply(lf_3D, readRDS)


## fn_flex <- lf_flex[1]
## fn_flex <- "/work/bb1036/b380333/data/model/FLEXPART/DARDAR/DARNI_PRO_L2_v2.0_20100203185536_FLEX.nc"
## fn_flex <- "/work/bb1036/b380333/data/model/FLEXPART/DARDAR/DARNI_PRO_L2_v2.0_20100203185536_FLEX_number.nc"
## fn_flex <- "/work/bb1036/b380333/data/model/FLEXPART/DARDAR/DARNI_PRO_L2_v2.0_20100203185536_FLEX_homogeneous.nc"
## load_data(fn_flex, lf_dardar = lf_dardar, dir_out = dir_dardar_origin, overwrite = TRUE)

source(paste0(dir_main, "/fig_stat_origin.r"))
fig_stat_origin(df_2D)

source(paste0(dir_main, "/fig_zonal.r"))
fig_zonal(df_3D)

source(paste0(dir_main, "/fig_map.r"))
fig_map(df_2D, flag_season = TRUE)

source(paste0(dir_main, "/fig_dolphin.r"))
fig_dolphin(df_2D)

source(paste0(dir_main, "/fig_profiles.r"))
fig_profiles(df_3D)

source(paste0(dir_main, "/fig_dt.r"))
fig_dt(df_3D)


bins_ta <- seq(-80, 0, by = 2.5)
bins_lat_region <- c(-90, -67.7, -23.3, 23.3, 67.7, 90)
lab_lat_region <- c("Antarctic","Mid-lat S.","Tropics","Mid-lat N.","Arctic")

xx <- df_3D %>%
  slice(1:1E7) %>%
  filter(origin %in% c(0, 1), origin_flag == 1) %>%
  #mutate(ta_origin = ta) %>%
  mutate(ta_origin = ta_origin - 273.15) %>%
  plotutils::bin(ta_origin, bins = bins_ta) %>%
  plotutils::bin(lat, bins_lat_region) %>%
  mutate(region = factor(lat_bin, levels = baseutils::bins2lev(bins_lat_region), labels = lab_lat_region)) %>%
  group_by(ta_origin_bin, origin, region) %>%
  summarize(count = n(),
            .groups = "drop") %>%
  data.frame()

p <- xx %>%
  ggplot(aes(x = ta_origin_bin, y = count, fill = origin)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~region)
ggsave("~/fig_flex_test.png", p, width = 10, height = 5)


bins_w <- seq(-5, 5, by = 0.25)
bins_ta <- seq(-80, 0, by = 2.5)
bins_origin <- c(0, 0.5, 1)
bins_dt <- c(0, 5, 130)
xx <- df_3D %>%
  filter(origin %in% c(0, 1)) %>%
  mutate(ta_origin = ta_origin - 273.15) %>%
  plotutils::bin(w_origin, bins = bins_w) %>%
  plotutils::bin(origin, bins = bins_origin) %>%
  plotutils::bin(ta_origin, bins = bins_ta) %>%
  plotutils::bin(dt_cloud, bins = bins_dt) %>%
  group_by(w_origin_bin, ta_origin_bin, origin_bin, dt_cloud_bin) %>%
  summarize(iwc = mean(iwc),
            icnc = mean(icnc),
            .groups = "drop") %>%
  data.frame()

p <- xx %>%
  ggplot(aes(x = w_origin_bin, y = ta_origin_bin, fill = icnc)) +
  geom_tile() +
  scale_fill_distiller(palette = "Spectral") +
  facet_grid(dt_cloud_bin~origin_bin)

ggsave("~/fig_flex_test.png", p, width = 10, height = 5)



## Ok below we test

lf_out <- list.files(dir_dardar_origin, full = TRUE, recursive = TRUE, pattern = ".nc")
fn_test <- lf_out[grepl("20100203185536", lf_out)]

nc_test <- ncdf4::nc_open(fn_test)
df_test <- data.frame(expand.grid(idx_height = 1:nc_test$dim$height$len,
                                  idx_time = 1:nc_test$dim$time$len)) %>%
  dplyr::mutate(origin = c(ncdf4::ncvar_get(nc_test, "origin")),
                dt_cloud = c(ncdf4::ncvar_get(nc_test, "dt_cloud")),
                origin_quality = c(ncdf4::ncvar_get(nc_test, "origin_quality")),
                n_part = c(ncdf4::ncvar_get(nc_test, "n_part")),
                lat_origin = c(ncdf4::ncvar_get(nc_test, "lat_origin")),
                lon_origin = c(ncdf4::ncvar_get(nc_test, "lon_origin")),
                lat = ncdf4::ncvar_get(nc_test, "lat")[idx_time],
                lon = ncdf4::ncvar_get(nc_test, "lon")[idx_time],
                height = ncdf4::ncvar_get(nc_test, "height")[idx_height]
                )

lat_min <- 31.2
lat_max <- 41.7

p <- df_test %>%
  mutate(var = origin) %>%
  mutate(var = replace(var, !is.na(var), 1)) %>%
  filter(!is.na(var)) %>%
  filter(lat > lat_min, lat < lat_max, lon < 0) %>%
  ggplot(aes(x = lat, y = height, fill = var)) +
  geom_tile() +
  scale_fill_distiller(palette = "Greens")
ggsave("~/fig_flex_test.png", p, width = 10, height = 5)

p <- df_dardar_flex %>%
  mutate(var = origin) %>%
  #filter(!is.na(var)) %>%
  filter(lat > lat_min, lat < lat_max, lon > 0) %>% str()
  ggplot(aes(x = lat, y = height, fill = var)) +
  geom_tile() +
  scale_fill_distiller(palette = "Spectral")
ggsave("~/fig_flex_test.png", p, width = 10, height = 5)


df_flex_overpass %>%
  filter(lat > 36, lat < lat_max, lon < 0) %>%
  filter(abs(lat - 36.02703) < 0.001)
  arrange(lat) %>%
  slice(1)

df_flex %>%
  filter(time_traj == 0) %>%
  filter(lat > 33, lat < 33.1, lon < 0, height < 7000) %>%
  slice(1)
