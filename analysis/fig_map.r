fig_map <- function(df_2D, flag_season = FALSE) {

  bins_origin <- c(0, 0.05, 0.5, 1)
  bins_origin_lev <- baseutils::bins2lev(bins_origin)
  bins_origin_lab <- c("in-situ", "]0-50%]", "]50-100%]")

## bins_origin <- c(0, 0.01, 1)
## bins_origin_lev <- baseutils::bins2lev(bins_origin)
## bins_origin_lab <- c("in-situ", "]0-100%]")

bins_lat <- seq(-90, 90, by = 1)
bins_lon <- seq(-180, 180, by = 1)

  if(!flag_season) {
    xx <- df_2D %>%
      plotutils::bin(origin_cloud, bins = bins_origin) %>%
      plotutils::bin(lat, bins_lat) %>%
      plotutils::bin(lon, bins_lon) %>%
      group_by(lat_bin, lon_bin) %>%
      mutate(n_tot = n()) %>%
      ungroup() %>%
      group_by(origin_cloud_bin, lat_bin, lon_bin) %>%
      summarize(count = n(),
                density = count / n_tot[1],
                .groups = "keep") %>%
      data.frame() %>%
      mutate(origin_cloud_bin = factor(origin_cloud_bin, levels = bins_origin_lev, labels = bins_origin_lab)) %>%
      rename(origin = origin_cloud_bin)
  } else {
xx <- df_2D %>%
      plotutils::bin(origin_cloud, bins = bins_origin) %>%
      plotutils::bin(lat, bins_lat) %>%
      plotutils::bin(lon, bins_lon) %>%
      group_by(lat_bin, lon_bin, season) %>%
      mutate(n_tot = n()) %>%
      ungroup() %>%
      group_by(origin_cloud_bin, lat_bin, lon_bin, season) %>%
      summarize(count = n(),
                density = count / n_tot[1],
                .groups = "keep") %>%
      data.frame() %>%
      mutate(origin_cloud_bin = factor(origin_cloud_bin, levels = bins_origin_lev, labels = bins_origin_lab)) %>%
  rename(origin = origin_cloud_bin) %>%
  filter(season %in% c("DJF", "JJA"))
}

p <- xx %>%
  ggplot() +
  geom_tile(aes(x = lon_bin, y = lat_bin, fill = density)) +
  scale_fill_distiller(palette = "Spectral", limits = c(0, 1)) +
  plotutils::geom_world_polygon() +
  plotutils::scale_x_geo() +
  plotutils::scale_y_geo() +
  facet_wrap(~origin)

  if(!flag_season) {
    p <- p + facet_wrap(~origin)
    ggsave("~/fig_flex_map_origin_fraction.png", p, width = 10, height = 5)
  } else {
    p <- p + facet_grid(~season ~origin)
    ggsave("~/fig_flex_map_origin_fraction_season.png", p, width = 10, height = 5)
  }

  return(NULL)

}
