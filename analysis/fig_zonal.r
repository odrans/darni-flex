fig_zonal <- function(df_in) {

  bins_height <- seq(0, 20000, by = 120)
  bins_lat <- seq(-90, 90, 0.1)
  df <- df_in %>%
    plotutils::bin(height, bins = bins_height) %>%
    plotutils::bin(lat, bins = bins_lat) %>%
    dplyr::mutate(lat = lat_bin) %>%
    dplyr::group_by(lat, height_bin) %>%
    dplyr::summarise(origin = mean(origin, na.rm = TRUE),
                     dt_cloud = mean(dt_cloud, na.rm = TRUE),
                     ta = mean(ta, na.rm = TRUE),
                     .groups = "drop")

  df_ta <- df %>%
    mutate(ta = ta - 273.15) %>%
    dplyr::group_by(lat) %>%
    dplyr::summarise(height_40 = height_bin[which.min(abs(ta + 40))],
                     height_0 = height_bin[which.min(abs(ta - 0))],
                     .groups = "drop")

  p <- df %>%
    ggplot() +
    geom_tile(aes(x = lat, y = height_bin * 1E-3, fill = origin)) +
    geom_line(data = df_ta, aes(x = lat, y = height_40 * 1E-3), color = "black", size = 0.5) +
    scale_fill_distiller("",
                         palette = "Spectral", limits = c(0, 1),
                         breaks = c(0, 0.25, 0.5, 0.75, 1),
                         labels = c("in-situ", "", "50%", "", "liquid")) +
    scale_y_continuous("Altitude (km)", limits = c(0, 20), expand = c(0, 0)) +
    plotutils::scale_x_geo_zonmean() +
    theme(aspect.ratio = 1)
  p <- p + theme(legend.position = "top")
  ggsave("~/fig_flex_zonal_origin.png", p, width = 5, height = 5)

  p <- df %>%
    ggplot() +
    geom_tile(aes(x = lat, y = height_bin * 1E-3, fill = dt_cloud)) +
    geom_line(data = df_ta, aes(x = lat, y = height_40 * 1E-3), color = "black", size = 0.5) +
    scale_fill_distiller("Hours since formation",
                         palette = "Spectral", limits = c(0, 120)) +
    scale_y_continuous("Altitude (km)", limits = c(0, 20), expand = c(0, 0)) +
    plotutils::scale_x_geo_zonmean() +
    theme(aspect.ratio = 1)

  p <- p + theme(legend.position = "top")

  ggsave("~/fig_flex_zonal_dt-cloud.png", p, width = 5, height = 5)

  return(NULL)

}
