fig_profiles <- function(df_grid_3D) {

  bins_dz_top_frac <- seq(0, 1, by = 0.05)
  bins_origin <- c(0, 0.05, 0.5, 1)
  bins_origin_lev <- baseutils::bins2lev(bins_origin)
  bins_origin_lab <- c("in-situ", "]0-50%]", "]50-100%]")

  xx <- df_grid_3D %>%
    filter(origin %in% c(0, 1)) %>%
    group_by(lat, lon, layer_index) %>%
    mutate(origin_cloud = mean(origin),
           dz_top_max = max(dz_top),
           dz_top_frac = dz_top / dz_top_max
           ) %>%
    ungroup() %>%
    plotutils::bin(dz_top_frac, bins = bins_dz_top_frac) %>%
    plotutils::bin(origin_cloud, bins = bins_origin) %>%
    filter(!is.na(dz_top_frac)) %>%
    group_by(dz_top_frac_bin, origin_cloud_bin) %>%
    summarize(origin = mean(origin),
              iwc = mean(iwc) * 1E6,
              icnc = mean(icnc) * 1E-3,
              dt_cloud = mean(dt_cloud, na.rm = TRUE),
              .groups = "keep") %>%
    data.frame() %>%
    mutate(origin_cloud_bin = factor(origin_cloud_bin, levels = bins_origin_lev, labels = bins_origin_lab))

  p <- xx %>%

    tidyr::pivot_longer(c("origin", "dt_cloud", "iwc", "icnc"), names_to = "var", values_to = "val") %>%
    mutate(var = factor(var, levels = c("origin", "dt_cloud", "iwc", "icnc"),
                        labels = c("Liquid-origin fraction", "Hours since formation", "IWC (mg m-3)", "Ni (#/L)"))) %>%
    ggplot(aes(y = 1 - dz_top_frac_bin)) +
    geom_path(aes(x = val, color = origin_cloud_bin)) +
    scale_x_continuous("") +
    scale_y_continuous("Relative position in cloud", limits = c(0, 1),
                       breaks = c(0, 0.25, 0.5, 0.75, 1),
                       labels = c("Base", "", "Mid", "", "Top")
                       ) +
    scale_color_manual("", values = c("in-situ" = "deepskyblue1", "]0-50%]" = "darkgoldenrod2", "]50-100%]" = "brown1")) +
    theme(aspect.ratio = 1) +
    facet_wrap(~var, scales = "free_x")
  ggsave("~/fig_flex_dz_top_frac.png", p, width = 10, height = 5)

  return(NULL)


}
