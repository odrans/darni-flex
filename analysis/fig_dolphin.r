fig_dolphin <- function(df_in) {

  bins_origin <- c(0, 0.01, 1)
  bins_origin_lev <- baseutils::bins2lev(bins_origin)
  bins_origin_lab <- c("in-situ", "]50-100%]")

  bins_iwp <- 10^seq(-6, 7, by = 0.05)
  bins_icncc <- 10^seq(1, 11, by = 0.05)

  bins_time <- c(0, 2, 10, 120)

  df_all <- df_in %>%
    dplyr::mutate(iwp = iwp * 1E-3,
                  icncc = icncc * 1E6
                  ) %>%
    dplyr::ungroup() %>%
    plotutils::bin(origin_cloud, bins = bins_origin) %>%
    plotutils::bin(iwp, bins = bins_iwp) %>%
    plotutils::bin(icncc, bins = bins_icncc) %>%
    dplyr::filter(!is.na(origin_cloud_bin), !is.na(iwp_bin), !is.na(icncc_bin))

  df_origin <- df_all %>%
    dplyr::group_by(iwp_bin, origin_cloud_bin) %>%
    dplyr::mutate(count_iwp = n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(icncc_bin, iwp_bin, origin_cloud_bin) %>%
    dplyr::summarise(count = n(), density_iwp = n() / count_iwp[1],
                     .groups = "keep") %>%
    data.frame() %>%
    dplyr::mutate(origin_cloud_bin = factor(origin_cloud_bin, levels = bins_origin_lev, labels = bins_origin_lab)) %>%
    dplyr::rename(origin_cloud = origin_cloud_bin,
                  iwp = iwp_bin,
                  icncc = icncc_bin)

  df <- df_all %>%
    dplyr::group_by(iwp_bin) %>%
    dplyr::mutate(count_iwp = n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(icncc_bin, iwp_bin) %>%
    dplyr::summarise(count = n(), density_iwp = n() / count_iwp[1],
                     .groups = "keep") %>%
    data.frame() %>%
    dplyr::rename(iwp = iwp_bin,
                  icncc = icncc_bin)

  p <- df_origin %>%
    rename(var = density_iwp) %>%
    mutate(var = pmin(var, 0.25)) %>%
    ggplot() +
    geom_tile(aes(x = iwp, y = icncc, fill = var)) +
    scale_x_continuous(limits = c(1E-6, 1E2), expand = c(0, 0), trans = "log10") +
    scale_y_continuous(limits = c(1E3, 1E11), expand = c(0, 0), trans = "log10") +
    scale_fill_distiller(palette = "Spectral", limits = c(0, 0.25)) +
    theme(aspect.ratio = 1)  +
    facet_wrap(~origin_cloud)

  ggsave("~/fig_flex_iwp_icncc_origin.png", p, width = 10, height = 5)

  p <- df %>%
    rename(var = density_iwp) %>%
    mutate(var = pmin(var, 0.25)) %>%
    ggplot() +
    geom_tile(aes(x = iwp, y = icncc, fill = var)) +
    scale_x_continuous(limits = c(1E-6, 1E2), expand = c(0, 0), trans = "log10") +
    scale_y_continuous(limits = c(1E3, 1E11), expand = c(0, 0), trans = "log10") +
    scale_fill_distiller(palette = "Spectral", limits = c(0, 0.25)) +
    theme(aspect.ratio = 1)

  ggsave("~/fig_flex_iwp_icncc.png", p, width = 10, height = 5)
  
  rm(df_all, df, df_origin); gc()

  return(NULL)

}
