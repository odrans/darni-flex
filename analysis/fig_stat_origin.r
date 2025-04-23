fig_stat_origin <- function(df_grid_3D) {

  bins_lat_region <- c(-90, -67.7, -23.3, 23.3, 67.7, 90)
  lab_lat_region <- c("Antarctic","Mid-lat S.","Tropics","Mid-lat N.","Arctic")

  bins_origin <- c(0, 0.05, 0.5, 1)
  bins_origin_lev <- baseutils::bins2lev(bins_origin)
  bins_origin_lab <- c("in-situ", "]0-50%]", "]50-100%]")

  df <- df_grid_3D %>%
    plotutils::bin(origin_cloud, bins = bins_origin) %>%
    plotutils::bin(lat, bins_lat_region) %>%
    mutate(region = factor(lat_bin, levels = baseutils::bins2lev(bins_lat_region), labels = lab_lat_region),
           origin_cloud_bin = factor(origin_cloud_bin, levels = bins_origin_lev, labels = bins_origin_lab)) %>%
     select(c(region, origin_cloud_bin, iwp, icncc, H, ctt, cre_ice_sw, cre_ice_lw)) %>%
    rename(origin = origin_cloud_bin)
  gc()

 df <- rbind(df, df %>% mutate(region = factor("Global")))

  df_var <- df %>%
    tidyr::pivot_longer(c("iwp", "icncc", "H", "ctt"), names_to = "var", values_to = "val") %>%
    group_by(origin, region, var) %>%
    summarize(val_50 = quantile(val, 0.5, na.rm = TRUE),
              val_25 = quantile(val, 0.25, na.rm = TRUE),
              val_75 = quantile(val, 0.75, na.rm = TRUE),
              val_10 = quantile(val, 0.1, na.rm = TRUE),
              val_90 = quantile(val, 0.9, na.rm = TRUE),
              .groups = "drop") %>%
    data.frame()

  df_cre <- df %>%
    mutate(cre_ice = cre_ice_sw + cre_ice_lw) %>%
    tidyr::pivot_longer(c("cre_ice_sw", "cre_ice_lw", "cre_ice"), names_to = "var", values_to = "val") %>%
    group_by(origin, region, var) %>%
    summarize(val_50 = quantile(val, 0.5, na.rm = TRUE),
              val_25 = quantile(val, 0.25, na.rm = TRUE),
              val_75 = quantile(val, 0.75, na.rm = TRUE),
              val_10 = quantile(val, 0.1, na.rm = TRUE),
              val_90 = quantile(val, 0.9, na.rm = TRUE),
              .groups = "drop") %>%
    data.frame()

  df_count <- df %>%
    group_by(origin, region) %>%
    summarize(count = n(),
              .groups = "drop") %>%
    data.frame()
  rm(df); gc()

  p_count <- df_count %>%
    ggplot(aes(x = region, y = count, fill = origin)) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    scale_fill_manual("", values = c("in-situ" = "deepskyblue1", "]0-50%]" = "darkgoldenrod2", "]50-100%]" = "brown1")) +
    #scale_fill_manual("", values = c("in-situ" = "deepskyblue1", "]0-100%]" = "brown1")) +
    scale_y_continuous("#") +
    theme_bw() +
    theme(aspect.ratio = 0.4)

  p_iwc <- df_var %>%
    filter(var %in% c("iwp")) %>%
    ggplot(aes(x = region, y = val_50, fill = origin)) +
    geom_boxplot(aes(lower = val_25, middle = val_50, upper = val_75, ymin = val_10, ymax = val_90), stat = "identity") +
    scale_fill_manual("", values = c("in-situ" = "deepskyblue1", "]0-50%]" = "darkgoldenrod2", "]50-100%]" = "brown1")) +
    #scale_fill_manual("", values = c("in-situ" = "deepskyblue1", "]0-100%]" = "brown1")) +
    scale_y_log10("IWP (g m-2)") +
    theme_bw() +
    theme(aspect.ratio = 0.4)

  p_icncc <- df_var %>%
    filter(var %in% c("icncc")) %>%
    ggplot(aes(x = region, y = val_50, fill = origin)) +
    geom_boxplot(aes(lower = val_25, middle = val_50, upper = val_75, ymin = val_10, ymax = val_90), stat = "identity") +
    scale_fill_manual("", values = c("in-situ" = "deepskyblue1", "]0-50%]" = "darkgoldenrod2", "]50-100%]" = "brown1")) +
    #scale_fill_manual("", values = c("in-situ" = "deepskyblue1", "]0-100%]" = "brown1")) +
    scale_y_log10("Ni burden (10^6 m-2)") +
    scale_x_discrete("") +
    theme_bw() +
    theme(aspect.ratio = 0.4)

  p_H <- df_var %>%
    filter(var %in% c("H")) %>%
    ggplot(aes(x = region, y = val_50, fill = origin)) +
    geom_boxplot(aes(lower = val_25, middle = val_50, upper = val_75, ymin = val_10, ymax = val_90), stat = "identity") +
    scale_fill_manual("", values = c("in-situ" = "deepskyblue1", "]0-50%]" = "darkgoldenrod2", "]50-100%]" = "brown1")) +
    #scale_fill_manual("", values = c("in-situ" = "deepskyblue1", "]0-100%]" = "brown1")) +
    scale_y_continuous("H (km)") +
    theme_bw() +
    theme(aspect.ratio = 0.4)

  p_ctt <- df_var %>%
    filter(var %in% c("ctt")) %>%   ggplot(aes(x = region, y = val_50, fill = origin)) +
    geom_boxplot(aes(lower = val_25, middle = val_50, upper = val_75, ymin = val_10, ymax = val_90), stat = "identity") +
    scale_fill_manual("", values = c("in-situ" = "deepskyblue1", "]0-50%]" = "darkgoldenrod2", "]50-100%]" = "brown1")) +
    #scale_fill_manual("", values = c("in-situ" = "deepskyblue1", "]0-100%]" = "brown1")) +
    scale_y_continuous("CTT (C)") +
    theme_bw() +
    theme(aspect.ratio = 0.4)

  p_H <- p_H + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  p_iwc <- p_iwc + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  p_count <- p_count + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  p_ctt <- p_ctt + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  p_icncc <- p_icncc + theme(axis.text.x = element_text(angle = 45, hjust = 1))

  p_icncc <- p_icncc + theme(legend.position = "none")
  p_ctt <- p_ctt + theme(legend.position = "none")
  p_iwc <- p_iwc + theme(legend.position = "none")
  p_H <- p_H + theme(legend.position = "none")
  p_count <- p_count + theme(legend.position = "top")

  p <- p_count / p_ctt / p_H / p_iwc / p_icncc
  ggsave("~/fig_flex_stats_origin.png", p, width = 6, height = 10)

  p_cre_lw <- df_cre %>%
    filter(var %in% c("cre_ice_lw")) %>%
    ggplot(aes(x = region, y = val_50, fill = origin)) +
    geom_boxplot(aes(lower = val_25, middle = val_50, upper = val_75, ymin = val_10, ymax = val_90), stat = "identity") +
    scale_fill_manual("", values = c("in-situ" = "deepskyblue1", "]0-50%]" = "darkgoldenrod2", "]50-100%]" = "brown1")) +
    #scale_fill_manual("", values = c("in-situ" = "deepskyblue1", "]0-100%]" = "brown1")) +
    scale_x_discrete("") +
    scale_y_continuous("CRE LW (W m-2)") +
    theme_bw() +
    theme(aspect.ratio = 0.4)

  p_cre_sw <- df_cre %>%
    filter(var %in% c("cre_ice_sw")) %>%
    ggplot(aes(x = region, y = val_50, fill = origin)) +
    geom_boxplot(aes(lower = val_25, middle = val_50, upper = val_75, ymin = val_10, ymax = val_90), stat = "identity") +
    scale_fill_manual("", values = c("in-situ" = "deepskyblue1", "]0-50%]" = "darkgoldenrod2", "]50-100%]" = "brown1")) +
    #scale_fill_manual("", values = c("in-situ" = "deepskyblue1", "]0-100%]" = "brown1")) +
    scale_x_discrete("") +
    scale_y_continuous("CRE SW (W m-2)") +
    theme_bw() +
    theme(aspect.ratio = 0.4)

  p_cre <- df_cre %>%
    filter(var %in% c("cre_ice")) %>%
    ggplot(aes(x = region, y = val_50, fill = origin)) +
    geom_boxplot(aes(lower = val_25, middle = val_50, upper = val_75, ymin = val_10, ymax = val_90), stat = "identity") +
    geom_hline(yintercept = 0, linetype = 2) +
    scale_fill_manual("", values = c("in-situ" = "deepskyblue1", "]0-50%]" = "darkgoldenrod2", "]50-100%]" = "brown1")) +
    #scale_fill_manual("", values = c("in-situ" = "deepskyblue1", "]0-100%]" = "brown1")) +
    scale_x_discrete("") +
    scale_y_continuous("CRE Net (W m-2)") +
    theme_bw() +
    theme(aspect.ratio = 0.4)

  p_cre_sw <- p_cre_sw + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  p_cre_lw <- p_cre_lw + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

  p_cre_sw <- p_cre_sw + theme(legend.position = "top")
  p_cre_lw <- p_cre_lw + theme(legend.position = "none")
  p_cre <- p_cre + theme(legend.position = "none")

  p <- p_cre_sw / p_cre_lw / p_cre
  ggsave("~/fig_flex_stats_origin_cre.png", p, width = 6, height = 6)


  return(NULL)

}
