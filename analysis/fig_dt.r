fig_dt <- function(df_in, test = FALSE) {

  limits_time <- c(0, 120)

  bins_ta <- c(-100, -40, 0)
  bins_ta_lev <- baseutils::bins2lev(bins_ta)
  bins_ta_label <- c("T < -40C", "T > -40C")
  bins_dt <- seq(0, 130, by = 1)

  if(test) df_in <- slice(df_in, 1:1E7)

  xx <- df_in %>%
    filter(season %in% c("DJF", "JJA")) %>%
    mutate(ta = ta_origin) %>%
    filter(dt_cloud < 125) %>%
    filter(origin %in% c(0, 1)) %>%
    plotutils::bin(dt_cloud, bins_dt) %>%
    ## plotutils::bin(ta, bins_ta) %>%
    filter(!is.na(iwc), !is.na(icnc), !is.na(dt_cloud)) %>%
    group_by(dt_cloud_bin, origin, season) %>%
    summarize(iwc_50 = quantile(iwc, 0.5) * 1E6,
              icnc_50 = quantile(icnc, 0.5) * 1E-3,
              count = n(),
              .groups = "drop") %>%
    data.frame() %>%
    mutate(origin = factor(origin, levels = c(0, 1), labels = c("in-situ", "liquid")))
##           ta_bin = factor(ta_bin, levels = bins_ta_lev, labels = bins_ta_label))

  p_count <- xx %>%
    ggplot(aes(x = dt_cloud_bin)) +
    geom_histogram(aes(y = count, fill = origin), stat = "identity", position = "stack") +
    scale_x_continuous("", limits = limits_time, expand = c(0, 0)) +
    scale_y_continuous("Count", expand = c(0, 0)) +
    scale_color_manual(values = c("in-situ" = "deepskyblue1", "liquid" = "brown1")) +
    scale_fill_manual(values = c("in-situ" = "deepskyblue1", "liquid" = "brown1")) +
    theme(aspect.ratio = 0.4) +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    facet_wrap(~season, scales = "free_y")

  p_iwc <- xx %>%
    ggplot(aes(x = dt_cloud_bin)) +
    geom_path(aes(y = iwc_50, color = origin)) +
    ##geom_ribbon(aes(ymin = iwc_25, ymax = iwc_75, fill = origin), alpha = 0.5) +
    scale_x_continuous("", limits = limits_time, expand = c(0, 0)) +
    scale_y_continuous(expression(IWC~(mg~m^-3)), expand = c(0, 0), trans = "log10", limits = c(1, 1e3)) +
    scale_color_manual(values = c("in-situ" = "deepskyblue1", "liquid" = "brown1")) +
    scale_fill_manual(values = c("in-situ" = "deepskyblue1", "liquid" = "brown1")) +
    theme(aspect.ratio = 0.4) +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    facet_wrap(~season)

  p_icnc <- xx %>%
    ggplot(aes(x = dt_cloud_bin)) +
    geom_path(aes(y = icnc_50, color = origin)) +
    ##geom_ribbon(aes(ymin = icnc_25, ymax = icnc_75, fill = origin), alpha = 0.5) +
    scale_x_continuous("Hours since ice formation", expand = c(0, 0), limits = limits_time) +
    scale_y_continuous(expression(ICNC~(L^-1)), expand = c(0, 0), trans = "log10", limits = c(1e0, 1e3)) +
    scale_color_manual(values = c("in-situ" = "deepskyblue1", "liquid" = "brown1")) +
    scale_fill_manual(values = c("in-situ" = "deepskyblue1", "liquid" = "brown1")) +
    theme(aspect.ratio = 0.4) +
    facet_wrap(~season)

  p_count <- p_count + theme(legend.position = "top")
  p_iwc <- p_iwc + theme(legend.position = "none")
  p_icnc <- p_icnc + theme(legend.position = "none")

  p <- p_count / p_iwc / p_icnc
  ggsave("~/fig_flex_dt_cloud_iwc.png", p, width = 6, height = 5)

  return(NULL)

}
