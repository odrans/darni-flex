load_dardar_origin_2D <- function(fn_dardar_origin, lf_dardar, lf_dardar_flux) {

  idx_name_dardar <- gsub(".nc", "", strsplit(basename(fn_dardar_origin), "_", fixed = TRUE)[[1]][5])
  fn_dardar <- lf_dardar[which(grepl(idx_name_dardar, lf_dardar))]
  fn_dardar_flux <- lf_dardar_flux[which(grepl(idx_name_dardar, lf_dardar_flux))]
  if(!length(fn_dardar) | !length(fn_dardar_flux)) {
    return(NULL)
  }

  nc_dardar_origin <- ncdf4::nc_open(fn_dardar_origin)
  nc_dardar <- ncdf4::nc_open(fn_dardar)
  nc_dardar_flux <- ncdf4::nc_open(fn_dardar_flux)

  len_height_origin <- nc_dardar_origin$dim$height$len
  len_time_origin <- nc_dardar_origin$dim$time$len

  len_height <- nc_dardar$dim$height$len
  len_time <- nc_dardar$dim$time$len

  len_time_flux <- nc_dardar_flux$dim$point$len

  if(len_height != len_height_origin | len_time != len_time_origin | len_time != len_time_flux) {
    return(NULL)
  }

  df <- data.frame(expand.grid(idx_height = 1:len_height_origin,
                               idx_time = 1:len_time_origin)) %>%
    dplyr::mutate(
             origin = c(ncdf4::ncvar_get(nc_dardar_origin, "origin")),
             ##origin_flag = c(ncdf4::ncvar_get(nc_dardar_origin, "origin_flag")),
             layer_index = c(ncdf4::ncvar_get(nc_dardar, "layer_index")),
             dz_top = c(ncdf4::ncvar_get(nc_dardar, "dz_top")),
             iwc = c(ncdf4::ncvar_get(nc_dardar, "iwc")),
             icnc = c(ncdf4::ncvar_get(nc_dardar, "icnc_5um")),
             ta = c(ncdf4::ncvar_get(nc_dardar, "ta")),
             clm = c(ncdf4::ncvar_get(nc_dardar, "clm")),
             flag_mixed = c(ncdf4::ncvar_get(nc_dardar, "mixedphase_flag"))
             ) %>%
    dplyr::filter(!is.na(origin), !is.na(layer_index), iwc > 1E-8, clm == 1, flag_mixed == 0) %>%
    dplyr::group_by(idx_time, layer_index) %>%
    dplyr::summarize(
             origin_cloud = mean(origin),
             H = max(dz_top) *1E-3,
             iwp = sum(iwc) * 60 * 1E3,
             icncc = sum(icnc) * 60 * 1E-6,
             ctt = ta[which.min(dz_top)],
             .groups = "drop"
             ) %>%
    data.frame() %>%
    dplyr::mutate(
             lat = c(ncdf4::ncvar_get(nc_dardar, "lat"))[idx_time],
             lon = c(ncdf4::ncvar_get(nc_dardar, "lon"))[idx_time],
             cre_ice_sw = c(ncdf4::ncvar_get(nc_dardar_flux, "cre_ice_sw"))[idx_time],
             cre_ice_lw = c(ncdf4::ncvar_get(nc_dardar_flux, "cre_ice_lw")[idx_time]),
             iteration_flag =  c(ncdf4::ncvar_get(nc_dardar, "iteration_flag")[idx_time]),
             time = ncdf4::ncvar_get(nc_dardar, "dtime")[idx_time] + ncdf4::ncvar_get(nc_dardar, "base_time") + as.POSIXct("1970-01-01"),
             season = baseutils::time2season(time)
           ) %>%
    dplyr::filter(iteration_flag == 1) %>%
    dplyr::select(-c(layer_index, iteration_flag, time))


  ncdf4::nc_close(nc_dardar_origin)
  ncdf4::nc_close(nc_dardar)
  ncdf4::nc_close(nc_dardar_flux)

  return(df)

}



load_dardar_origin_3D <- function(fn_dardar_origin, lf_dardar, dir_rds) {

  fn_out <- paste0(dir_rds, "/3D/", basename(fn_dardar_origin))
  if(file.exists(fn_out)) {
    return(NULL)
  }

  idx_name_dardar <- gsub(".nc", "", strsplit(basename(fn_dardar_origin), "_", fixed = TRUE)[[1]][5])
  fn_dardar <- lf_dardar[which(grepl(idx_name_dardar, lf_dardar))]
  if(!length(fn_dardar)) {
    return(NULL)
  }

  nc_dardar_origin <- ncdf4::nc_open(fn_dardar_origin)
  nc_dardar <- ncdf4::nc_open(fn_dardar)

  len_height_origin <- nc_dardar_origin$dim$height$len
  len_time_origin <- nc_dardar_origin$dim$time$len

  len_height <- nc_dardar$dim$height$len
  len_time <- nc_dardar$dim$time$len

  if(len_height != len_height_origin | len_time != len_time_origin) {
    return(NULL)
  }

  df <- data.frame(expand.grid(idx_height = 1:len_height_origin,
                               idx_time = 1:len_time_origin)) %>%
    dplyr::mutate(
             origin = c(ncdf4::ncvar_get(nc_dardar_origin, "origin")),
             dt_cloud = c(ncdf4::ncvar_get(nc_dardar_origin, "dt_cloud")),
             #origin_flag = c(ncdf4::ncvar_get(nc_dardar_origin, "origin_flag")),
             iwc = c(ncdf4::ncvar_get(nc_dardar, "iwc")),
             icnc = c(ncdf4::ncvar_get(nc_dardar, "icnc_5um")),
             clm = c(ncdf4::ncvar_get(nc_dardar, "clm")),
             ta = c(ncdf4::ncvar_get(nc_dardar, "ta")),
             flag_mixed = c(ncdf4::ncvar_get(nc_dardar, "mixedphase_flag")),
             dz_top = c(ncdf4::ncvar_get(nc_dardar, "dz_top")),
             layer_index = c(ncdf4::ncvar_get(nc_dardar, "layer_index")),
             ta_origin = c(ncdf4::ncvar_get(nc_dardar_origin, "ta_origin"))
             ) %>%
    dplyr::filter(!is.na(origin), iwc > 1E-8, clm == 1, flag_mixed == 0) %>%
    dplyr::mutate(
             lat = c(ncdf4::ncvar_get(nc_dardar, "lat"))[idx_time],
             lon = c(ncdf4::ncvar_get(nc_dardar, "lon"))[idx_time],
             height = c(ncdf4::ncvar_get(nc_dardar, "height"))[idx_height],
             iteration_flag =  c(ncdf4::ncvar_get(nc_dardar, "iteration_flag")[idx_time]),
             time = ncdf4::ncvar_get(nc_dardar, "dtime")[idx_time] + ncdf4::ncvar_get(nc_dardar, "base_time") + as.POSIXct("1970-01-01"),
             season = baseutils::time2season(time)
           ) %>%
    dplyr::filter(iteration_flag == 1) %>%
    dplyr::select(-c(iteration_flag, flag_mixed, clm, time, idx_height, idx_time)) %>%
    dplyr::filter(!is.na(dt_cloud))

  ncdf4::nc_close(nc_dardar_origin)
  ncdf4::nc_close(nc_dardar)

  saveRDS(df, file = fn_out)

  return(NULL)

}
