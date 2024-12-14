chk_not_any_empty_sf <- function(x, x_name = NULL) {
  if (vld_not_any_empty_sf(x)) {
    return(invisible(x))
  }
  if (is.null(x_name)) x_name <- chk::deparse_backtick_chk(substitute(x))
  chk::abort_chk(x_name, " must not have any empty geometries", x = x)
}
vld_not_any_empty_sf <- function(x) !any(sf::st_is_empty(x))

allowed_geom <- c("POINT", "POLYGON", "MULTIPOINT", "MULTIPOLYGON")
chk_only_allowed_sf <- function(x, x_name = NULL) {
  if (vld_only_allowed_sf(x)) {
    return(invisible(x))
  }
  if (is.null(x_name)) x_name <- chk::deparse_backtick_chk(substitute(x))
  chk::abort_chk(x_name, " must only have point or polygon geometries", x = x)
}
vld_only_allowed_sf <- function(x) all(as.character(sf::st_geometry_type(x)) %in% allowed_geom)

chk_not_na_crs <- function(x, x_name = NULL) {
  if (vld_not_na_crs(x)) {
    return(invisible(x))
  }
  if (is.null(x_name)) x_name <- chk::deparse_backtick_chk(substitute(x))
  chk::abort_chk(x_name, " must only have point or polygon geometries", x = x)
}
vld_not_na_crs <- function(x) !is.na(sf::st_crs(x))

chk_columns_exist <- function(x, columns, x_name = NULL) {
  columns_exist <- vld_columns_exist(x, columns)
  if (all(columns_exist)) {
    return(invisible(x))
  }
  if (is.null(x_name)) x_name <- chk::deparse_backtick_chk(substitute(x))
  
  chk::abort_chk("Can't find column `", columns[which(!columns_exist)][1], "` in ", x_name, x = x)
}
vld_columns_exist <- function(x, columns) columns %in% colnames(x)

chk_not_any_infinite <- function(x, x_name = NULL) {
  if (vld_not_any_infinite(x)) {
    return(invisible(x))
  }
  if (is.null(x_name)) x_name <- chk::deparse_backtick_chk(substitute(x))
  chk::abort_chk(x_name, " must not have any infinite values", x = x)
}
vld_not_any_infinite <- function(x) !any(is.infinite(x[!is.na(x)]))

chk_is_not_lonlat <- function(x, x_name = NULL) {
  if (vld_is_not_lonlat(x)) {
    return(invisible(x))
  }
  if (is.null(x_name)) x_name <- chk::deparse_backtick_chk(substitute(x))
  chk::abort_chk(x_name, " must have projected coordinates", x = x)
}
vld_is_not_lonlat <- function(x) !sf::st_is_longlat(x)
