lonlat_cartesian <- function(X, Y) {
  phi <- ((90 - Y) * pi) / 180
  theta <- (X * pi) / 180

  x_cart <- sin(phi) * cos(theta)
  y_cart <- sin(phi) * sin(theta)
  z_cart <- cos(phi)

  list(x = x_cart, y = y_cart, z = z_cart)
}

cartesian_lonlat <- function(x, y, z) {
  lat <- 90 - ((180 * acos(z)) / pi)
  lon <- ifelse(
    x > 0,
    atan(y / x) * (180 / pi),
    ifelse(
      y > 0,
      atan(y / x) * (180 / pi) + 180,
      atan(y / x) * (180 / pi) - 180
    )
  )

  list(X = lon, Y = lat)
}

x_checks <- function(x, x_name) {
  if (!inherits(x, "sf")) {
    stop("`", x_name, "` is not an simple features object")
  }
  allowed_geom <- c("POINT", "POLYGON", "MULTIPOINT", "MULTIPOLYGON")
  if (any(!(as.character(sf::st_geometry_type(x)) %in% allowed_geom))) {
    stop("`", x_name, "` does not have only point or polygon geometries")
  }
  if (is.na(sf::st_crs(x))) {
    stop("`", x_name, "` does not have defined coordinate reference system")
  }
  if (any(sf::st_is_empty(x))) {
    stop("`", x_name, "` contains empty geometries")
  }
}

x_processing <- function(x, weight) {
  coordinates <- suppressWarnings(sf::st_centroid(x)) |>
    sf::st_coordinates()  |>
      tibble::as_tibble() |>
      tibble::tibble(geometry = _)
    
  x |>
    sf::st_drop_geometry() |>
    tibble::tibble() |>
    dplyr::bind_cols(coordinates)
}

weight_checks <- function(x, x_name, weight) {
  if (is.null(weight)) {
    return(NULL)
  }
  if (!is.character(weight)) {
    stop("weight argument is not a character")
  }
  if (length(weight) != 1) {
    stop("weight argument length is not 1")
  }

  if (!(weight %in% colnames(x))) {
    stop(x_name, "$`", weight, "` does not exist")
  }
  if (!is.numeric(x[[weight]])) {
    stop(x_name, "$`", weight, "` is not numeric")
  }
  if (any(is.na(x[[weight]]))) {
    stop(x_name, "$`", weight, "` contains at least one missing value")
  }
  if (any(x[[weight]] < 0)) {
    stop(x_name, "$`", weight, "` contains at least one negative value")
  }
}

group_checks <- function(x, x_name, group) {
  if (is.null(group)) {
    return(NULL)
  } 
  if (!is.character(group)) {
    stop("group argument is not a character")
  }
  if (length(group) == 0) {
    stop("group argument length is zero")
  }

  if (length(unique(group)) != length(group)) {
    stop("group argument contains repeated values")
  }
  for (grp in group) {
    if (!(grp %in% colnames(x))) {
      stop(x_name, "$`", grp, "` is not numeric")
    }
  }
}

output_processing <- function(centers, x) {
  output <- centers |>
    dplyr::mutate(
      geometry = sf::st_as_sfc(sf::st_as_sf(
        geometry, 
        coords = c("X", "Y"), 
        crs = sf::st_crs(x),
        na.fail = FALSE
      ))
    ) |>
    as.data.frame() |>
    sf::st_as_sf()

  rownames(output) <- NULL
  class(output) <- class(x)

  output
}

# do.call with extra params
do_call <- function(what, ...) {
  list_of_args <- list(...)

  is_list <- sapply(list_of_args, is.list)
  lists <- unlist(list_of_args[is_list], recursive = FALSE)
  non_lists <- list_of_args[!is_list]

  args <- c(lists, non_lists)

  do.call(what, args)
}
