has_null_crs <- function(x) {
  if (inherits(x, "sf")) {
    bool <- is.na(sf::st_crs(x))
  }
  if (inherits(x, "SpatVector")) {
    bool <- terra::crs(x) == ""
  }
  bool
}

is_lonlat <- function(x) {
  if (inherits(x, "sf")) {
    bool <- sf::st_is_longlat(x)
  }
  if (inherits(x, "SpatVector")) {
    bool <- terra::is.lonlat(x)
  }
  bool
}

has_empty_geom <- function(x) {
  if (inherits(x, "sf")) {
    bool <- any(sf::st_is_empty(x))
  }
  if (inherits(x, "SpatVector")) {
    bool <- terra::is.empty(x) || any(is.na(x))
  }
  bool
}

is_point_or_poly <- function(x) {
  if (inherits(x, "sf")) {
    allowed_geom <- c("POINT", "POLYGON", "MULTIPOINT", "MULTIPOLYGON")
    bool <- all(as.character(sf::st_geometry_type(x)) %in% allowed_geom)
  }
  if (inherits(x, "SpatVector")) {
    bool <- terra::is.points(x) || terra::is.polygons(x)
  }
  bool
}

centroids <- function(x) {
  if (inherits(x, "sf")) {
    x <- suppressWarnings(sf::st_centroid(x))
  }
  if (inherits(x, "SpatVector")) {
    x <- terra::centroids(x)
  }
  x
}

coordinates <- function(x) {
  if (inherits(x, "sf")) {
    coords <- sf::st_coordinates(x)
  }
  if (inherits(x, "SpatVector")) {
    coords <- terra::crds(x)
  }
  colnames(coords) <- c("X", "Y")
  as.data.frame(coords)
}

# standardizes the accessor for sf and terra
pluck <- function(x, accessor) {
  if (inherits(x, "sf")) {
    val <- x[[accessor]]
  }
  if (inherits(x, "SpatVector")) {
    val <- x[[accessor, drop = TRUE]]
  }
  val
}
