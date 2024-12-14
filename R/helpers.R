lonlat_cartesian <- function(X, Y, ...) {
  phi <- ((90 - Y) * pi) / 180
  theta <- (X * pi) / 180

  x_cart <- sin(phi) * cos(theta)
  y_cart <- sin(phi) * sin(theta)
  z_cart <- cos(phi)

  tibble::tibble(x = x_cart, y = y_cart, z = z_cart, ...)
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

  tibble::tibble(X = lon, Y = lat)
}
