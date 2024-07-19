set.seed(1)
n <- 1000

df <- data.frame(
  lon = runif(n, min = -124.848974, max = -66.885444),
  lat = runif(n, min = 24.396308, max = 49.384358),
  grp = sample(c("a", "b", "c"), n, replace = TRUE),
  wts = sample.int(5, n, replace = TRUE)
)
x <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
x_proj <- sf::st_transform(x, crs = "ESRI:102003")

df_rep <- df[rep(row.names(df), df$wts), ]
# Introduce some variation in the points,
# so the same point is not seen as the same
df_rep$lon <- df_rep$lon + runif(nrow(df_rep), min = 0, max = 0.00000001)
x_rep <- sf::st_as_sf(df_rep, coords = c("lon", "lat"), crs = 4326)
x_rep_proj <- sf::st_transform(x_rep, crs = "ESRI:102003")

x_grp <- do.call("c", lapply(unique(x$grp),
  function(grp) {
    sf::st_union(x[x$grp == grp, ])
  }
))
x_grp_proj <- sf::st_transform(x_grp, crs = "ESRI:102003")

x_rep_grp <- do.call("c", lapply(unique(x_rep$grp),
  function(grp) {
    sf::st_union(x_rep[x_rep$grp == grp, ])
  }
))
x_rep_grp_proj <- sf::st_transform(x_rep_grp, crs = "ESRI:102003")
