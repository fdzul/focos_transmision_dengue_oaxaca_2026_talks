
suppressMessages({library(sf); library(leaflet); library(mapview); library(mapgl)})
Sys.setenv(RSTUDIO_PANDOC = dirname(Sys.which("pandoc")))
set.seed(1)
bb <- st_bbox(c(xmin=-96.80, ymin=17.02, xmax=-96.65, ymax=17.12), crs = 4326)
pts <- function(n) st_as_sf(data.frame(id=1:n, semana=sample(1:52,n,TRUE),
        lon=runif(n,bb["xmin"],bb["xmax"]), lat=runif(n,bb["ymin"],bb["ymax"])),
        coords=c("lon","lat"), crs=4326)
pols <- function(n) { g <- st_make_grid(st_as_sfc(bb), n = c(ceiling(sqrt(n)), ceiling(sqrt(n))), square=FALSE)
        st_sf(id=seq_along(g), riesgo=sample(c("Muy alto","Alto","Moderado","Bajo"),length(g),TRUE), geometry=g) }
mk <- list(
  leaflet = function(x) { if (inherits(st_geometry(x),"sfc_POINT"))
      leaflet(x) |> addTiles() |> addCircleMarkers(radius=3, stroke=FALSE, fillOpacity=.8)
    else leaflet(x) |> addTiles() |> addPolygons(weight=1, fillOpacity=.6) },
  mapview = function(x) mapview(x, zcol = names(x)[2])@map,
  mapgl = function(x) { m <- maplibre(bounds = x)
    if (inherits(st_geometry(x),"sfc_POINT")) m |> add_circle_layer(id="l", source=x, circle_radius=3, circle_color="#d7301f")
    else m |> add_fill_layer(id="l", source=x, fill_color="#d7301f", fill_opacity=.6) })
dsize <- function(d) sum(file.info(list.files(d, recursive=TRUE, full.names=TRUE))$size)
res <- list()
for (tipo in c("puntos","poligonos")) for (n in c(0, 1000, 10000, 50000)) {
  x <- if (n == 0) pts(1) else if (tipo=="puntos") pts(n) else pols(n)
  for (pk in names(mk)) {
    d <- file.path("salida_benchmark", paste(pk,tipo,n,sep="_")); dir.create(d, recursive=TRUE, showWarnings=FALSE)
    w <- mk[[pk]](x)
    htmlwidgets::saveWidget(w, file.path(d,"index.html"), selfcontained = FALSE, libdir = "lib")
    res[[length(res)+1]] <- data.frame(paquete=pk, tipo=tipo, n=if(n==0) 0 else nrow(x),
       html_kb=round(file.size(file.path(d,"index.html"))/1024), total_kb=round(dsize(d)/1024),
       lib_kb=round(dsize(file.path(d,"lib"))/1024), extra_kb=round((dsize(d)-dsize(file.path(d,"lib"))-file.size(file.path(d,"index.html")))/1024))
  }
}
out <- do.call(rbind,res); out <- out[!(out$tipo=="poligonos" & out$n<=1),]
print(out); write.csv(out, "R_scripts/tamanos.csv", row.names=FALSE)
cat("mapview fgb option:", mapviewGetOption("fgb"), "\n")
