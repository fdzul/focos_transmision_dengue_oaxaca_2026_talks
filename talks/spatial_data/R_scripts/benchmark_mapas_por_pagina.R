
suppressMessages({library(sf); library(mapgl); library(leaflet)})
set.seed(1)
x <- st_as_sf(data.frame(id=1:200, lon=runif(200,-96.8,-96.65), lat=runif(200,17.02,17.12)), coords=c("lon","lat"), crs=4326)
for (pk in c("mapgl","leaflet")) {
  maps <- lapply(1:20, function(i) {
    if (pk=="mapgl") maplibre(bounds=x, width="300px", height="200px") |> add_circle_layer(id=paste0("l",i), source=x, circle_radius=3)
    else leaflet(x, width="300px", height="200px") |> addTiles() |> addCircleMarkers(radius=3)
  })
  page <- htmltools::tagList(lapply(seq_along(maps), function(i) htmltools::div(style="display:inline-block;margin:2px", htmltools::tags$b(i), maps[[i]])))
  htmltools::save_html(page, file=paste0("multi_",pk,".html"), libdir="lib_multi")
}
