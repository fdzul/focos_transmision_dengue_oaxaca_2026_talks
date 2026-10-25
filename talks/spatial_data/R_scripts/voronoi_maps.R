# Step 1. load the dataset ####
load("~/Library/CloudStorage/OneDrive-Personal/proyects/geocoding_mex/2023/9.geocoded_dataset/dengue_mx_2023.RData")

# Step 2. extract the locality boundary ####
gua <- rgeomex::extract_locality(locality =  c("Guadalajara", "Tlaquepaque", 
                                               "Zapopan", "Tonalá"),
                                 cve_edo = "14") 

# Step 3. extract the dengue cases by locality ####

# Step 3.1 transform the dengue cases geocoded in sf object ####
z <- z |>
    dplyr::filter(ESTATUS_CASO %in% c(2)) |>
    sf::st_as_sf(coords = c("long", "lat"),
                 crs = 4326)

# Step 3.2 extract the dengue cases in the locality of guadalajara ####
z_cases <- z[gua, ] |>
    dplyr::mutate(lon = sf::st_coordinates(geometry)[,1],
                  lat = sf::st_coordinates(geometry)[,2])


bb_box <- sf::st_bbox(gua)

#This creates the voronoi line segments
voronoi <- deldir::deldir(x = z_cases$lon, 
                          y = z_cases$lat,
                          rw = c(bb_box[1],
                                 bb_box[3],
                                 bb_box[2],
                                 bb_box[4]))
# Now we can make a plot
ggplot2::ggplot() +
    ggplot2::geom_sf(data = gua,
                     linewidth = 1,
                     fill = NA)  +
    ggplot2::geom_segment(size = 2,
                          ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
                          data = voronoi$delsgs, #voronoi$dirsgs,
                          linetype = 1,
                          linewidth = .5,
                          color= "#FFB958") + 
    ggplot2::geom_point(fill= rgb(70,130,180,255,
                                  maxColorValue=255),
                        pch=  21,
                        size = 1.5,
                        color="#333333") +
    ggplot2::geom_sf(data = z_cases,
                     col = "blue")
