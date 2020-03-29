date_to_boundary_year <- function(x){
  #' Returns the set of electoral boundaries that were in place given a year
  #' @param x the date to check (yyyy-mm-dd format)
  #' @export
  the_date <- lubridate::date(x)
  the_year <- lubridate::year(the_date)
  y<-dplyr::case_when(
    the_year>1830 & the_year<1862 ~ "1832",
    the_year>1861 & the_year<1868 ~ "1862",
    the_year>1867 & the_year<1870 ~ "1868",
    the_year>1869 & the_year<1885 ~ "1870",
    the_year>1884 & the_year<1914 ~ "1885",
    TRUE ~ NA_character_

  )
  y
}

find_const <- function(lat, lon, boundary_label, constituencies_shapes_list = durhamevp::const_shape_list, col_to_return="G_NAME") {
  #' Find constituency from latitude and longitude.
  #'
  #' Finds the constituency (Vision of Britian G_NAME) of a latitude and longitude location.
  #' @param lat the latitude to check.
  #' @param lon the longitude to check.
  #' @param constituencies_shapes_list The constituencies. A named list of Simple Features dataframes. The names identify which Simple Features Dataframe to use.
  #' @param boundary_label The name in the list of the Simple Feature dataframe to use.In the default list the options are: 1832, 1862, 1868, 1870, 1885 (also the values returned by the \code{date_to_boundary_year} function).
  #' @export
  #'

  # if(is.na(boundary_label)|is.na(lat)|is.na(lon)) return(NA)
  if (!((boundary_label) %in% names(constituencies_shapes_list))) {
    warning("boundary_label not in constituency_shapes_list")
    return (NA)
  }

  these_constituencies <- constituencies_shapes_list[[boundary_label]]

  make_st <- function(x){
    y <- sf::st_as_sf(x, crs = 4326, coords = c("lon", "lat"))
    y
  }

  the_place <- make_st(data.frame(lon=lon, lat=lat))

  the_place <- sf::st_transform(the_place, crs=sf::st_crs(these_constituencies))

  res <- sf::st_join(the_place, these_constituencies)


  # if(nrow(res)>1) {
  #   warning("returned multiple places: only returning first result")
  # }

  as.character(dplyr::pull(res, col_to_return))
}

find_county <- function(lat, lon, county_shapes = durhamevp::county_shapes, col_to_return="G_NAME"){
  #' Finds county of a latitude and longitude.
  #'
  #' Finds the county (Vision of Britian G_NAME) of a location given by latitude and longitude.
  #'
  #'
  #' @param lat the latitude to check.
  #' @param lon the longitude to check.
  #' @param county_shapes A Simple Feature dataframe of the counties
  #' @export


  these_counties <- county_shapes

  make_st <- function(x){
    y <- sf::st_as_sf(x, crs = 4326, coords = c("lon", "lat"))
    y
  }

  the_place <- make_st(data.frame(lon=lon, lat=lat))

  the_place <- sf::st_transform(the_place, crs=sf::st_crs(these_counties))

  res <- sf::st_join(the_place, these_counties)


  as.character(dplyr::pull(res, col_to_return))
}


map_points_on_background <- function(z, the_shapes, title="my map", labels=TRUE){
  #' plots sf points against background of a polygons (the_shapes)
  #' @param z the points (sf format).
  #' @param the_shapes the background polygons.
  #' @param title the title of the map.
  #' @param labels should the points be labelled with their G_NAME.
  #' @export

  bb<-sf::st_as_sfc(st_bbox(z))

  bb<-sf::st_transform(bb, crs=st_crs(the_shapes))
  z <- sf::st_transform(z, crs=st_crs(the_shapes))

  on_the_map<-sf::st_intersects(county_shapes, bb, sparse = FALSE)
  the_map <- the_shapes[on_the_map, ]
  res <- ggplot2::ggplot(data=the_map) +
    ggplot2::geom_sf(data=the_map)
  if(county_labels){
    res <- res +
      ggplot2::geom_sf_label(size=2, aes(label=G_NAME))
  }

  res+
    ggplot2::geom_sf(data=z, size=2, colour="red") +
    ggplot2::ggtitle(title)
}

