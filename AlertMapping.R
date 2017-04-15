require(rgdal)
require(leaflet)
require(albersusa)

if (!exists("alert_tally")) source("CMAS_Clean.R")
county_spdf = counties_composite() #%>%
state_spdf = usa_composite()


# Add the alert tally to the county data
if (!exists("fips_lookup")) fips_lookup <- load_fips()
# Join the tally by fips number
county_spdf@data <- left_join(county_spdf@data, unique(select(fips_lookup
                                                              , abb
                                                              , fips)
                                                       )
                              ) %>%
  left_join(alert_tally) %>%
  mutate(total = AMBER + FlashFlood + Other + Tornado + Tsunami)

## Remove NA Values introduced by the join
county_spdf@data[is.na(county_spdf@data)] <- 0
        
# Create Popup Labels
bins <- c(0, 1, 5, 10, 15, 20, 25, 30, Inf)
pal <- colorBin("YlGnBu", domain = county_spdf@data$total, bins = bins)
labels <- sprintf(
        "<strong>%s %s, %s: <br/ >
        %g Alerts</strong>"
        , county_spdf@data$name
        , county_spdf@data$lsad
        , county_spdf@data$abb
        , county_spdf@data$total
) %>%
paste0( if_else(county_spdf@data$AMBER > 0
            , true = sprintf("<br/>%g AMBER", county_spdf@data$AMBER)
            , false =  ""
            , missing = "")
        ,if_else(county_spdf@data$FlashFlood > 0
            , true = sprintf("<br/>%g Flash Flood"
                             , county_spdf@data$FlashFlood)
            , false =  ""
            , missing = "")
        ,if_else(county_spdf@data$Tornado > 0
            , true = sprintf("<br/>%g Tornado"
                             , county_spdf@data$Tornado)
            , false = ""
            , missing = "")
        ,if_else(county_spdf@data$Tsunami > 0
            , true = sprintf("<br/>%g Tsunami"
                             , county_spdf@data$Tsunami)
            , false = ""
            , missing = "")
        , if_else(county_spdf@data$Other > 0
        , true = sprintf("<br/>%g Other<br/>", county_spdf@data$Other)
        , false = ""
        , missing = "")
    )  %>%  #end label
lapply(htmltools::HTML)
# Albers US projection cribbed from 
# https://rstudio.github.io/leaflet/projections.html

epsg2163 <- leafletCRS(
  crsClass = "L.Proj.CRS",
  code = "EPSG:2163",
  proj4def = "+proj=laea 
               +lat_0=45 +lon_0=-95 
               +x_0=0 +y_0=0 
               +a=6370997 
               +b=6370997
               +units=m +no_defs",
  resolutions = 2^(16:7))

m = leaflet(county_spdf, options = leafletOptions(crs = epsg2163)) %>%
  # addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
  #   attribution = paste(
  #   '&copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors',
  #   '&copy; <a href="http://cartodb.com/attributions">CartoDB</a>')
  #   )%>%
  addPolygons(stroke = FALSE
              , fillOpacity = .7
              , smoothFactor = 0.25
              , fillColor = ~pal(total)
              , highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 1,
                bringToFront = TRUE)
              , label = labels
              , labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")
  ) %>%
  addPolygons(data = state_spdf #statelines
              , stroke = TRUE
              , weight = 2
              , opacity = 1
              , color = "grey"
              , fill = FALSE) %>%
  addLegend(pal = pal
            , values = ~total, opacity = 1
            , title = "Total WARN <br />Sent",
            position = "bottomleft")
