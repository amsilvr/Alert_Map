# Download Shapefiles

countyshapes_url <- "http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_county_500k.zip"

if (!dir.exists("data")) {dir.create("data")}
if (!file.exists("data/county_shape_file.zip")) {
        download.file(countyshapes_url
                      , destfile = "data/county_shape_file.zip")
        t <- unzip("data/county_shape_file.zip", exdir = "data")
}
# Read the file with rgdal
 
require(rgdal)
require(leaflet)

county_spdf =readOGR(dsn = "data/cb_2015_us_county_20m.shp")

# Add the alert tally to the county data
if (!exists("fips_lookup")) fips_lookup <- load_fips()
# Join the tally by GEOID
        county_spdf@data <- 
                left_join(county_spdf@data, unique(select(fips_lookup, StateAbbr, STATEFP = StateNum))) %>%       
                left_join(alert_tally) %>%
                mutate(AMBER + FlashFlood + Other + Tornado + Tsunami)
        county_spdf@data[is.na(county_spdf@data)] <- 0
        
# Create Popup Labels
bins <- c(0, 1, 5, 10, 15, 20, 25, 30, Inf)
pal <- colorBin("YlGnBu", domain = county_spdf@data$total, bins = bins)
labels <- sprintf(
        "<strong>%s, %s: <br/ >
        %g Alerts</strong>
        <br/>%g AMBER<br/>
        %g Flash Flood<br/>
        %g Tornado<br/>
        %g Tsunami<br/>
        %g Other"
        , county_spdf@data$NAME
        , county_spdf@data$StateAbbr
        , county_spdf@data$total
        , county_spdf@data$AMBER
        , county_spdf@data$FlashFlood
        , county_spdf@data$Tornado
        , county_spdf@data$Tsunami
        , county_spdf@data$Other
) %>% lapply(htmltools::HTML)
m = leaflet(county_spdf) %>%
        addTiles()  %>% 
        setView(-96, 37.8, 4) %>%
        addPolygons(stroke = FALSE
                   , fillOpacity = 0.5
                   , smoothFactor = 0.5
                   # , color = ~colorQuantile("YlOrRd", total)(total)
                   , fillColor = ~pal(total)
                   , highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE)
                    , label = labels
                    , labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto")
                    ) %>%
        addLegend(pal = pal
                  , values = ~total, opacity = 0.7
                  , title = "Total WARN Messages Sent",
                      position = "bottomright")
