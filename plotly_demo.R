require(maps)
require(tidyverse)
require(plotly)

# Set Plotly info

Sys.setenv("plotly_username"="amsilvr")
Sys.setenv("plotly_api_key"="kDoqXYPQYD2PqBVKpNA0")


# Create data frame
# Bring in data frame from CMAS Messages
if (!exists("alert_states")) source("CMAS_States.R")
 
# Join alert_tally to  to return the matching county name
if (!exists("fips_lookup")) fips_lookup <- load_state_fips() 

t <-  select(fips_lookup, abb, name)
 
state_alert_df <- ungroup(alert_states) %>% 
  magrittr::set_colnames(value = tolower(colnames(.))) %>% 
  mutate(total = amber + flashflood + other + tornado + tsunami) %>%
  group_by(abb) %>%
  # select(StateAbbr, amber, flashflood, other, tornado, tsunami, total) %>%
  summarize_all(sum) %>%
  left_join(t) 
 

# ##########

# # Create hover text
 
state_alert_df$hover <- with(state_alert_df
                       , paste('<b>',name, '<br />', 'Total:', total,'</b>'
                          , if_else(amber > 0
                                    ,true = paste('<br />','AMBER:', amber)
                                    ,false = '')
                          , if_else(flashflood > 0
                                    ,true = paste('<br />','Flash Flood:', flashflood)
                                    ,false = '')
                          , if_else(tornado > 0
                                    ,true = paste('<br />','Tornado:', tornado)
                                    ,false = '')
                          , if_else(tsunami > 0
                                    ,true = paste('<br />','Tsunami:', tsunami)
                                    ,false = '')
                          , if_else(other > 0
                                    ,true = paste('<br />','Others:', other)
                                    ,false = '')
                          )
                       )

                          
# # Make state borders white
l <- list(color = toRGB("white"), width = 2)
 
 # Set up some mapping options
g <- list(scope = 'usa'
          , projection = list(type = 'albers usa')
          , showlakes = TRUE
          , lakecolor = toRGB("white")
)

p <- plot_geo(state_alert_df,locationmode = 'USA-states') %>%
  add_trace(
    z = ~total
    , text = ~hover
    , hoverinfo = "text"
    , locations = ~abb
    , color = ~total
    , colors = 'Greens'
  ) %>%
  
  colorbar(title = 'Total Alerts') %>%
  layout(title = 'WARN Messages: <br /> 
         May 2014 to March 2017'
         , geo = g)

# chart_link = plotly_POST(p, filename = "warn_states")
# chart_link