require(maps)
require(plotly)
require(magrittr)

# Create data frame
# Bring in data frame from CMAS Messages
if (!exists("alert_tally")) source("CMAS_Clean.R")
 
# Join alert_tally to  to return the matching county name
if (!exists("fips_lookup")) fips_lookup <- load_fips() %>%
   select(1,4, fips = Num)

t <-  data_frame(StateAbbr = state.abb, name = state.name)
 
alert_df <- ungroup(alert_tally) %>% 
  set_colnames(value = tolower(colnames(.))) %>% 
  select(fips = geoid, 2:6) %>%
  mutate(fips = as.character(fips)) %>%
  left_join(fips_lookup) %>%
  mutate(total = amber + flashflood + other + tornado + tsunami) %>%
  group_by(StateAbbr) %>%
  # select(StateAbbr, amber, flashflood, other, tornado, tsunami, total) %>%
  summarize_all(sum) %>%
  left_join(t) 
   
 
   
   
   

# ##########
# # Create hover text
# 
#  alert_df$hover <- with(alert_df
#                         , paste(StateAbbr, '<br>', "Total:", total))
# # # Make state borders white
# l <- list(color = toRGB("white"), width = 2)
# 
# # # Set up some mapping options
# g <- list(scope = 'county'
#           , projection = list(type = 'albers usa')
#           , showlakes = TRUE
#           , lakecolor = toRGB("white")
# )
# 
# p <- plot_geo(alert_df
#               , locationmode = 'USA-states'
#               , mode = "markers")
# #   add_trace(
# #     z = ~total, text = ~hover, locations = ~code
#   
# )