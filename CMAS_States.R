## CMAS Clean

require(googlesheets)
require(tidyverse)
require(lubridate)
require(stringr)
require(albersusa)

ss_new <- gs_key("1Xw4JefUCS4HHQ0KpvKhr-DjklqzhH3_CeA-zhoAuQfI", visibility = "private") #CMAS_Alerts_Processed

load_msgs <- function() {
        msg <- gs_read(ss = ss_new
                       , col_names = c("rec_time", "cmac", "full_text")
                       , coltypes = "Tcc", skip = 1, trim_ws = TRUE) %>%
                mutate(rec_time = mdy_hms(gsub(" at ", " ", rec_time)
                                          , tz = "America/New_York"
                                          , truncated = 3) 
                ) %>%
                separate(full_text,
                         c("blank", "gateway_id" ,"id"
                           ,"special_handling", "message_type" 
                           , "category", "response_type", "severity" 
                           , "urgency", "certainty", "expire_time"
                           , "text_language", "alert_message","dummy")
                         , sep = "CMAC_[:word:]*: "
                         , fill = "right" ## drops the warning for rows with too many records
                         , remove = TRUE
                ) 
        
        
        ## creates a table for fields with "update" records
        
        updates <- filter(msg, nchar(special_handling) < 10) %>%
                select(rec_time, cmac, gateway_id
                       , id
                       , ref_id = special_handling
                       , special_handling = message_type
                       , message_type = category
                       , category = response_type
                       , response_type = severity
                       , severity = urgency 
                       , urgency = certainty 
                       , certainty = expire_time 
                       , text_language = alert_message 
                       , alert_message = dummy
                )
        
        msg <- filter(msg, nchar(special_handling) >= 10) %>%
                select(-blank, -dummy)
        
        ## puts all the records back into a single table and 
        ## uses two different separators to split out the alert 
        ## text from the plain English "area" field
        ## and finally removes the tcs boilerplate
        
        msg <- bind_rows(msg, updates) %>%
                mutate(expire_time = ymd_hms(expire_time)) %>%
                separate(alert_message, c("message_text","t2")
                         , sep = "Targeted Areas: "
                         , remove = TRUE) %>%
                separate(t2, c("areas"), sep = "[:punct:]{4}"
                         , extra = "drop", remove = TRUE) %>%
                mutate(threat_type = gsub("\\. .*","", cmac)
                       ,msg_id = str_trim(id)) %>%
                dplyr::filter(!(gateway_id == "http://tcs.tsis.com\n") ) 
        
        msg <- msg[-grep(" test", msg$threat_type),] 
        
       return(msg)
       
        } 

classify_message <- function(msg) {
  
  mutate(msg, type = 
           case_when(
             grepl("Tornado", msg$message_text, ignore.case = TRUE) ~ "Tornado",
             grepl("Flash Flood", msg$message_text, ignore.case = TRUE) ~ "FlashFlood", 
             grepl("Amber", msg$message_text, ignore.case = TRUE) ~ "AMBER",
             grepl("Tsunami", msg$message_text, ignore.case = TRUE) ~ "Tsunami",
             TRUE ~ "Other")
  ) %>%
    transmute(msg_id
              , rec_time
              , expire_time
              , response = response_type
              , urgency
              , wea = message_text 
              , type = as.factor(type)
    ) 
}

## Download local copy of FIPS lookup data and read into memory
load_state_fips <- function() {

    fips_lookup <- albersusa::usa_sf() %>%
      transmute(fips_state, name
                ,abb = iso_3166_2)
      
}

## Create functions for replacement loops ##
state_find <- function(area_list) {
  area_list <- str_replace_all(area_list
                               , pattern = "(([A-z]*) \\(([A-Z]{2})\\)), \\1"
                               , replacement = "\\2 city \\(\\3\\), \\2 \\(\\3\\)"
  )
  
   n <- str_match_all(string = area_list
                     , pattern = "\\(?([A-Z]{2})\\)?")
  
  area_clean <- paste(str_trim(n[[1]][,2], side = "both")) %>%
    unique() %>%
    return()
}



#####################
## Run Functions  ###
#####################

if (!exists("msg")) msg <- load_msgs()

areas_states <- transmute(msg, msg_id, areas = str_trim(areas))

state_msg <- tapply(areas_states$areas, state_find,INDEX = areas_states$msg_id,simplify = TRUE) %>%
  unlist(recursive = TRUE) %>%
  as_tibble(validate = TRUE) %>%
  rownames_to_column()    %>%
  rename(msg_id = rowname, abb = value)
## Cleanup

# rm(list = c("areas", "fips_lookup", "two", "three"))



msg2 <- classify_message(msg)

alert_states <- inner_join(msg2, state_msg) %>%
        select(msg_id, abb, type) %>%
        count(abb, type) %>%
        #rename(WEATYPE = type, WEANUM = n) %>%
        spread(type, n, fill = "0", convert = TRUE)

        