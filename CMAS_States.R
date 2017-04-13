## CMAS Clean

require(googlesheets)
require(tidyverse)
require(lubridate)
require(stringr)

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
                select(rec_time, cmac, gateway_id, id
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


## Download local copy of FIPS lookup data and read into memory
load_fips <- function() {
        
census <- "http://www2.census.gov/geo/docs/reference/codes/files/national_county.txt"

fips_lookup <- read_csv(file = census
         , col_names = c("StateAbbr"
                         , "StateNum"
                         , "CountyNum"
                         , "CountyName")
         , col_types = "cccc_"
                 ) %>%
        mutate(Name = paste(StateAbbr, str_replace(CountyName," County",replacement = ""))
               , Num = paste0(StateNum, CountyNum))

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
area_find <- function(area_list) { ## isolates the state and county
        area_list <- str_replace_all(area_list
                     , pattern = "(([A-z]*) \\(([A-Z]{2})\\)), \\1"
                     , replacement = "\\2 city \\(\\3\\), \\2 \\(\\3\\)"
                     )

        m <- str_match_all(string = area_list
                             , pattern = "[A-z. ]{3,} ")
        
        n <- str_match_all(string = area_list
                           , pattern = "\\(?([A-Z]{2})\\)?")
        
        area_clean <- paste(n[[1]][,2]
                            , str_trim(m[[1]][,1], side = "both")) %>%
        return() %>%
  ## Clean TCS county names to match census list
  ##       
        str_replace_all(pattern = "E\\.",replacement = "East") %>%
        str_replace_all(pattern = "W\\.",replacement = "West") %>%
        str_replace_all(pattern = "(IN La|IL La) (Porte|Salle)",replacement = "\\1\\2") %>%
        str_replace_all(pattern = "FL Dade", "FL Miami-Dade") %>%
        str_replace_all(pattern = "PR lsabela", "PR Isabela") %>%
        str_replace_all(pattern = "TX wall", "TX Wall") %>%
        str_replace_all(pattern = "TX hell", "TX Hall") %>%
        str_replace_all(pattern = "MT Lewis Clark", "MT Lewis and Clark") 
        return(area_clean)

}
classify_message <- function(msg) {
  
  mutate(msg, type = 
           case_when(
               grepl("Tornado", msg$message_text, ignore.case = TRUE) ~ "Tornado"
             , grepl("Flash Flood", msg$message_text, ignore.case = TRUE) ~ "FlashFlood" 
             , grepl("Amber", msg$message_text, ignore.case = TRUE) ~ "AMBER"
             , grepl("Tsunami", msg$message_text, ignore.case = TRUE) ~ "Tsunami"
            ,TRUE ~ "Other")
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
## Substitutes all counties in a state 
## For areas that include only state names


#####################
## Run Functions  ###
#####################

if (!exists("msg")) msg <- load_msgs()

areas <- transmute(msg
                   , msg_id = str_trim(id)
                   , areas = str_trim(areas))

state_msg <- tapply(areas$areas, state_find,INDEX = areas$msg_id,simplify = TRUE) %>%
  unlist(recursive = TRUE) %>%
  as_tibble(validate = TRUE) %>%
  rownames_to_column()    %>%
  rename(msg_id = rowname, StateAbbr = value)
## Cleanup

# rm(list = c("areas", "fips_lookup", "two", "three"))



msg2 <- classify_message(msg)

alert_states <- inner_join(msg2, state_msg) %>%
        select(msg_id, abbr = StateAbbr, type) %>%
        count(abbr, type) %>%
        #rename(WEATYPE = type, WEANUM = n) %>%
        spread(type, n, fill = "0", convert = TRUE)

        