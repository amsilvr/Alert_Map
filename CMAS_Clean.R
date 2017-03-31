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
                mutate(threat_type = gsub("\\. .*","", cmac)) %>%
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

fips_replace <- function(f) { 
## looks up the state and county from area_find and returns the 5-digit fips code
       fframe <- data_frame(nm = f, nu = 0)

       for (i in seq_along(fframe$nm)) {
              fframe$nu[i] = 
                        filter(fips_lookup, grepl(fframe$nm[i]
                                          , fips_lookup$Name) == TRUE) %>%
                              select(Num) %>%
                      str_extract("[0-9]{5}")
                      } 
       return(fframe)
}

## Substitutes all counties in a state 
## For areas that include only state names

full_state <- function(areas_states) {
        if (!exists("fips_lookup")) fips_lookup <- load_fips()
        t <- data_frame(st = "")
        for (i in seq_along(areas_states$areas)) {
                t[i,1] <- filter(fips_lookup
                                 , StateAbbr == trimws(areas_states[i,2])) %>%
                        transmute(counties = 
                                          gsub(pattern = "([A-Z]{2}) (.*)",
                                               replace = "\\2 (\\1)", 
                                               x = Name)) %>% 
                        lapply(paste, collapse = ", ") 
        } 
        return(t)
}
#####################
## Run Functions  ###
#####################

if (!exists("msg")) msg <- load_msgs()

areas <- transmute(msg
                   , id = trimws(id)
                   , areas = trimws(areas))

areas_states <- filter(areas, str_length(areas) == 2)
fsa <- full_state(areas_states)
areas_states <- cbind(areas_states, fsa) %>%
        select(id, areas = st)
areas <- filter(areas, str_length(areas) > 2)



if (!exists("fips_lookup")) fips_lookup <- load_fips()

two <- tapply(areas$areas, area_find,INDEX = areas$id,simplify = TRUE)
three <- tapply(areas_states$areas, area_find,INDEX = areas_states$id,simplify = TRUE)

# return a lookup table for ID and fips codes

fips_msg <- lapply(c(two,three), fips_replace) %>%
        unlist(recursive = TRUE) %>%
        as_tibble(validate = TRUE) %>%
        rownames_to_column()    %>%
        separate(col = rowname, into = c("id","type")) %>%
        filter(grepl("nu",type) == TRUE) %>%
        select(-type) %>%
        rename(msg_id = id, GEOID = value)

## Cleanup

rm(list = c("areas", "fips_lookup", "two", "three"))

classify_message <- function(msg) {
        
        mutate(msg, type = 
                   case_when(
                        grepl("Tornado", msg$message_text, ignore.case = TRUE) ~ "Tornado",
                        grepl("Flash Flood", msg$message_text, ignore.case = TRUE) ~ "FlashFlood", 
                        grepl("Amber", msg$message_text, ignore.case = TRUE) ~ "AMBER",
                        grepl("Tsunami", msg$message_text, ignore.case = TRUE) ~ "Tsunami",
                        TRUE ~ "Other")
                        ) %>%
                transmute(msg_id = str_trim(id)
                          , rec_time
                          , expire_time
                          , response = response_type
                          , urgency
                          , wea = message_text 
                          , type = as.factor(type)
                          ) 
}

msg2 <- classify_message(msg) %>% unique()

alert_tally <- left_join(fips_msg, msg2) %>%
        transmute(msg_id, GEOID, type = as.factor(type)) %>%
        count(GEOID, type) %>%
        rename(WEATYPE = type, WEANUM = n) %>%
        spread(WEATYPE, WEANUM, fill = "0")

        