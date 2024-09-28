
#################################################################
##       Make panel and time series of tax receipts data       ##
#################################################################

receipts = readRDS("data/clean/liquor-taxes/liquor-receipts.rds")

# Inflation adjust all receipts!

cpi = read.csv("data/raw/economy/price index/South-urban-cpi.csv") %>% 
  rename(year = Year, cpi = Value) %>% 
  mutate(month = month(ym(Label))) %>% 
  select(year, month, cpi)

target_cpi = cpi[cpi$year == 2010 & cpi$month == 1, "cpi"]

final_data = receipts %>% 
  # filtering out 1993 as only data for November and December
  filter(year!= 1993) %>% 
  left_join(., cpi, by = c("year", "month")) %>% 
  mutate(receipts_adjusted = receipts*(target_cpi/cpi))

##----------------------------------------------------------------
##                      QUATERLY TIME SERIES                     -
##----------------------------------------------------------------

receipts_quaterly = final_data %>% 
  group_by(quarter, type) %>% 
  summarise(
    year = unique(year),
    total_receipts = sum(receipts_adjusted)
  )

write.csv(receipts_quaterly, "data/clean/liquor-taxes/quarterly_receipts.csv", row.names = F)

##----------------------------------------------------------------
##                      CITY-QUARTER PANEL                       -
##----------------------------------------------------------------

populationdata = read.csv("data/clean/population/texas_population_by_cities_1990_2023.csv")

receipts_city_quarter = final_data %>% 
  group_by(city, quarter, type) %>% 
  summarise(
    total_receipts = sum(receipts_adjusted),
    year = unique(year)
  )

##------------------------
##  Merge with elections  
##------------------------

crosswalk_citynames = read.csv("data/clean/city-name-xwalk/city_name_crosswalk.csv")

electiondata = read.csv("data/clean/elections-data/elections-data-97-2020-cleaned.csv")


policychanges = electiondata %>% 
  filter(!is.na(city) & result == "passed" & status_before == "dry") %>% 
  mutate(
    # clean up dates
    election_date = as.Date(election_date, format = "%Y-%m-%d"),
    election_year = year(election_date),
  ) %>% 
  group_by(city) %>% 
  summarise(
    first_policy_change = min(election_year),
    first_policy_change_quarter = min(sapply(election_date, convert_to_quarter))
  )
policychanges = policychanges %>% 
  mutate(
    id = seq(1:nrow(policychanges))
  )

all_combinations = expand.grid(
  city = policychanges$city,
  type = unique(receipts_quaterly$type),
  quarter = quarters
) %>% 
  left_join(crosswalk_citynames, join_by(city == electiondata)) %>% 
  mutate(year = startYear + floor(quarter/4))

final_panel = all_combinations %>% 
  # join receipts data
  left_join(.,receipts_city_quarter, join_by(receiptsdata==city, quarter, year, type)) %>% 
  
  # change NAs to 0 (Missing data here => true zeros)
  mutate(total_receipts = ifelse(is.na(total_receipts), 0, total_receipts)) %>% 
  
  # add policy changes
  left_join(policychanges, by = "city") %>% 
  
  # add population 
  left_join(populationdata, join_by(popdata == city, year)) %>% 
  mutate(
    # calculate receipts per capita
    receipts_capita = total_receipts/population,
    # create treatment dummy
    treatment_quarter = ifelse(quarter >= first_policy_change_quarter, 1, 0),
    # create event study dummy
    periods_from_treatment = quarter - first_policy_change_quarter
  )

write.csv(final_panel, "data/clean/merged/quarterly_city_receipts.csv", row.names = F)


##---------------------------------------------------------------
##                        CITY-YEAR PANEL                       -
##---------------------------------------------------------------

receipts_city_annual = final_data %>% 
  group_by(city, year, type) %>% 
  summarise(
    total_receipts = sum(receipts_adjusted)
  )

write.csv(receipts_city_annual, "data/clean/liquor-taxes/annual_city_receipts.csv", row.names = F)

all_combinations = expand.grid(
  city = policychanges$city,
  type = unique(receipts_quaterly$type),
  year = seq(min(receipts_city_annual$year), endYear)
) %>% 
  left_join(crosswalk_citynames, join_by(city == electiondata))

final_panel_annual = all_combinations %>% 
  # join receipts data
  left_join(.,receipts_city_annual, join_by(receiptsdata==city, year, type)) %>% 
  
  # change NAs to 0 (Missing data here => true zeros)
  mutate(total_receipts = ifelse(is.na(total_receipts), 0, total_receipts)) %>% 
  
  # add policy changes
  left_join(policychanges, by = "city") %>% 
  
  # add population 
  left_join(populationdata, join_by(popdata == city, year)) %>% 
  mutate(
    # calculate receipts per capita
    receipts_capita = total_receipts/population,
    # create treatment dummy
    treatment_year = ifelse(year >= first_policy_change, 1, 0),
    # create event study dummy
    periods_from_treatment = year - first_policy_change
  )

write.csv(final_panel_annual, "data/clean/merged/annual_city_receipts.csv", row.names = F)
