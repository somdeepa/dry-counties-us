
#################################################################
##               CLEAN DATA FOR R-D STYLE GRAPHS               ##
#################################################################

# I will select elections according to the following criteria -
# - ALL ELECTIONS
# - cities only for now
# - Note- FOR NOW I AM CONSIDERING "DUPLLICATE" ELECTIONS
# (Which happen in the same geography same year)

# Load data

crosswalk_citynames = read.csv("data/clean/city-name-xwalk/city_name_crosswalk.csv")

electiondata = read.csv("data/clean/elections-data/elections-data-97-2020-cleaned.csv") %>% 
  filter(!is.na(city) & status_before == "dry") %>% 
  mutate(
    # add an ID for each election
    id = seq(1, nrow(.)),
    
    # clean up dates
    election_date = as.Date(election_date, format = "%Y-%m-%d"),
    election_year = year(election_date),
    
    # calculate vote share
    for_vote_share = for_vote/(for_vote+against_vote)
  )

populationdata = read.csv("data/clean/population/texas_population_by_cities_1990_2023.csv")

##----------------------------------------------------------------
##                  Merge for City-Quarter-All                   -
##----------------------------------------------------------------

elections = electiondata %>% 
  select(city, election_date, issue, result, for_vote, against_vote,
         status_before, status_current, election_year, for_vote_share) %>% 
  mutate(
    election_quarter = sapply(election_date, convert_to_quarter)
  )

##------------
##  Licenses  
##------------

city_licenses_panel = read.csv("data/clean/liquor-licenses/quarterly_city_panel_all_licenses.csv") %>% 
  left_join(., crosswalk_citynames, join_by(city == licensesdata))

final_panel = elections %>% 
  # join license data
  left_join(city_licenses_panel, join_by(city == electiondata), relationship = "many-to-many") %>% 
  # change NAs to 0 (Missing data here => true zeros)
  mutate(total_businesses = ifelse(is.na(total_businesses), 0, total_businesses)) %>% 
  # add population 
  left_join(populationdata, join_by(popdata == city, year)) %>% 
  mutate(
    # calculate licenses per 1000 population
    licensepop = total_businesses/population*1000,
    # create event study dummy
    periods_from_election = quarter - election_quarter
  )

write.csv(final_panel, "data/clean/merged/licenses-elections/RDpanel_quaterly_city.csv", row.names = F)

# on-premise

city_licenses_panel_on = read.csv("data/clean/liquor-licenses/quarterly_city_panel_off_on.csv") %>% 
  filter(category == "On-Premise") %>% 
  left_join(., crosswalk_citynames, join_by(city == licensesdata))

final_panel = elections %>% 
  # join license data
  left_join(city_licenses_panel_on, join_by(city == electiondata), relationship = "many-to-many") %>% 
  # change NAs to 0 (Missing data here => true zeros)
  mutate(total_businesses = ifelse(is.na(total_businesses), 0, total_businesses)) %>% 
  # add population 
  left_join(populationdata, join_by(popdata == city, year)) %>% 
  mutate(
    # calculate licenses per 1000 population
    licensepop = total_businesses/population*1000,
    # create event study dummy
    periods_from_election = quarter - election_quarter
  )

write.csv(final_panel, "data/clean/merged/licenses-elections/RDpanel_quaterly_city_on_premise.csv", row.names = F)

# off-premise

city_licenses_panel_off = read.csv("data/clean/liquor-licenses/quarterly_city_panel_off_on.csv") %>% 
  filter(category == "Off-Premise") %>% 
  left_join(., crosswalk_citynames, join_by(city == licensesdata))

final_panel = elections %>% 
  # join license data
  left_join(city_licenses_panel_off, join_by(city == electiondata), relationship = "many-to-many") %>% 
  # change NAs to 0 (Missing data here => true zeros)
  mutate(total_businesses = ifelse(is.na(total_businesses), 0, total_businesses)) %>% 
  # add population 
  left_join(populationdata, join_by(popdata == city, year)) %>% 
  mutate(
    # calculate licenses per 1000 population
    licensepop = total_businesses/population*1000,
    # create event study dummy
    periods_from_election = quarter - election_quarter
  )

write.csv(final_panel, "data/clean/merged/licenses-elections/RDpanel_quaterly_city_off_premise.csv", row.names = F)


##------------
##  Receipts  
##------------

receipts_city_annual = read.csv("data/clean/liquor-taxes/annual_city_receipts.csv") %>% 
  left_join(., crosswalk_citynames, join_by(city == receiptsdata))

final_panel = elections %>% 
  # join license data
  left_join(receipts_city_annual, join_by(city == electiondata), relationship = "many-to-many") %>% 
  # change NAs to 0 (Missing data here => true zeros)
  mutate(total_receipts = ifelse(is.na(total_receipts), 0, total_receipts)) %>% 
  # add population 
  left_join(populationdata, join_by(popdata == city, year)) %>% 
  mutate(
    # calculate receipts per capita
    receipts_capita = total_receipts/population,
    # create event study dummy
    periods_from_election = year - election_year
  )

write.csv(final_panel, "data/clean/merged/RDpanel_receipts_elections_city.csv", row.names = F)
