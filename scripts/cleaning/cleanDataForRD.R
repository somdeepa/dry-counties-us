
#################################################################
##               CLEAN DATA FOR R-D STYLE GRAPHS               ##
#################################################################

# I will select elections according to the following criteria -
# - ALL ELECTIONS
# - cities only for now
# - Note- FOR NOW I AM CONSIDERING "DUPLLICATE" ELECTIONS
# (Which happen in the same geography same year)

# Load data

crosswalk_cityname_election = read.csv("data/clean/city-name-xwalk/city_name_crosswalk_electiondata.csv")

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
  ) %>% 
  left_join(crosswalk_cityname_election, join_by(city == electiondata))

crosswalk_cityname_popdata = read.csv("data/clean/city-name-xwalk/city_name_crosswalk_popdata.csv")

populationdata = read.csv("data/clean/population/texas_population_by_cities_decennial.csv") %>% 
  left_join(crosswalk_cityname_popdata, join_by(city == popdata))

crosswalk_cityname_licensesdata = read.csv("data/clean/city-name-xwalk/city_name_crosswalk_licensesdata.csv")

all_combinations = expand.grid(
  city = unique(electiondata$city),
  quarter = quarters
) %>% 
  left_join(crosswalk_cityname_election, join_by(city == electiondata))

##----------------------------------------------------------------
##                  Merge for City-Quarter-All                   -
##----------------------------------------------------------------


city_licenses_panel = read.csv("data/clean/liquor-licenses/quarterly_city_panel_all_licenses.csv") %>% 
  left_join(crosswalk_cityname_licensesdata, join_by(city == licensesdata))

elections = electiondata %>% 
  mutate(
    election_quarter = sapply(election_date, convert_to_quarter)
)

final_panel = elections %>% 
  # join license data
  left_join(city_licenses_panel, by = "keyformerge", relationship = "many-to-many") %>% 
  # change NAs to 0 (Missing data here => true zeros)
  mutate(total_businesses = ifelse(is.na(total_businesses), 0, total_businesses)) %>% 
  # add population 
  left_join(populationdata, join_by(keyformerge, adjusted_year == year)) %>% 
  mutate(
    # calculate licenses per 1000 population
    licensepop = total_businesses/population*1000,
    # create event study dummy
    periods_from_election = quarter - election_quarter
  )

write.csv(final_panel, "data/clean/merged/licenses-elections/RDpanel_quaterly_city.csv", row.names = F)
