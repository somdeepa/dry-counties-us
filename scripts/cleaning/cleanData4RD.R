
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

elections = read.csv("data/clean/elections-data/elections-data-97-2020-cleaned.csv") %>% 
  filter(!is.na(city)) %>% 
  left_join(crosswalk_cityname_election, join_by(city == electiondata))

crosswalk_cityname_popdata = read.csv("data/clean/city-name-xwalk/city_name_crosswalk_popdata.csv")

populationdata = read.csv("data/clean/population/texas_population_by_cities_decennial.csv") %>% 
  left_join(crosswalk_cityname_popdata, join_by(city == popdata))

crosswalk_cityname_licensesdata = read.csv("data/clean/city-name-xwalk/city_name_crosswalk_licensesdata.csv")

all_combinations = expand.grid(
  city = unique(elections$city),
  quarter = quarters
) %>% 
  left_join(crosswalk_cityname_election, join_by(city == electiondata))

##----------------------------------------------------------------
##                  Merge for City-Quarter-All                   -
##----------------------------------------------------------------


city_licenses_panel = read.csv("data/clean/liquor-licenses/quarterly_city_panel_all_licenses.csv") %>% 
  left_join(crosswalk_cityname_licensesdata, join_by(city == licensesdata))

final_panel = all_combinations %>% 
  # join license data
  left_join(city_licenses_panel, by = c("keyformerge", "quarter")) %>% 
  # change NAs to 0 (Missing data here => true zeros)
  mutate(total_businesses = ifelse(is.na(total_businesses), 0, total_businesses)) %>% 
  # add population figure
  left_join(populationdata, join_by(keyformerge, adjusted_year == year)) %>% 
  mutate(
    # calculate licenses per 1000 population
    licensepop = total_businesses/population*1000,
    #periods_from_treatment = quarter - first_policy_change_quarter
  ) %>% 
  group_by(city) %>% 
  #filter(n()<=116) %>% 
  ungroup()
