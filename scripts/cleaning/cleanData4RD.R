
#################################################################
##               CLEAN DATA FOR R-D STYLE GRAPHS               ##
#################################################################

I will select elections according to the following criteria -
- ALL ELECTIONS
- cities only for now
- Note- FOR NOW I AM CONSIDERING "DUPLLICATE" ELECTIONS
(Which happen in the same geography same year)

elections = read.csv("data/clean/elections-data/elections-data-97-2020-cleaned.csv") %>% 
  filter(!is.na(city))

crosswalk_citynames = read.csv("data/clean/merged/city_name_crosswalk.csv")

# Merge for CITY-QUARTER-ALL

city_licenses_panel = read.csv("data/clean/liquor-licenses/quarterly_city_panel_all_licenses.csv") %>% 
  left_join(crosswalk_citynames, join_by(city == licensesdata))


all_combinations = expand.grid(
  city = unique(elections$city),
  quarter = quarters
)

final_panel = all_combinations %>% 
  # join license data
  left_join(city_licenses_panel, join_by(city == electiondata, quarter)) %>% 
  # add policy changes
  left_join(policychanges, by = "city") %>% 
  # add population figure
  left_join(populationdata, join_by(popdata == city, adjusted_year == year)) %>% 
  mutate(
    # calculate licenses per 1000 population
    licensepop = total_businesses/population*1000,
    periods_from_treatment = quarter - first_policy_change_quarter
  ) %>% 
  group_by(city) %>% 
  filter(n()<=116) %>% 
  ungroup() %>% 
  filter(!is.infinite(licensepop))
