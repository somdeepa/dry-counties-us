
###############################################################################
##                      Create crosswalk of city names                       ##
##  Because all these damn datasets have different spellings for city names  ##
##                              !!!!!!!!!!!!!!!                              ##
###############################################################################

# load data

elections = read.csv("data/clean/elections-data/elections-data-97-2020-cleaned.csv")

populationdata = read.csv("data/clean/population/texas_population_by_cities_decennial.csv")

liquor_licenses = read.csv("data/clean/liquor-licenses/all-liquor-licenses.csv")

# Cities from licenses file

licenses_cities = data.frame(licensesdata = unique(liquor_licenses$city))
licenses_cities$keyformerge = tolower(licenses_cities$licensesdata)
licenses_cities$keyformerge = str_remove_all(licenses_cities$keyformerge, "village of ")
licenses_cities$keyformerge = str_remove_all(licenses_cities$keyformerge, " city")
licenses_cities$keyformerge = str_remove_all(licenses_cities$keyformerge, " village")
licenses_cities$keyformerge = str_remove_all(licenses_cities$keyformerge, " town")

licenses_cities[licenses_cities$keyformerge=="apples prings", 2] = "apple springs"
licenses_cities[licenses_cities$keyformerge=="st paul", 2] = "st. paul"
licenses_cities[licenses_cities$keyformerge=="odonnell", 2] = "o'donnell"
licenses_cities[licenses_cities$keyformerge=="morgans point", 2] = "morgan's point"

# Cities from elections file

elections_cities = data.frame(electiondata = unique(elections$city)) %>% filter(!is.na(electiondata))
elections_cities$keyformerge = tolower(elections_cities$electiondata)
elections_cities$keyformerge = str_remove_all(elections_cities$keyformerge, " city")
elections_cities$keyformerge = str_remove_all(elections_cities$keyformerge, "village of ")
elections_cities$keyformerge = str_remove_all(elections_cities$keyformerge, " village")
elections_cities[elections_cities$keyformerge=="south lake", 2] = "southlake"
elections_cities[elections_cities$keyformerge=="west lake", 2] = "westlake"

# make some adjustments to election city names to match census data
elections_cities[elections_cities$keyformerge=="cherino", 2] = "chireno"
elections_cities[elections_cities$keyformerge=="cransfill gap", 2] = "cranfills gap"
elections_cities[elections_cities$keyformerge=="nederwald", 2] = "niederwald"
elections_cities[elections_cities$keyformerge=="new fairfield", 2] = "new fairview"

# Cities from census population file

popdata_cities = data.frame(popdata = unique(populationdata$city)) %>% 
  filter(!grepl("(pt\\.|County)", popdata, ignore.case = TRUE))
popdata_cities$keyformerge = tolower(popdata_cities$popdata)
popdata_cities$keyformerge = str_remove_all(popdata_cities$keyformerge, " city")
popdata_cities$keyformerge = str_remove_all(popdata_cities$keyformerge, " village")
popdata_cities$keyformerge = str_remove_all(popdata_cities$keyformerge, " town")

crosswalk_citynames = full_join(elections_cities, licenses_cities, by = "keyformerge") %>% 
  full_join(., popdata_cities, by = "keyformerge") %>% 
  arrange(keyformerge)

write.csv(crosswalk_citynames,"data/clean/merged/city_name_crosswalk.csv", row.names=F)

# Create a summary table of NA counts for different combinations
summary_na = crosswalk_citynames %>%
  summarise(
    electiondata_NA = sum(is.na(electiondata)),
    licensesdata_NA = sum(is.na(licensesdata)),
    popdata_NA = sum(is.na(popdata)),
    nolicensesdata_NA = sum(!is.na(electiondata) & is.na(licensesdata)),
    nopopdata_NA = sum(!is.na(electiondata) & is.na(popdata)),
    licensesdata_and_popdata_NA = sum(is.na(licensesdata) & is.na(popdata))
  )
