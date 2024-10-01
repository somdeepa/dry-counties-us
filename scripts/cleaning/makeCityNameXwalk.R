

###############################################################################
##                      Create crosswalk of city names                       ##
##  Because all these damn datasets have different spellings for city names  ##
##                              !!!!!!!!!!!!!!!                              ##
###############################################################################

# load data

elections = read.csv("data/clean/elections-data/elections-data-97-2020-cleaned.csv")

populationdata = read.csv("data/clean/population/texas_population_by_cities_1990_2023.csv")

liquor_licenses = read.csv("data/clean/liquor-licenses/all-liquor-licenses.csv")

liquor_receipts = readRDS("data/clean/liquor-taxes/liquor-receipts.rds")

crashes_data = read.csv("data/clean/outcomes/crashes/texas_crashes_by_cities_2003_2019.csv")

maps_data = read_sf("data/raw/geography/TxDOT_City_Boundaries_-7440712964994907480/Cities.shp")

# Cities from crashes file

crashes_cities = data.frame(crashesdata = unique(crashes_data$city))
crashes_cities$keyformerge = tolower(crashes_cities$crashesdata) %>% 
  str_remove_all("village of | city| village| town")
crashes_cities[crashes_cities$crashesdata=="clarksville city", 2] = "clarksville city"
crashes_cities[crashes_cities$keyformerge=="aurora (wise)",2] = "aurora" 
crashes_cities[crashes_cities$keyformerge=="de kalb",2] = "dekalb" 
crashes_cities[crashes_cities$keyformerge=="hubbard (hill)",2] = "hubbard" 
crashes_cities[crashes_cities$keyformerge=="pine forest (orange)",2] = "pine forest"
crashes_cities[crashes_cities$keyformerge=="rogers (bell)",2] = "rogers"
crashes_cities[crashes_cities$keyformerge=="st. paul (collin)",2] = "st. paul"
crashes_cities[crashes_cities$keyformerge=="dish", 2] = "clark"

# Cities from receipts file

receipts_cities = data.frame(receiptsdata = unique(liquor_receipts$city))
receipts_cities$keyformerge = tolower(receipts_cities$receiptsdata) %>% 
  str_remove_all("village of | city| village| town")
receipts_cities[receipts_cities$keyformerge == "wichtia falls",2] = "wichita falls"
receipts_cities[receipts_cities$keyformerge=="dish", 2] = "clark"

# Cities from licenses file

licenses_cities = data.frame(licensesdata = unique(liquor_licenses$city))
licenses_cities$keyformerge = tolower(licenses_cities$licensesdata) %>% 
  str_remove_all("village of | city| village| town")

licenses_cities[licenses_cities$keyformerge=="apples prings", 2] = "apple springs"
licenses_cities[licenses_cities$keyformerge=="st paul", 2] = "st. paul"
licenses_cities[licenses_cities$keyformerge=="odonnell", 2] = "o'donnell"
licenses_cities[licenses_cities$keyformerge=="morgans point", 2] = "morgan's point"
licenses_cities[licenses_cities$keyformerge=="dish", 2] = "clark"

# Cities from elections file

elections_cities = data.frame(electiondata = unique(elections$city)) %>% filter(!is.na(electiondata))
elections_cities$keyformerge = tolower(elections_cities$electiondata) %>% 
  str_remove_all("village of | city| village| town")

elections_cities[elections_cities$keyformerge=="south lake", 2] = "southlake"
elections_cities[elections_cities$keyformerge=="west lake", 2] = "westlake"

# make some adjustments to election city names to match census data
elections_cities[elections_cities$keyformerge=="cherino", 2] = "chireno"
elections_cities[elections_cities$keyformerge=="cransfill gap", 2] = "cranfills gap"
elections_cities[elections_cities$keyformerge=="nederwald", 2] = "niederwald"
elections_cities[elections_cities$keyformerge=="new fairfield", 2] = "new fairview"
elections_cities[elections_cities$keyformerge=="dish", 2] = "clark"

# Cities from census population file

popdata_cities = data.frame(popdata = unique(populationdata$city))
popdata_cities$keyformerge = tolower(popdata_cities$popdata) %>% 
  str_remove_all("village of | city| village| town")
popdata_cities[popdata_cities$popdata=="Clarksville City", 2] = "clarksville city"
popdata_cities[popdata_cities$keyformerge=="de kalb", 2] = "dekalb"
popdata_cities[popdata_cities$keyformerge=="st. jo", 2] = "saint jo"
popdata_cities[popdata_cities$keyformerge=="dish", 2] = "clark"

# Cities from maps file

mapsdata_cities = data.frame(mapsdata = unique(maps_data$CITY_NM))
mapsdata_cities$keyformerge = tolower(mapsdata_cities$mapsdata) %>% 
  str_remove_all("village of | city| village| town")
mapsdata_cities[mapsdata_cities$mapsdata=="Clarksville City", 2] = "clarksville city"
mapsdata_cities[mapsdata_cities$keyformerge=="saint paul", 2] = "st. paul"
mapsdata_cities[mapsdata_cities$keyformerge=="dish", 2] = "clark"


###  MERGE TO MAKE OVERALL CROSSWALK                             

crosswalk_citynames = full_join(elections_cities, licenses_cities, by = "keyformerge") %>% 
  full_join(., crashes_cities, by = "keyformerge") %>% 
  full_join(., receipts_cities, by = "keyformerge") %>% 
  full_join(., popdata_cities, by = "keyformerge") %>% 
  full_join(., mapsdata_cities, by = "keyformerge") %>% 
  filter(rowSums(!is.na(.)) > 2) %>% 
  arrange(keyformerge)

write.csv(crosswalk_citynames, "data/clean/city-name-xwalk/city_name_crosswalk.csv", row.names = F)

# Will create separate Xwalk file for each dataset
# resolves issue: when two cities have different spellings in the same dataset,
# I have mapped them to the same key. But when performing the final merges, I get
# duplicates for those cities in other columns. Thus is creating issues during merge.

# election data
crosswalk_cityname_election = crosswalk_citynames %>% 
  select(keyformerge, electiondata) %>% 
  distinct(electiondata, .keep_all = TRUE)

write.csv(crosswalk_cityname_election,"data/clean/city-name-xwalk/city_name_crosswalk_electiondata.csv", row.names=F)

# sales 

# license data
crosswalk_cityname_licensesdata = crosswalk_citynames %>% 
  select(keyformerge, licensesdata) %>% 
  distinct(licensesdata, .keep_all = TRUE)

write.csv(crosswalk_cityname_licensesdata,"data/clean/city-name-xwalk/city_name_crosswalk_licensesdata.csv", row.names=F)

# population data
crosswalk_cityname_popdata = crosswalk_citynames %>% 
  select(keyformerge, popdata) %>% 
  distinct(popdata, .keep_all = TRUE)

write.csv(crosswalk_cityname_popdata,"data/clean/city-name-xwalk/city_name_crosswalk_popdata.csv", row.names=F)

# maps data 
crosswalk_cityname_maps = crosswalk_citynames %>% 
  select(keyformerge, mapsdata) %>% 
  distinct(mapsdata, .keep_all = TRUE)

write.csv(crosswalk_cityname_maps,"data/clean/city-name-xwalk/city_name_crosswalk_mapsdata.csv", row.names=F)


