# clean population data of Texas cities 
# data obtained from https://www.census.gov/programs-surveys/popest/technical-documentation/research/evaluation-estimates/2020-evaluation-estimates/2010s-cities-and-towns-total.html
# Last updated: 30-04-2024


##---------------------------------------------------------------
##                  Population Data from Census                 -
##---------------------------------------------------------------

# read data
data = read.csv("data/raw/population/cities/SUB-EST2020_48.csv")

# make panel 

popdata = data %>%
  pivot_longer(cols = starts_with("POPESTIMATE"),
               names_to = "year",
               names_prefix = "POPESTIMATE",
               values_to = "population") %>% 
  mutate(year = as.integer(year)) %>% 
  rename(
    city = NAME,
    countyfips = COUNTY) %>%
  filter(city != "Texas") %>% 
  filter(year <= 2019) %>% 
  select(city, countyfips, year, population)

write.csv(popdata, "data/clean/population/texas_population_by_cities_2010_2019.csv", row.names = F)  


##---------------------------------------------------------------
##    Population data from Texas Department of Transportation   -
##---------------------------------------------------------------

# dataset of all cities with county names and population included, poulation data is patchy
citydata = read_sf("data/raw/geography/Texas_Cities_-8753904644452599183/City.shp") %>% 
  select(CITY_NM, CNTY_NM) %>% 
  #pivot_longer(., starts_with("POP"), names_prefix = "POP", names_to = "year", values_to = "population") %>% 
  st_drop_geometry()
# Three pairs of municipalities share the same name: Lakeside, Oak Ridge, and Reno
# will modify city name to reflect this

duplicate_cities = c("Lakeside", "Oak Ridge", "Reno")

citydata = citydata %>%
  mutate(
    CITY_NM = ifelse(CITY_NM %in% duplicate_cities, str_c(CITY_NM, " (", CNTY_NM, ")"), CITY_NM)
  )

# dataset with smaller cities of cities, complete popualtion data
mapdata = read_sf("data/raw/geography/TxDOT_City_Boundaries_-7440712964994907480/Cities.shp") %>% 
  select(CITY_NM, POP1990, POP2000, POP2010, POP2020) %>%
  pivot_longer(., starts_with("POP"), names_prefix = "POP", names_to = "year", values_to = "population") %>% 
  st_drop_geometry()

texas_cities_population  = left_join(mapdata, citydata, by = "CITY_NM") %>% 
  rename(
    city = CITY_NM,
    county = CNTY_NM
  )

# County names of some cities are missing
# I will fill in the county names with data from Wikipedia

url = "https://en.wikipedia.org/wiki/List_of_municipalities_in_Texas"

tables = read_html(url) %>% 
  html_nodes("table")%>% 
  html_table(header = TRUE)

countyfips = read.csv("data/clean/fips codes/texas_county_fips.csv")

wikipedia_cities = bind_rows(tables[[1]])

wikipedia_cities = wikipedia_cities[-c(1, (nrow(wikipedia_cities)-1):nrow(wikipedia_cities)), ] %>% 
  select(Municipality, `Primary county`) %>% 
  rename(
    city = Municipality,
    final_county = `Primary county`
  ) %>% 
  mutate(
    final_county = ifelse(final_county == "DeWitt", "De Witt", final_county)
  )

# check which counties are missing
temp = texas_cities_population %>% filter(is.na(county)) %>%  group_by(city) %>% count() %>% select(city)
temp1 = left_join(temp, wikipedia_cities, by = "city")



# rejoin counties to main data set

texas_cities_population = texas_cities_population %>% 
  left_join(temp1, by = "city") %>% 
  mutate(county = ifelse(is.na(county), final_county, county)) %>% 
  select(-final_county)

write.csv(texas_cities_population, "data/clean/population/texas_population_by_cities_decennial.csv", row.names = F)  
