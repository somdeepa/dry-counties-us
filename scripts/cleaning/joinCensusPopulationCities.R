
##################################################################
##               MANEL CITY_YEAR POPULATION PANEL               ##
##################################################################

# NOTE
#   
# For 1990-1999 data, cities are separated into parts by which county the part belongs to;
# types level: city, town, village
# For county: missing data implies multiple counties for the city 

filenames = list.files("data/raw/population/city-population-census/", pattern = "\\.dta$", full.names = T)

# Three pairs of municipalities share the same name: Lakeside, Oak Ridge, and Reno
# will modify city name to reflect this
# Also resolve duplicate cities with different spellings

duplicate_cities = c("Lakeside", "Oak Ridge", "Reno")

countyfips = read.csv("data/clean/fips codes/texas_county_fips.csv")

populationdata = do.call(rbind, lapply(filenames, read_dta)) %>% 
  rename(countyfips = county,
         city = place_name) %>% 
  left_join(., countyfips, by = "countyfips") %>% 
  mutate(
    city = ifelse(city %in% duplicate_cities, str_c(city, " (", county, ")"), city)
  ) %>% 
  group_by(city, year) %>% 
  summarise(population = sum(population))
  
write.csv(populationdata, "data/clean/population/texas_population_by_cities_1990_2023.csv")
