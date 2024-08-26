# Clean population data by county from "https://seer.cancer.gov/popdata/popdic.html"

# Texas Population by county (1969-2022)

widths = c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8)

data = read.fwf("data/raw/population/tx.1969_2022.19ages.adjusted.txt", widths = widths)

names(data) = c("year", "state", "statefips", "countyfips", "registry", "race", 
                "origin", "sex", "age", "population")

write.csv(data, "data/raw/population/tx.1969_2022.csv")

population = read.csv("data/raw/population/tx.1969_2022.csv")

population = population %>% group_by(countyfips, year) %>% 
  summarise(population = sum(population))

write.csv(population, "data/clean/population/texas_population_by_county_1969_2022.csv", row.names = F)

# population only for ages 20-34

population_young = data %>% filter(age %in% c(5,6,7)) %>% 
  group_by(countyfips, year) %>% 
  summarise(population_young= sum(population)) %>% 
  filter(year > 1996)

write.csv(population_young, "data/clean/population/texas_population_by_county_1997_2022_ages_20-34.csv", row.names = F)
