
##----------------------------------------------------------------
##            LICENSE TRENDS FOR TREATED COUNTIES ONLY           -
##----------------------------------------------------------------

## BY CATEGORY

total_businesses_per_year = read.csv("data/clean/liquor-licenses/total_businesses_by_year_treated_only_1990_2019.csv")
# Aggregate total number of businesses per year for each category across all counties
businesses_per_category = read.csv("data/clean/liquor-licenses/total_businesses_by_year_treated_only_category_detail_1990_2019.csv")
# Add a column to distinguish the total businesses line from category lines
total_businesses_per_year$category_detail = "Total"
businesses_per_category$category_detail = as.character(businesses_per_category$category_detail)


# Combine the data frames
combined_data = bind_rows(total_businesses_per_year, businesses_per_category)
ggplot(combined_data, aes(x = year, y = total_businesses, color = category_detail, group = category_detail)) +
  geom_line(size =0.8) +
  geom_point(aes(shape = category_detail), size = 1.5) +
  labs(
    x = "",
    y = "",
    title = "Total # of Alcohol Licenses in Treated Cities, 1990-2019"
  ) +
  scale_color_paletteer_d("ggthemr::greyscale") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

ggsave("results/weekly updates/4-9/licenses_over_time_treated_only.png", device = "png")

## BY OFF/ON

total_businesses_per_year = read.csv("data/clean/liquor-licenses/total_businesses_by_year_treated_only_1990_2019.csv")
# Aggregate total number of businesses per year for each category across all counties
businesses_per_category = read.csv("data/clean/liquor-licenses/total_businesses_by_year_category_treated_only_1990_2019.csv")
# Add a column to distinguish the total businesses line from category lines
total_businesses_per_year$category = "Total"
businesses_per_category$category = as.character(businesses_per_category$category)


# Combine the data frames
combined_data = bind_rows(total_businesses_per_year, businesses_per_category)
ggplot(combined_data, aes(x = year, y = total_businesses, color = category, group = category)) +
  geom_line(size =0.8) +
  geom_point(aes(shape = category), size = 1.5) +
  labs(
    x = "",
    y = "",
    title = "Total # of Retail Alcohol Licenses in Treated Cities, 1990-2019"
  ) +
  theme_bw() +
  scale_color_paletteer_d("ggthemr::greyscale") +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )
ggsave("results/weekly updates/4-9/licenses_over_time_retail_only_treated_only.png", device = "png")


##----------------------
##  PER CAPITA VERSION  
##----------------------

crosswalk_citynames_elections = read.csv("data/clean/city-name-xwalk/city_name_crosswalk_electiondata.csv")

electiondata = read.csv("data/clean/elections-data/elections-data-97-2020-cleaned.csv") %>% 
  left_join(crosswalk_citynames_elections, join_by(city==electiondata))

crosswalk_citynames_popdata = read.csv("data/clean/city-name-xwalk/city_name_crosswalk_popdata.csv")

populationdata = read.csv("data/clean/population/texas_population_by_cities_1990_2023.csv") %>% 
  left_join(crosswalk_citynames_popdata, join_by("city" == "popdata"))

policychanges = electiondata %>% 
  filter(!is.na(city) & result == "passed" & status_before == "dry") %>% 
  mutate(
    # clean up dates
    election_date = as.Date(election_date, format = "%Y-%m-%d"),
    election_year = year(election_date),
  ) %>% 
  group_by(city) %>% 
  summarise(
    keyformerge = unique(keyformerge),
    first_policy_change = min(election_year),
    first_policy_change_quarter = min(sapply(election_date, convert_to_quarter))
  )


crosswalk_citynames_licenses = read.csv("data/clean/city-name-xwalk/city_name_crosswalk_licensesdata.csv")

city_licenses_panel = read.csv("data/clean/liquor-licenses/quarterly_city_panel_off_on.csv")

data = city_licenses_panel %>% 
  pivot_wider(names_from = category, values_from = total_businesses) %>% 
  mutate(Total = `Off-Premise` + `On-Premise`) %>% 
  pivot_longer(
    cols = `Off-Premise`:Total,
    names_to = "category", 
    values_to = "total_businesses") %>%
  left_join(., crosswalk_citynames_licenses, join_by(city == licensesdata)) %>% 
  mutate(category = factor(category, levels = c("Off-Premise", "On-Premise", "Total")))
  
temp = data %>%   
# filter out treated cities
  filter(keyformerge %in% policychanges$keyformerge) %>% 
  
  # join population
  left_join(populationdata, join_by(keyformerge, year)) %>% 
  # aggregate up year
  group_by(category, year) %>% 
  summarise(
    total_businesses = sum(total_businesses, na.rm = T),
    population = sum(population, na.rm = T)
  ) %>% 
  mutate(licensepop = total_businesses/population*1000)

p1 = ggplot(temp, aes(x = year, y = licensepop, color = category, group = category)) +
  geom_line(size =0.8) +
  geom_point(aes(shape = category), size = 1.5) +
  labs(
    x = "",
    y = "",
    title = "Alcohol licenses per capita in cities with status change"
  ) +
  scale_color_paletteer_d("awtools::gpalette") +
  scale_y_continuous(limits = c(0,3)) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

ggsave("results/weekly updates/25-9/licenses_over_time_per_capita_retail_only_treated_only.png", device = "png")


##---------------------------------------------------------------
##                    ALWAYS TREATED CITIES                     -
##---------------------------------------------------------------


# cities that were dry during or any point after 1997

excluded_cities = electiondata %>% 
  filter(!is.na(city) & status_before == "dry") %>% 
  select(city, keyformerge) %>% 
  unique()

temp = data %>% 
  
  # exclude treated cities
  filter(!keyformerge %in% excluded_cities$keyformerge) %>%
  
  # join population
  left_join(populationdata, join_by(keyformerge, year)) %>% 
  # aggregate up year
  group_by(category, year) %>% 
  summarise(
    total_businesses = sum(total_businesses, na.rm = T),
    population = sum(population, na.rm = T)
  ) %>% 
  mutate(licensepop = total_businesses/population*1000)

p2 = ggplot(temp, aes(x = year, y = licensepop, color = category, group = category)) +
  geom_line(size =0.8) +
  geom_point(aes(shape = category), size = 1.5) +
  labs(
    x = "",
    y = "",
    title = "Alcohol licenses per capita in already wet cities"
  ) +
  scale_color_paletteer_d("awtools::gpalette") +
  scale_y_continuous(limits = c(0,3)) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

p2+p1

ggsave("results/weekly updates/2-10/licenses_over_time_per_capita_treated_vs_wet.png", width = 12, height = 5, device = "png")
