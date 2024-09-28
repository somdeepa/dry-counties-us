
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

crosswalk_citynames = read.csv("data/clean/city-name-xwalk/city_name_crosswalk.csv")

electiondata = read.csv("data/clean/elections-data/elections-data-97-2020-cleaned.csv")

populationdata = read.csv("data/clean/population/texas_population_by_cities_1990_2023.csv")

policychanges = electiondata %>% 
  filter(!is.na(city) & result == "passed" & status_before == "dry") %>% 
  mutate(
    # clean up dates
    election_date = as.Date(election_date, format = "%Y-%m-%d"),
    election_year = year(election_date),
  ) %>% 
  group_by(city) %>% 
  summarise(
    first_policy_change = min(election_year),
    first_policy_change_quarter = min(sapply(election_date, convert_to_quarter))
  )
policychanges = policychanges %>% 
  mutate(
    id = seq(1:nrow(policychanges))
  )

city_licenses_panel = read.csv("data/clean/liquor-licenses/quarterly_city_panel_off_on.csv") %>% 
  pivot_wider(names_from = category, values_from = total_businesses) %>% 
  mutate(Total = `Off-Premise` + `On-Premise`) %>% 
  pivot_longer(
    cols = `Off-Premise`:Total,
    names_to = "category", 
    values_to = "total_businesses") %>%
  left_join(., crosswalk_citynames, join_by(city == licensesdata)) %>% 
  
  # filter out treated cities
  filter(electiondata %in% policychanges$city) %>% 
  
  # join population
  left_join(., populationdata, join_by(popdata == city, year)) %>% 
  # aggregate up year
  group_by(category, year) %>% 
  summarise(
    total_businesses = sum(total_businesses, na.rm = T),
    population = sum(population, na.rm = T)
  ) %>% 
  mutate(licensepop = total_businesses/population*1000)

ggplot(city_licenses_panel, aes(x = year, y = licensepop, color = category, group = category)) +
  geom_line(size =0.8) +
  geom_point(aes(shape = category), size = 1.5) +
  labs(
    x = "",
    y = "",
    title = "Total # of Retail Alcohol Licenses in Treated Cities, 1990-2019"
  ) +
  scale_color_paletteer_d("ggthemr::greyscale") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

ggsave("results/weekly updates/25-9/licenses_over_time_per_capita_retail_only_treated_only.png", device = "png")
