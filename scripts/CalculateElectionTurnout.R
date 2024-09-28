
####################################################################
##  Merge election data to population data (cities and counties)  ##
##          Calculate imputed turnout for each election           ##
##           Calculate actual turnout for each election           ##
####################################################################


# load all data files
elections = read.csv("data/clean/elections-data/elections-data-97-2020-cleaned.csv")

censuspopulation = read.csv("data/clean/population/texas_population_by_cities_2010_2019.csv") %>% 
  distinct() %>% 
  filter(!city == "Lakeside town") %>%  # not well thought out, dis this to avoid issues when merging
  group_by(city, year) %>%
  summarise(population = sum(population, na.rm = TRUE)) %>%
  ungroup() %>% 
  arrange(city, year)


countypopulation = read.csv("data/clean/population/texas_population_by_county_1969_2022.csv")

countyfips = read.csv("data/clean/fips codes/texas_county_fips.csv")

countypopulation = left_join(countypopulation, countyfips, by = "countyfips")

votingbycounty = read.csv("data/clean/voting/voter-turnout-by-county-texas-1988-2022.csv")

votingall = read.csv("data/clean/voting/1970-2022-turnout-texas-all.csv")

crosswalk_citynames = read.csv("data/clean/city-name-xwalk/city_name_crosswalk.csv")


##------------------------------------------------------------------
##  Merge population and voter turnout data to elections dataset   -
##------------------------------------------------------------------


merged_data = elections %>% 
  mutate(
    election_year = year(election_date),
    adjusted_year = ifelse(election_year<=2010, 2010, election_year),
    adjusted_year2 = ifelse((election_year)%%2==0, election_year, election_year-1)) %>% 
  left_join(crosswalk_citynames, join_by(city == electiondata)) %>% 
  
  # merge city population
  left_join(censuspopulation, join_by(censusdata == city, adjusted_year == year)) %>% 
  rename(population_city = population) %>% 
  
  # merge county population
  left_join(countypopulation, join_by(county1 == county, adjusted_year == year)) %>% 
  rename(
    population_county_1 = population,
    countyfips_1 = countyfips
  ) %>% 
  left_join(countypopulation, join_by(county2 == county, adjusted_year == year)) %>% 
  rename(
    population_county_2 = population,
    countyfips_2 = countyfips
  ) %>% 
  left_join(countypopulation, join_by(county3 == county, adjusted_year == year)) %>% 
  rename(
    population_county_3 = population,
    countyfips_3 = countyfips
  ) %>% 
  
  # merge voting data
  left_join(votingbycounty, join_by(county1 == county, adjusted_year2 == year)) %>% 
  rename(
    registered_voters_county_1 = registered_voters,
    voted_county_1 = voted,
  ) %>% 
  left_join(votingbycounty, join_by(county2 == county, adjusted_year2 == year)) %>% 
  rename(
    registered_voters_county_2 = registered_voters,
    voted_county_2 = voted,
  ) %>% 
  left_join(votingbycounty, join_by(county3 == county, adjusted_year2 == year)) %>% 
  rename(
    registered_voters_county_3 = registered_voters,
    voted_county_3 = voted,
  )


##----------------------------------------------------------------
##    Calculate Imputed and Actual turnout for each election     -
##----------------------------------------------------------------

# First, for cities only in one county
temp1 = merged_data %>%
  filter(!is.na(city) & is.na(county2)) %>% 
  mutate(
    voters = population_city/population_county_1*registered_voters_county_1,
    voted = population_city/population_county_1*voted_county_1
  )

# cities in 2 counties
temp2 = merged_data %>%
  filter(!is.na(city) & !is.na(county2) & is.na(county3)) %>% 
  mutate(
    voters = 0.5*(population_city/population_county_1*registered_voters_county_1 +
                    population_city/population_county_2*registered_voters_county_2),
    voted = 0.5*(population_city/population_county_1*voted_county_1 +
                   population_city/population_county_1*voted_county_1)
  )

# cities in 3 counties
temp3 = merged_data %>%
  filter(!is.na(city) & !is.na(county3)) %>% 
  mutate(
    voters = (1/3)*(population_city/population_county_1*registered_voters_county_1 +
                      population_city/population_county_2*registered_voters_county_2),
    voted = (1/3)*(population_city/population_county_1*voted_county_1 +
                     population_city/population_county_1*voted_county_1)
  )

# Then, for counties
temp = merged_data %>% 
  filter(is.na(city) & is.na(precinct)) %>% 
  mutate(
    voters = registered_voters_county_1,
    voted = voted_county_1
  )

# putting it together (will exclude precinct-level elections)
final_data = bind_rows(temp1, temp2, temp3, temp) %>% 
  mutate(
    actual_turnout = (for_vote + against_vote)/voters,
    imputed_turnout = voted/voters
  ) %>% 
  # leave out prohibitory elections
  filter(prohibitory == 0)

write.csv(final_data, "data/clean/merged/election_level_with_turnout_and_population.csv", row.names = F)

##################################################################
##                 ANALYSIS (TABLES AND GRAPHS)                 ##
##################################################################

# plot distributions

final_data %>% 
  filter(actual_turnout <= 1) %>% 
  ggplot() +
  geom_density(aes(x = actual_turnout, y = after_stat(density), fill = "actual"), alpha = 0.3) +
  geom_density(aes(x = imputed_turnout, y = after_stat(density), fill = "imputed"), alpha = 0.3) +
  scale_fill_manual(values = c("blue", "red"), labels = c("actual", "imputed")) +
  labs(x = "turnout", title = "Distribution of election turnout in jurisdictions (cities/counties)") +
  theme_bw() +
  theme(
    legend.position = "bottom",  # Inside the plot area
    legend.title = element_blank()
  )

# passed vs failed

final_data %>% 
  filter(actual_turnout <= 1) %>% 
  ggplot(aes(x = actual_turnout, fill = result)) +
  geom_density(alpha = 0.3) +
  theme_bw() +
  theme(
    legend.position = "bottom",  # Position the legend inside the plot area
    legend.title = element_blank()  # Remove the legend title
  )

final_data %>% 
  filter(actual_turnout <= 1) %>% 
  filter(issue.clean %in% c(
    "legal sale of beer and wine for off-premises consumption only",
    "legal sale of mixed beverages in restaurants by food and beverage certificate holders only",
    "legal sale of all alcoholic beverages including mixed beverages",
    "legal sale of all alcoholic beverages for off-premises consumption only"
  )) %>% 
  ggplot(aes(x = actual_turnout, fill = issue.short)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~ issue.short)+
  theme_bw() +
  theme(
    legend.position = "none",  # Position the legend inside the plot area
  )

# Time series on turnout figures

temp = final_data %>% 
  group_by(adjusted_year2) %>% 
  summarise(
    imputed_turnout_mean = mean(imputed_turnout, na.rm = T),
    imputed_turnout_ci = sd(imputed_turnout, na.rm = T)/sqrt(n())*1.96
  ) %>% 
  left_join(votingall, join_by(adjusted_year2 == year))

temp1 = final_data %>% 
  group_by(election_year) %>% 
  summarise(
    actual_turnout_mean = mean(actual_turnout, na.rm = T),
    actual_turnout_ci = sd(actual_turnout, na.rm = T)/sqrt(n())*1.96,
  )

temp = bind_rows(temp, temp1)

temp %>% 
  ggplot(aes(x = adjusted_year2)) +

  # All texas  
  geom_line(aes(y = all_texas_turnout, color = "All Texas"), size = 1, linetype = "dashed") +
  geom_point(aes(y = all_texas_turnout, color = "All Texas"), size = 1.5) +
  
  # Actual 
  geom_ribbon(aes(x = election_year, ymin = actual_turnout_mean - actual_turnout_ci, ymax = actual_turnout_mean + actual_turnout_ci,
                  fill = "actual"), alpha = 0.1) +
  geom_line(aes(x = election_year, y = actual_turnout_mean, color = "actual"), size = 1) +
  geom_point(aes(x = election_year, y = actual_turnout_mean, color = "actual"), size = 1.5) +
  
  # Imputed
  geom_ribbon(aes(ymin = imputed_turnout_mean - imputed_turnout_ci, ymax = imputed_turnout_mean + imputed_turnout_ci,
                  fill = "imputed"), alpha = 0.1) +
  geom_line(aes(y = imputed_turnout_mean, color = "imputed"), size = 1) +
  geom_point(aes(y = imputed_turnout_mean, color = "imputed"), size = 1.5) +
  
  scale_x_continuous(breaks = unique(temp$adjusted_year2)) +
  
  scale_fill_manual(values = c("blue", "red"), labels = c("actual", "imputed")) +
  scale_color_manual(values = c("blue", "black", "red"), labels = c( "actual", "All Texas", "imputed")) +
  
  labs(x = "Election year", y = "Turnout", title = "Voter turnout from 1997-2019") +
  theme_bw() +
  
  # Customize the legend position and appearance
  theme(
    legend.position = "bottom",  # Position the legend at the bottom
    legend.title = element_blank()  # Remove the legend title
  ) +
  
  # Combine legends
  guides(fill = "none")


# Scatter plots

# actual turnout vs For Vote Share

temp = final_data %>% 
  filter(!is.na(actual_turnout) & actual_turnout <= 1) %>% 
  mutate(
    for_vote_share = for_vote/(for_vote+against_vote)
  )

ggplot(temp, aes(x = for_vote_share, y = actual_turnout))+
  geom_point() +
  geom_smooth(method = "lm", se = T)+
  theme_bw()

ggplot(temp, aes(x = for_vote_share, y = imputed_turnout))+
  geom_point() +
  geom_smooth(method = "lm", se = T)+
  theme_bw()


# actual turnout vs City population

final_data %>% 
  filter(!is.na(actual_turnout) & actual_turnout <= 1 & !is.na(population_city)) %>% 
  ggplot(aes(x = log(population_city), y = actual_turnout)) +
  geom_point() +
  geom_smooth(method = "lm", se = T)+
  theme_bw()

final_data %>% 
  filter(!is.na(imputed_turnout) & !is.na(population_city)) %>% 
  ggplot(aes(x = log(population_city), y = imputed_turnout)) +
  geom_point() +
  geom_smooth(method = "lm", se = T)+
  theme_bw()
