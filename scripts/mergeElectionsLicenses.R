
##################################################################
##         Merge Election Results and City/County names         ##
##################################################################

electiondata = read.csv("data/clean/elections-data/elections-data-97-2020-cleaned.csv") %>% 
  mutate(
    election_date = as.Date(election_date, format = "%Y-%m-%d"),
    election_year = year(election_date)
  )

crosswalk_citynames = read.csv("data/clean/merged/city_name_crosswalk.csv")

populationdata = read.csv("data/clean/population/texas_population_by_cities_decennial.csv")

policychanges = electiondata %>% 
  filter(!is.na(city) & result == "passed") %>% 
  group_by(city) %>% 
  summarise(
    first_policy_change = min(election_year),
    first_policy_change_quarter = min(sapply(election_date, convert_to_quarter))
    )
policychanges = policychanges %>% 
  mutate(
    id = seq(1:nrow(policychanges))
  )

# Counties dry at the end of 2019
dry_counties = c("Borden", "Kent", "Throckmorton", "Hemphill", "Roberts")

# select 

# Merge for CITY-QUARTER-ALL

city_licenses_panel = read.csv("data/clean/liquor-licenses/quarterly_city_panel_all_licenses.csv") %>% 
  left_join(crosswalk_citynames, join_by(city == licensesdata))
  

all_combinations = expand.grid(
  city = policychanges$city,
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

# plot raw data

summarytable = final_panel %>% 
  filter(periods_from_treatment %in% seq(-20,20)) %>% 
  group_by(periods_from_treatment) %>% 
  summarise(
    mean_licensepop = mean(licensepop, na.rm = T),
    Lower = mean_licensepop - 1.96*sd(licensepop, na.rm=T)/sqrt(n()),
    Upper = mean_licensepop + 1.96*sd(licensepop, na.rm=T)/sqrt(n()),
  )

ggplot(summarytable) +
  
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  geom_errorbar(mapping = aes(x = periods_from_treatment, ymin = Lower, ymax = Upper),
                width = 0.3, linewidth =0.5) +
  geom_point(aes(x = periods_from_treatment, y = mean_licensepop), size = 2, shape = 21, fill = "black") +
  scale_x_continuous(breaks = seq(-20,20, 4)) +
  labs(x = "Quarters",
       y = "Licenses per 1000 population") +
  theme_minimal()

ggsave("results/weekly updates/13-08/license_event_study_raw_data_all_licenses.png", device = "png")

# calloway-santanna results

cs_res = att_gt(yname = "licensepop",
                gname = "first_policy_change_quarter",
                idname = "id",
                tname = "quarter",
                xformla = ~1,
                data = final_panel,
                control_group = "notyettreated",
                base_period = "universal"
)

agg_cs_res <- aggte(cs_res, type = "dynamic",min_e = -20, max_e = 20, na.rm = T)

ggdid(agg_cs_res, title = "", xgap = 4, ylab = "Alcohol licenses per 1000 population", xlab = "quarters")

ggsave("results/weekly updates/13-08/licenses_quarterly_all.png", device = "png")


# Merge for CITY-QUARTER-OFF/ON

city_licenses_panel = read.csv("data/clean/liquor-licenses/quarterly_city_panel_off_on.csv") %>% 
  left_join(crosswalk_citynames, join_by(city == licensesdata))

# On premise
city_licenses_panel_on = city_licenses_panel %>% filter(category== "On-Premise")
all_combinations = expand.grid(
  city = policychanges$city,
  quarter = quarters
)

final_panel = all_combinations %>% 
  # join license data
  left_join(city_licenses_panel_on, join_by(city == electiondata, quarter)) %>% 
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
  ungroup() %>% 
  filter(!is.infinite(licensepop))

# plot raw data

summarytable = final_panel %>% 
  filter(periods_from_treatment %in% seq(-20,20)) %>% 
  group_by(periods_from_treatment) %>% 
  summarise(
    mean_licensepop = mean(licensepop, na.rm = T),
    Lower = mean_licensepop - 1.96*sd(licensepop, na.rm=T)/sqrt(n()),
    Upper = mean_licensepop + 1.96*sd(licensepop, na.rm=T)/sqrt(n()),
  )

ggplot(summarytable) +
  
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  geom_errorbar(mapping = aes(x = periods_from_treatment, ymin = Lower, ymax = Upper),
                width = 0.3, linewidth =0.5) +
  geom_point(aes(x = periods_from_treatment, y = mean_licensepop), size = 2, shape = 21, fill = "black") +
  scale_x_continuous(breaks = seq(-20,20, 4)) +
  labs(x = "Quarters",
       y = "On-Premise Licenses per 1000 population") +
  theme_minimal()

ggsave("results/weekly updates/13-08/license_event_study_raw_data_on_premise_quaterly.png", device = "png")


# calloway-santanna results

cs_res = att_gt(yname = "licensepop",
                gname = "first_policy_change_quarter",
                idname = "id",
                tname = "quarter",
                xformla = ~1,
                data = final_panel,
                control_group = "notyettreated",
                base_period = "universal"
)

agg_cs_res <- aggte(cs_res, type = "dynamic",min_e = -20, max_e = 20)

ggdid(agg_cs_res, title = "", xgap = 4, ylab = "On-premise licenses per 1000 population", xlab = "quarters")

ggsave("results/weekly updates/13-08/licenses_quarterly_on_premise.png", device = "png")

# Off premise
city_licenses_panel_off = city_licenses_panel %>% filter(category== "Off-Premise")


final_panel = all_combinations %>% 
  # join license data
  left_join(city_licenses_panel_off, join_by(city == electiondata, quarter)) %>% 
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
  ungroup() %>% 
  filter(!is.infinite(licensepop))

# plot raw data

summarytable = final_panel %>% 
  filter(periods_from_treatment %in% seq(-20,20)) %>% 
  group_by(periods_from_treatment) %>% 
  summarise(
    mean_licensepop = mean(licensepop, na.rm = T),
    Lower = mean_licensepop - 1.96*sd(licensepop, na.rm=T)/sqrt(n()),
    Upper = mean_licensepop + 1.96*sd(licensepop, na.rm=T)/sqrt(n()),
  )

ggplot(summarytable) +
  
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  geom_errorbar(mapping = aes(x = periods_from_treatment, ymin = Lower, ymax = Upper),
                width = 0.3, linewidth =0.5) +
  geom_point(aes(x = periods_from_treatment, y = mean_licensepop), size = 2, shape = 21, fill = "black") +
  scale_x_continuous(breaks = seq(-20,20, 4)) +
  labs(x = "Quarters",
       y = "Off-Premise Licenses per 1000 population") +
  theme_minimal()

ggsave("results/weekly updates/13-08/license_event_study_raw_data_off_premise_quaterly.png", device = "png")


# calloway-santanna results

cs_res = att_gt(yname = "licensepop",
                gname = "first_policy_change_quarter",
                idname = "id",
                tname = "quarter",
                xformla = ~1,
                data = final_panel,
                control_group = "notyettreated",
                base_period = "universal"
)

agg_cs_res <- aggte(cs_res, type = "dynamic",min_e = -20, max_e = 20)

ggdid(agg_cs_res, title = "", xgap = 4, ylab = "Off-premise licenses per 1000 population", xlab = "quarters")

ggsave("results/weekly updates/13-08/licenses_quarterly_off_premise.png", device = "png")

# Merge for CITY-YEAR-ALL

city_licenses_panel = read.csv("data/clean/liquor-licenses/city_panel_all_licenses.csv") %>% 
  left_join(crosswalk_citynames, join_by(city == licensesdata))
  
  
  all_combinations = expand.grid(
    city = policychanges$city,
    year = years)


  
final_panel = all_combinations %>% 
  # join license data
  left_join(city_licenses_panel, join_by(city == electiondata, year)) %>% 
  # add policy changes
  left_join(policychanges, by = "city") %>% 
  # add population figure
  left_join(populationdata, join_by(popdata == city, adjusted_year == year)) %>% 
  mutate(
    # calculate licenses per 1000 population
    licensepop = total_businesses/population*1000,
    periods_from_treatment = year - first_policy_change
  ) %>% 
  group_by(city) %>%
  ungroup() %>% 
  filter(!is.infinite(licensepop))

# plot raw data

summarytable = final_panel %>% 
  filter(periods_from_treatment %in% seq(-5,5)) %>% 
  group_by(periods_from_treatment) %>% 
  summarise(
    mean_licensepop = mean(licensepop, na.rm = T),
    Lower = mean_licensepop - 1.96*sd(licensepop, na.rm=T)/sqrt(n()),
    Upper = mean_licensepop + 1.96*sd(licensepop, na.rm=T)/sqrt(n()),
  )

ggplot(summarytable) +
  
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  geom_errorbar(mapping = aes(x = periods_from_treatment, ymin = Lower, ymax = Upper),
                width = 0.3, linewidth =0.5) +
  geom_point(aes(x = periods_from_treatment, y = mean_licensepop), size = 2, shape = 21, fill = "black") +
  scale_x_continuous(breaks = seq(-5,5)) +
  labs(x = "Quarters",
       y = "Licenses per 1000 population") +
  theme_minimal()

ggsave("results/weekly updates/13-08/license_event_study_raw_data_all_licenses_annual.png", device = "png")

# calloway-santanna results

cs_res = att_gt(yname = "licensepop",
                gname = "first_policy_change",
                idname = "id",
                tname = "year",
                xformla = ~1,
                data = final_panel,
                control_group = "notyettreated",
                base_period = "universal"
)

agg_cs_res <- aggte(cs_res, type = "dynamic",min_e = -5, max_e = 5)

ggdid(agg_cs_res, title = "", xgap = 1, ylab = "Alcohol licenses per 1000 population", xlab = "years")

ggsave("results/weekly updates/13-08/licenses_quarterly_all_years.png", device = "png")
