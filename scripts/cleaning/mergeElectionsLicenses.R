
##################################################################
##         Merge Election Results and City/County names         ##
##################################################################

crosswalk_cityname_election = read.csv("data/clean/city-name-xwalk/city_name_crosswalk_electiondata.csv")

electiondata = read.csv("data/clean/elections-data/elections-data-97-2020-cleaned.csv")

crosswalk_cityname_popdata = read.csv("data/clean/city-name-xwalk/city_name_crosswalk_popdata.csv")

populationdata = read.csv("data/clean/population/texas_population_by_cities_decennial.csv") %>% 
  left_join(crosswalk_cityname_popdata, join_by(city == popdata))

policychanges = electiondata %>% 
  filter(!is.na(city) & result == "passed" & status_before == "dry") %>% 
  mutate(
    # clean up dates
    election_date = as.Date(election_date, format = "%Y-%m-%d"),
    election_year = year(election_date),
  )
  group_by(city) %>% 
  summarise(
    first_policy_change = min(election_year),
    first_policy_change_quarter = min(sapply(election_date, convert_to_quarter))
    )
policychanges = policychanges %>% 
  mutate(
    id = seq(1:nrow(policychanges))
  )%>% 
  left_join(crosswalk_cityname_election, join_by(city == electiondata))

# Counties dry at the end of 2019
# dry_counties = c("Borden", "Kent", "Throckmorton", "Hemphill", "Roberts")

crosswalk_cityname_licensesdata = read.csv("data/clean/city-name-xwalk/city_name_crosswalk_licensesdata.csv")


# Merge for CITY-QUARTER-ALL


city_licenses_panel = read.csv("data/clean/liquor-licenses/quarterly_city_panel_all_licenses.csv") %>% 
  left_join(crosswalk_cityname_licensesdata, join_by(city == licensesdata))
  

all_combinations = expand.grid(
  city = policychanges$city,
  quarter = quarters
) %>% 
  left_join(crosswalk_cityname_election, join_by(city == electiondata))

final_panel = all_combinations %>% 
  # join license data
  left_join(city_licenses_panel, join_by(keyformerge, quarter)) %>% 
  # change NAs to 0 (Missing data here => true zeros)
  mutate(total_businesses = ifelse(is.na(total_businesses), 0, total_businesses)) %>% 
  # add policy changes
  left_join(policychanges, by = "keyformerge") %>% 
  # add population 
  left_join(populationdata, join_by(keyformerge, adjusted_year == year)) %>% 
  mutate(
    # calculate licenses per 1000 population
    licensepop = total_businesses/population*1000,
    # create treatment dummy
    treatment_quarter = ifelse(quarter >= first_policy_change_quarter, 1, 0),
    # create event study dummy
    periods_from_treatment = quarter - first_policy_change_quarter
  )

write.csv(final_panel, "data/clean/merged/licenses-elections/all_licenses_city_quarter.csv", row.names = F)


# Merge for CITY-QUARTER-OFF/ON

city_licenses_panel = read.csv("data/clean/liquor-licenses/quarterly_city_panel_off_on.csv") %>% 
  left_join(crosswalk_cityname_licensesdata, join_by(city == licensesdata))

# On premise
city_licenses_panel_on = city_licenses_panel %>% filter(category== "On-Premise")

final_panel = all_combinations %>% 
  # join license data
  left_join(city_licenses_panel_on, join_by(keyformerge, quarter)) %>% 
  # change NAs to 0 (Missing data here => true zeros)
  mutate(total_businesses = ifelse(is.na(total_businesses), 0, total_businesses)) %>% 
  # add policy changes
  left_join(policychanges, by = "keyformerge") %>% 
  # add population
  left_join(populationdata, join_by(keyformerge, adjusted_year == year)) %>% 
  mutate(
    # calculate licenses per 1000 population
    licensepop = total_businesses/population*1000,
    # create treatment dummy
    treatment_quarter = ifelse(quarter >= first_policy_change_quarter, 1, 0),
    # create event study dummy
    periods_from_treatment = quarter - first_policy_change_quarter
  )

write.csv(final_panel, "data/clean/merged/licenses-elections/on_premise_licenses_city_quarter.csv", row.names = F)


# Off premise
city_licenses_panel_off = city_licenses_panel %>% filter(category== "Off-Premise")

final_panel = all_combinations %>% 
  # join license data
  left_join(city_licenses_panel_off, join_by(keyformerge, quarter)) %>% 
  # change NAs to 0 (Missing data here => true zeros)
  mutate(total_businesses = ifelse(is.na(total_businesses), 0, total_businesses)) %>% 
  # add policy changes
  left_join(policychanges, by = "keyformerge") %>% 
  # add population figure
  left_join(populationdata, join_by(keyformerge, adjusted_year == year)) %>% 
  mutate(
    # calculate licenses per 1000 population
    licensepop = total_businesses/population*1000,
    # create treatment dummy
    treatment_quarter = ifelse(quarter >= first_policy_change_quarter, 1, 0),
    # create event study dummy
    periods_from_treatment = quarter - first_policy_change_quarter
  ) %>% 
  group_by(keyformerge) %>% 
  ungroup()

write.csv(final_panel, "data/clean/merged/licenses-elections/off_premise_licenses_city_quarter.csv", row.names = F)

# Note: The code below needs to be updated

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
