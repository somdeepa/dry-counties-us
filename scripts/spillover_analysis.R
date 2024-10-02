
##################################################################
##                      SPILLOVER ANALYSIS                      ##
##################################################################
crosswalk_maps = read.csv("data/clean/city-name-xwalk/city_name_crosswalk_mapsdata.csv")

city_boundaries = read_sf("data/raw/geography/TxDOT_City_Boundaries_-7440712964994907480/Cities.shp") %>% 
  left_join(crosswalk_maps, join_by("CITY_NM" == "mapsdata")) %>% 
  st_transform(., crs = 32614) %>% 
  mutate(id = row_number())

ggplot(city_boundaries %>% filter(id %in% c(345,138))) +
  geom_sf() +
  theme_void()

cityid = city_boundaries %>% select(CITY_NM, id, keyformerge) %>% st_drop_geometry()

city_centroids = st_centroid(city_boundaries)

# Calculate pairwise distances (in meters) and convert to miles
distance_matrix = st_distance(city_centroids) %>% 
  set_units(., "miles") %>% 
  as.matrix()

rownames(distance_matrix) = city_boundaries$id
colnames(distance_matrix) = city_boundaries$id

threshold=set_units(10, "miles")

threshold_matrix = ifelse(distance_matrix<= threshold, 1, 0)
diag(threshold_matrix) = 0

##---------------------------------------------------------------
##                    ALWAYS TREATED CITIES                     -
##---------------------------------------------------------------

# Isolate always treated cities - cities that were wet to some degree by 1996 
# before elections data starts

crosswalk_elections = read.csv("data/clean/city-name-xwalk/city_name_crosswalk_electiondata.csv")

electiondata = read.csv("data/clean/elections-data/elections-data-97-2020-cleaned.csv") %>% 
  left_join(crosswalk_elections,join_by("city" == "electiondata"))

policychanges = electiondata %>% 
  filter(!is.na(city) & result == "passed" & status_before == "dry") %>% 
  mutate(
    # clean up dates
    election_date = as.Date(election_date, format = "%Y-%m-%d"),
    election_year = year(election_date),
  ) %>% 
  group_by(keyformerge) %>% 
  summarise(
    first_policy_change = min(election_year),
    first_policy_change_quarter = min(sapply(election_date, convert_to_quarter))
  ) %>% 
  left_join(cityid, by = "keyformerge") %>% 
  select(-CITY_NM)

# cities that were dry during or any point after 1997

exlude_cities = electiondata %>% 
  filter(!is.na(city) & status_before == "dry") %>% 
  select(city, keyformerge) %>% 
  unique()
  
always_treated_cities = anti_join(crosswalk_maps, exlude_cities, by = "keyformerge") %>% 
  filter(!is.na(mapsdata)) %>% 
  left_join(cityid, join_by("mapsdata" == "CITY_NM")) %>% 
  select(-keyformerge.y) %>% 
  rename(keyformerge = keyformerge.x)

countStatusChangeNeighbour = function(cityid){
  neighbors = colnames(threshold_matrix)[threshold_matrix[cityid, ] == 1]
  nearby_status_changes = policychanges %>% 
    filter(id %in% neighbors)
  
  nearby_status_change_count = nrow(nearby_status_changes)
  
  df = data.frame(id = cityid, 
                  neighbors = length(neighbors), 
                  nearby_status_change_count = nearby_status_change_count,
                  first_policy_change = ifelse(nearby_status_change_count>0,
                                               min(nearby_status_changes$first_policy_change),0),
                  first_policy_change_quarter = ifelse(nearby_status_change_count>0,
                                                       min(nearby_status_changes$first_policy_change_quarter),0)
  )
  return(df)
}

temp = as.data.frame(do.call(rbind, lapply(always_treated_cities$id, countStatusChangeNeighbour)))

# read licenses data

crosswalk_licenses = read.csv("data/clean/city-name-xwalk/city_name_crosswalk_licensesdata.csv")

city_licenses_panel = read.csv("data/clean/liquor-licenses/quarterly_city_panel_all_licenses.csv") %>% 
  left_join(crosswalk_licenses, join_by("city" == "licensesdata"))

crosswalk_popdata = read.csv("data/clean/city-name-xwalk/city_name_crosswalk_popdata.csv")

populationdata = read.csv("data/clean/population/texas_population_by_cities_1990_2023.csv") %>% 
  left_join(crosswalk_popdata, join_by("city" == "popdata")) %>% 
  select(-city)

all_combinations = expand.grid(
  keyformerge = always_treated_cities$keyformerge,
  quarter = quarters
) %>% 
  mutate(year = startYear + floor(quarter/4))

final_panel = all_combinations %>% 
  # join license data
  left_join(city_licenses_panel, join_by(keyformerge, quarter, year)) %>% 
  
  # change NAs to 0 (Missing data here => true zeros)
  mutate(total_businesses = ifelse(is.na(total_businesses), 0, total_businesses)) %>% 
  
  # add population 
  left_join(populationdata, join_by(keyformerge, year)) %>% 
  
  left_join(cityid, by = "keyformerge") %>% 
  left_join(temp, by = "id") %>% 
  mutate(
    # treated vs never treated indicator
    treat = ifelse(nearby_status_change_count>0,1,0),
    # calculate licenses per 1000 population
    licensepop = total_businesses/population*1000,
    # create post indicator
    post_treatment_quarter = ifelse(quarter >= first_policy_change_quarter, 1, 0),
    # create event study dummy (assigning 0 to never-treated)
    periods_from_treatment = ifelse(nearby_status_change_count>0,
                                    quarter - first_policy_change_quarter,0)
  )

mod.twfe = feols(
  licensepop ~ post_treatment_quarter | keyformerge + quarter,
  cluster = "keyformerge",
  data = final_panel
)


mod.es = feols(
  licensepop ~ i(periods_from_treatment, treat, ref = -1) | keyformerge + quarter,
  cluster = "keyformerge",
  data = final_panel
)

png("results/weekly updates/2-10/event_study_plot_spillover_20mi.png", width = 600, height = 500)

iplot(mod.es,  xlim = c(-20, 20), ylim = c(-3, 2))

dev.off()  # Close the device



# CS estimator

model.cs = att_gt(yname = "licensepop",
                  gname = "first_policy_change_quarter",
                  idname = "id",
                  tname = "quarter",
                  xformla = ~1,
                  data = final_panel,
                  base_period = "universal",
                  bstrap = F,
                  cband = F
)

agg_cs_res = aggte(model.cs, type = "dynamic",min_e = -20, max_e = 20, na.rm = T)

ggdid(agg_cs_res, xgap = 4, ylab = "Estimate and 95% interval", xlab = "Quarters since change of status",
      theming = F) +
  geom_errorbar(color = "black", width = 0.2) +
  geom_point(color = "black") +
  labs(title = "") +
  theme_bw() +
  theme(
    legend.position = "none"
  )

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
  geom_errorbar(mapping = aes(x = periods_from_treatment, ymin = Lower, ymax = Upper),
                width = 0.2, linewidth =0.5) +
  geom_line(aes(x = periods_from_treatment, y = mean_licensepop))+
  geom_point(aes(x = periods_from_treatment, y = mean_licensepop), size = 1.5, shape = 21, fill = "black") +
  scale_x_continuous(breaks = seq(-20,20, 4)) +
  labs(x = "Quarters",
       y = "Licenses per 1000 population") +
  theme_minimal()

# residualizing

mod.resids = feols(
  licensepop ~ 1 | keyformerge + quarter,
  cluster = "keyformerge",
  data = final_panel
)

data = final_panel %>% 
  filter(!is.na(licensepop)) %>% 
  mutate(residuals = residuals(mod.resids))

summarytable = data %>%
  filter(periods_from_treatment %in% seq(-20, 20)) %>%
  group_by(periods_from_treatment) %>%
  summarise(
    mean_residuals = mean(residuals, na.rm = TRUE),
    Lower = mean_residuals - 1.96 * sd(residuals, na.rm = TRUE) / sqrt(n()),
    Upper = mean_residuals + 1.96 * sd(residuals, na.rm = TRUE) / sqrt(n())
  )

ggplot(summarytable) +
  geom_errorbar(mapping = aes(x = periods_from_treatment, ymin = Lower, ymax = Upper),
                width = 0.2, linewidth =0.5) +
  geom_line(aes(x = periods_from_treatment, y = mean_residuals))+
  geom_point(aes(x = periods_from_treatment, y = mean_residuals), size = 1.5, shape = 21, fill = "black") +
  scale_x_continuous(breaks = seq(-20,20, 4)) +
  labs(x = "Quarters",
       y = "Residuals") +
  theme_minimal()

ggsave("results/weekly updates/2-10/raw_data_residual_plot_spillover_10mi.png", width = 6, height = 5, device = "png")

##---------------------------------------------------------------
##              CONTINUOUS DISTANCE SPECIFICATION               -
##---------------------------------------------------------------

# build of panel of city status (only treated cities)

is_dry = expand.grid(
  keyformerge = policychanges$keyformerge,
  quarter = quarters
) %>% 
  left_join(policychanges, by = "keyformerge") %>% 
  mutate(
    dry = ifelse(quarter<first_policy_change_quarter, 1, 0)
  )

all_combinations = expand.grid(
  id = always_treated_cities$id,
  quarter = quarters
)

nearestDryCity = function(cityid, curr_quarter){
  neighbors = colnames(threshold_matrix)[threshold_matrix[cityid, ] == 1]
  
  # dry cities in current quarter
  dry_cities_current = is_dry %>% filter(quarter==curr_quarter & dry == 1 )
  
  # count number of dry cities within the threshold
  dry_neighbors = sum(colnames(threshold_matrix)[threshold_matrix[cityid, ] == 1] %in% dry_cities_current$id)
  
  # calculate minimum distance from a dry city
  min_distance = min(distance_matrix[cityid, dry_cities_current$id], na.rm = T)
  
  df = data.frame(id = cityid, 
                  quarter = curr_quarter,
                  dry_neighbors = dry_neighbors,
                  min_distance = min_distance
  )
  return(df)
}

temp = as.data.frame(do.call
                     (rbind, mapply(nearestDryCity, 
                                    cityid = all_combinations$id, 
                                    curr_quarter = all_combinations$quarter, 
                                    SIMPLIFY = FALSE)))

# add back to the earlier panel

data = final_panel %>% 
  left_join(temp, join_by(id, quarter))

# continuous distance model

mod.cont1 = feols(
  licensepop ~ min_distance | keyformerge + quarter,
  cluster = "keyformerge",
  data = data
)

mod.cont2 = feols(
  licensepop ~ dry_neighbors | keyformerge + quarter,
  cluster = "keyformerge",
  data = data
)

mod.cont3 = feols(
  licensepop ~ post_treatment_quarter + min_distance + dry_neighbors | keyformerge + quarter,
  cluster = "keyformerge",
  data = data
)

models = list(mod.twfe,mod.cont2, mod.cont1, mod.cont3)

modelsummary(models,
             #title = "",
             coef_rename = c("post_treatment_quarter" = "Post indicator",
                             "min_distance" = "Distance from nearest dry city",
                             "dry_neighbors" = "No. of dry neighbors"),
             gof_omit = 'R2 |R2 Within|R2 Within Adj.|AIC|BIC|RMSE|Std',
             gof_map = list(
               list("raw" = "Mean of DV", "clean" = "Mean of Dep. Var.", fmt = 3),
               list("raw" = "nobs", "clean" = "N", "fmt" = 0),
               list("raw" = "adj.r.squared", "clean" = "$R^2$", "fmt" = 2),
               list("raw" = "FE: keyformerge", "clean" = "City fixed effects", fmt = 0),
               list("raw" = "FE: quarter", "clean" = "Time fixed effects", fmt = 0)),
             "modelsummary_format_numeric_latex" = "plain",
             align = "lcccc"
             #output = "results/weekly updates/2-10/table.html"
             )
