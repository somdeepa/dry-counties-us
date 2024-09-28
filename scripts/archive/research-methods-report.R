# Final project results for research methods
# Also for Public 2 enhanced proposal
# Note: most of the code will be re-purposed from earlier scripts
# Also, this is the first time my unit of observation is cities, not counties.

# Last updated: 30-04-2024

# read data

popdata = read.csv("data/clean/texas_population_by_cities_2010_2019.csv") %>% select(2:4) %>% 
  mutate(city = tolower(gsub(" city| village| town", "", city))) %>% 
  arrange(city, year)

crashes = read.csv("data/clean/texas_crashes_by_cities_2003_2019.csv") %>% select(2:4) %>% 
  mutate(city=tolower(city)) %>% 
  arrange(city, year)

elections = read.csv("data/clean/local-option-elections-97-2020.csv")

# Make panel of crashes and population

relevant_elections = elections %>% 
  filter(is.na(jp_precinct)) %>% 
  filter(!is.na(city)) %>% 
  filter(!is.na(result)) %>% 
  filter(!(result=="passed" & status_current=="dry")) %>% 
  mutate(city = trimws(tolower(gsub("City of|City ", "", city))))

failed_elections = relevant_elections %>% 
  filter(result == "failed")

passed_elections = relevant_elections %>% 
  filter(result=="passed") %>% 
  arrange(city, fiscal_year) %>% 
  distinct(city, .keep_all = T)

merged_elections = bind_rows(failed_elections, passed_elections)

treatcities = passed_elections %>%   
  select(X, city, fiscal_year) %>% 
  rename(
    treatment_year = fiscal_year,
    id = X
    )

# crash rate panel

pop2010 = popdata %>% filter(year == 2010) %>% select(!year) %>% 
  distinct(city, .keep_all = T)

crashpanel = left_join(crashes, pop2010, by = "city") %>% 
  mutate(crash_rate = annual_dui_crashes/population*1000)

panel = left_join(treatcities, crashpanel, by = "city")

# Create post-treatment indicator
df = panel %>% mutate(postTreated = !is.na(treatment_year) & year >= treatment_year)

twfe_results_crash = feols(crash_rate ~ postTreated | id + year, df,
                           cluster = "id")

cs_results_crash <- att_gt(
  yname = "crash_rate",
  tname = "year",
  idname = "id",
  gname = "treatment_year",
  data = panel,
  control_group = "notyettreated"
)

cs_result_agg_crash = aggte(cs_results_crash, type = "simple")

es_crash <- aggte(cs_results_crash,
                  type = "dynamic",
                  min_e = -3, max_e = 3
)

ggdid(es_crash, title = ".")

es_summary_crash = data.frame(
  group = seq(-3, 3),
  att = es_crash$att.egt,
  se = es_crash$se.egt
) %>% 
  mutate(
    ymin = att - 1.96*se,
    ymax = att + 1.96*se
  )

ggplot(es_summary_crash) +
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "dimgrey", size = 1) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_errorbar(mapping = aes(x = group, ymin = ymin, ymax = ymax),
                width = 0.2, size =1, color = "orange") +
  geom_point(aes(x = group, y = att), size = 3, shape = 21, fill = "dimgrey") +
  scale_x_continuous(breaks = seq(-3,3)) +
  labs(x = "Years since change of status",
       y = "ATT") +
  theme_minimal()

ggsave("results/research-methods-final-report//crashesATT.pdf", device = "pdf")

##-------------------
##  PLOT CRASH RATE  
##-------------------

summary_crashes = panel %>% 
  mutate(group = year-treatment_year) %>% 
  filter(group %in% seq(-5,5)) %>% 
  group_by(group) %>% 
  summarise(
    mean_crash_rate = mean(crash_rate, na.rm = T),
    Lower = mean_crash_rate - 1.96*sd(crash_rate, na.rm=T)/sqrt(n()),
    Upper = mean_crash_rate + 1.96*sd(crash_rate, na.rm=T)/sqrt(n()),
  )

ggplot(summary_crashes) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "dimgrey", size = 1) +
  geom_errorbar(mapping = aes(x = group, ymin = Lower, ymax = Upper),
                width = 0.2, size =1, color = "orange") +
  geom_point(aes(x = group, y = mean_crash_rate), size = 3, shape = 21, fill = "dimgrey") +
  scale_x_continuous(breaks = seq(-5,5)) +
  labs(
    x = "Years since change of status",
    y = "Annual crashes per 1000 population") +
  theme_minimal()

