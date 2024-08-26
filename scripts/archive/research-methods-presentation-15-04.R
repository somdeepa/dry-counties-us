# Presentation 2 Research Methods figures and tables

# Only work with county-wide elections 2004-2020

# Last-updated: 1-05-2024

##---------------------------------------------------------------
##                          LOAD DATA                           -
##---------------------------------------------------------------

# read elections data

elections = read.csv("data/clean/manual/local-option-elections-97-2020-manual.csv")

# read fips codes (county)

countyfips = read.csv("data/clean/texas_county_fips.csv")

# population by county
popdata = read.csv("data/clean/texas_population_by_county_1997_2022.csv") %>% select(2:4)

popyoungdata = read.csv("data/clean/texas_population_by_county_1997_2022_ages_20-34.csv") %>% 
  select(2:4)

# Texas borders shapefile

county_shapefile = st_read("data/raw/Texas_County_Boundaries_Detailed_6830276903728206656/County.shp") %>% 
  mutate(countyfips = as.integer(`CNTY_FIPS`)-48000)

# Crashes data

crashes = read.csv("data/clean/texas_crashes_by_county_2003_2019.csv") %>% select(2:4)

##----------------------------------------------------------------
##                        TREATED COUNTIES                       -
##----------------------------------------------------------------

countywide_elections = elections %>% 
  filter(is.na(city) & is.na(jp_precinct))

# extract only successful dry->wet elections

pass_election = countywide_elections %>% filter(result == "passed") %>% 
  filter(status_current!="dry") %>%  # exclude prohibitory election
  group_by(county) %>% 
  slice(1)

treatcounties = pass_election %>%   
  select(county, fiscal_year) %>% 
  rename(treatment_year = fiscal_year)

# add never treated counties

temp = data.frame(
  county = c("Borden", "Kent", "Throckmorton", "Hemphill", "Roberts"),
  treatment_year = rep(NA, 5)
)

treatcounties = rbind(treatcounties, temp) %>% 
  mutate(prior_year = treatment_year-1)


##----------------------------------------------------------------
##                        POPULATION PANEL                       -
##----------------------------------------------------------------

treatcounties = left_join(treatcounties, countyfips, by = "county")

treatcounties = left_join(treatcounties, popdata, by = c("countyfips", "prior_year" = "year")) %>% 
  rename(prior_year_population = population) # save prior (treatment) year population

treatcounties = left_join(treatcounties, popyoungdata, by = c("countyfips", "prior_year" = "year")) %>% 
  rename(prior_year_populationyoung = population_young) # save prior (treatment) year population


panel = left_join(treatcounties, popdata, by = "countyfips")
panel = left_join(panel, popyoungdata, by = c("countyfips", "year"))

# rescale population data (divide by year prior to treatment year)

panel = panel %>% mutate(rescaled_pop = population/prior_year_population,
                         rescaled_pop_young = population_young/prior_year_populationyoung)


##----------------------------
##  PLOT RESCALED POPULATION  
##----------------------------

# All population

summary_population = panel %>% 
  mutate(group = year-treatment_year) %>% 
  filter(group %in% seq(-5,5)) %>% 
  group_by(group) %>% 
  summarise(
    mean_rescaled_pop = mean(rescaled_pop, na.rm = T),
    Lower = mean_rescaled_pop - 1.96*sd(rescaled_pop, na.rm=T)/sqrt(n()),
    Upper = mean_rescaled_pop + 1.96*sd(rescaled_pop, na.rm=T)/sqrt(n()),
  )

ggplot(summary_population) +
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "dimgrey", size = 1) +
  geom_errorbar(mapping = aes(x = group, ymin = Lower, ymax = Upper),
                width = 0.2, size =1, color = "orange") +
  geom_point(aes(x = group, y = mean_rescaled_pop), size = 3, shape = 21, fill = "dimgrey") +
  scale_x_continuous(breaks = seq(-5,5)) +
  labs(x = "Years since change of status",
       y = "Re-scaled population to year prior change of status") +
  theme_minimal()

ggsave("results/research-methods-presentation-2/population_whole.pdf", device = "pdf")

summary_population_young = panel %>% 
  mutate(group = year-treatment_year) %>% 
  filter(group %in% seq(-5,5)) %>% 
  group_by(group) %>% 
  summarise(
    mean_rescaled_pop = mean(rescaled_pop_young, na.rm = T),
    Lower = mean_rescaled_pop - 1.96*sd(rescaled_pop_young, na.rm=T)/sqrt(n()),
    Upper = mean_rescaled_pop + 1.96*sd(rescaled_pop_young, na.rm=T)/sqrt(n()),
  )

ggplot(summary_population_young) +
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "dimgrey", size = 1) +
  geom_errorbar(mapping = aes(x = group, ymin = Lower, ymax = Upper),
                width = 0.2, size =1, color = "orange") +
  geom_point(aes(x = group, y = mean_rescaled_pop), size = 3, shape = 21, fill = "dimgrey") +
  scale_x_continuous(breaks = seq(-5,5)) +
  labs(x = "Years since change of status",
       y = "Re-scaled population to year prior change of status") +
  theme_minimal()

ggsave("results/research-methods-presentation-2/population_young.pdf", device = "pdf")

##---------------------
##  DID (TWFE and CS)  
##---------------------

# Whole population results

# Create post-treatment indicator
df = panel %>% mutate(postTreated = !is.na(treatment_year) & year >= treatment_year)

twfe_results_pop = feols(rescaled_pop ~ postTreated | countyfips + year, df,
                         cluster = "countyfips")


cs_results_pop <- att_gt(
  yname = "rescaled_pop",
  tname = "year",
  idname = "countyfips",
  gname = "treatment_year",
  data = panel,
  control_group = "notyettreated"
)

cs_result_agg_pop = aggte(cs_results_pop, type = "simple")

# young population results

twfe_results_pop_young = feols(rescaled_pop_young ~ postTreated | countyfips + year, df,
                               cluster = "countyfips")


cs_results_pop_young <- att_gt(
  yname = "rescaled_pop_young",
  tname = "year",
  idname = "countyfips",
  gname = "treatment_year",
  data = panel,
  control_group = "notyettreated"
)

cs_result_agg_pop_young = aggte(cs_results_pop_young, type = "simple")


##---------------
##  Event Study  
##---------------

# whole population

es_pop <- aggte(cs_results_pop,
                type = "dynamic",
                min_e = -3, max_e = 3
)

es_summary_pop = data.frame(
  group = seq(-3, 3),
  att = es_pop$att.egt,
  se = es_pop$se.egt
) %>% 
  mutate(
    ymin = att - 1.96*se,
    ymax = att + 1.96*se
  )

ggplot(es_summary_pop) +
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "dimgrey", size = 1) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_errorbar(mapping = aes(x = group, ymin = ymin, ymax = ymax),
                width = 0.2, size =1, color = "orange") +
  geom_point(aes(x = group, y = att), size = 3, shape = 21, fill = "dimgrey") +
  scale_x_continuous(breaks = seq(-3,3)) +
  labs(x = "Years since change of status",
       y = "ATT") +
  theme_minimal()

ggsave("results/research-methods-presentation-2/population_es.pdf", device = "pdf")

ggdid(es_pop, title = ".")

# young population (20-34)

es_pop_young <- aggte(cs_results_pop_young,
                      type = "dynamic",
                      min_e = -3, max_e = 3
)

es_summary_pop_young = data.frame(
  group = seq(-3, 3),
  att = es_pop_young$att.egt,
  se = es_pop_young$se.egt
) %>% 
  mutate(
    ymin = att - 3*se,
    ymax = att + 3*se
  )

ggplot(es_summary_pop_young) +
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "dimgrey", size = 1) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_errorbar(mapping = aes(x = group, ymin = ymin, ymax = ymax),
                width = 0.2, size =1, color = "orange") +
  geom_point(aes(x = group, y = att), size = 3, shape = 21, fill = "dimgrey") +
  scale_x_continuous(breaks = seq(-3,3)) +
  labs(x = "Years since change of status",
       y = "ATT") +
  theme_minimal()

ggsave("results/research-methods-presentation-2/population_young_es.pdf", device = "pdf")

ggdid(es_pop_young, title = ".")



##---------------------------------------------------------------
##                        CRASHES PANEL                         -
##---------------------------------------------------------------


panel = left_join(panel, crashes, by = c("county", "year"))

panel = panel %>% mutate(crash_rate = annual_dui_crashes/population*1000)


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

ggsave("results/research-methods-presentation-2/crashes.pdf", device = "pdf")

##---------------------
##  DID (TWFE and CS)  
##---------------------

# Create post-treatment indicator
df = panel %>% mutate(postTreated = !is.na(treatment_year) & year >= treatment_year)

twfe_results_crash = feols(crash_rate ~ postTreated | countyfips + year, df,
                           cluster = "countyfips")

cs_results_crash <- att_gt(
  yname = "crash_rate",
  tname = "year",
  idname = "countyfips",
  gname = "treatment_year",
  data = panel,
  control_group = "notyettreated"
)

cs_result_agg_crash = aggte(cs_results_crash, type = "simple")

es_crash <- aggte(cs_results_crash,
                  type = "dynamic",
                  min_e = -3, max_e = 3
)

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

ggsave("results/research-methods-presentation-2/crashes-es.pdf", device = "pdf")

ggdid(es_crash, title = ".")




##------------------------
##  TREATED COUNTIES MAP  
##------------------------

# create list of all counties with dummy to highlight which ones I am focusing on

temp = left_join(countyfips, pass_election, by = "county") %>% 
  select(county, countyfips, result) %>% 
  mutate(selected = !(is.na(result))) %>% 
  select(!result)

map_treated_counties = left_join(county_shapefile, temp, by = "countyfips")

ggplot() +
  geom_sf(data = map_treated_counties,aes(fill = selected)) +
  scale_fill_manual(values = c("TRUE" = "orange", "FALSE" = "white")) +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        legend.position = "none")

ggsave("results/research-methods-presentation-2/map.pdf", device = "pdf")

##---------------------------------------------------------------
##              DISTRIBUTION COUNTYWIDE ELECTIONS               -
##---------------------------------------------------------------

summary_elections = elections %>% 
  filter(result=="passed") %>% 
  group_by(fiscal_year) %>% 
  summarise(
    total = n(),
    notcountywide = sum(!is.na(city) | !is.na(jp_precinct)),
    countywide = total - notcountywide
  ) %>% 
  select(!total) %>% 
  pivot_longer(cols = countywide:notcountywide,
               names_to = "group",
               values_to = "count") %>% 
  mutate(
    group = factor(group, levels = c("notcountywide","countywide"))
  )

ggplot(summary_elections, aes(x = fiscal_year, y = count, fill = group)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("countywide" = "orange", "notcountywide" = "gray90")) +
  theme_minimal() +
  labs(
    x = "",
    y = "Number of status changes"
  ) +
  theme(legend.position = "none")

ggsave("results/research-methods-presentation-2/N_elections.pdf", device = "pdf")


##---------------------------------------------------------------
##                ALLUVIAL PLOT FOR STATUS CHANGE               -
##---------------------------------------------------------------

status_1996 = read.csv("data/clean/county_status_1996.csv")
status_2020 = read.csv("data/clean/county_status_2020.csv")

merged_status = full_join(status_1996, status_2020, by = "county") %>% 
  mutate(
    status_1996.1 = ifelse(status_1996.1 == "", "Dry", status_1996.1),
    status_2020.1 = ifelse(status_2020.1 == "", "Dry", status_2020.1)
  )

merged_status <- merged_status %>%
  mutate(
    status_1996 = ifelse(status_1996.1=="Dry in part", "Dry in part", status_1996),
    status_2020 = ifelse(status_2020.1=="Dry in part", "Dry in part", status_2020),
    status_1996 = factor(status_1996,
                         levels = c("Dry",
                                    "Partially wet",
                                    "Dry in part",
                                    "Wet")),
    status_2020 = factor(status_2020,
                         levels = c("Dry",
                                    "Partially wet",
                                    "Dry in part",
                                    "Wet")),
    )

alluvial_data=merged_status %>% 
  group_by(status_1996, status_2020) %>%
  summarise(Count = n())

ggplot(alluvial_data, aes(axis1 = status_1996, axis2 = status_2020, y = Count)) +
  geom_alluvium(aes(fill = status_1996), curve_type = "cubic") +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("1997", "2020"), expand = c(0, 0)) +
  scale_fill_manual(values = c("Dry" = "darkorange4", 
                               "Partially wet" = "orange",
                               "Dry in part" = "gray70",
                               "Wet" = "dimgrey")) +
  theme_minimal() +
  labs(title = "",
       x = "",
       y = "") +
  guides(fill = F)

ggsave("results/research-methods-presentation-2/alluvial.pdf", device = "pdf")


##---------------------------------------------------------------
##                      NUMBER OF ELECTIONS                     -
##---------------------------------------------------------------

# Simple bar plot for number of elections over the years

summary_elections_overall = elections %>% 
  filter(!is.na(result)) %>% 
  group_by(fiscal_year, result) %>% 
  summarise(
    total = n()
    )%>% 
  mutate(
    group = factor(group, levels = c("passed","failed"))
  )

ggplot(summary_elections_overall, aes(x = fiscal_year, y = total, fill = result)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("passed" = "orange", "failed" = "gray90")) +
  theme_minimal() +
  labs(
    x = "",
    y = "Number ofelections"
  ) +
  theme(legend.position = "bottom")

ggsave("results/research-methods-presentation-2/N_elections_overall.pdf", device = "pdf")


##----------------------------------------------------------------
##                      CLOSE ELECTION CHECK                     -
##----------------------------------------------------------------


rd_check = elections %>% 
  filter(!is.na(result)) %>% 
  mutate(total_votes = for_vote+against_vote,
         for_share = for_vote/total_votes,
         close1 = ifelse(for_share >= .40 & for_share<= .60, 1, 0),
         close2 = ifelse(for_share >= .45 & for_share<= .55, 1, 0),
         close3 = ifelse(for_share >= .48 & for_share<= .52, 1, 0),
         )

ggplot(rd_check, aes(x=for_share)) + 
  geom_histogram(fill = "orange", color = "dimgrey") +
  geom_vline(xintercept = .45, linetype = "dashed", color = "black", size = 1) +
  geom_vline(xintercept = .55, linetype = "dashed", color = "black", size = 1) +
  labs(x= "Share of FOR votes", y = "Number of elections") +
  theme_minimal()

ggsave("results/research-methods-presentation-2/rd_check.pdf", device = "pdf")
