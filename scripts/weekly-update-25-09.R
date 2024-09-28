
#################################################################
##               ON_PREMISE ALCOHOL SALES GRAPHS               ##
#################################################################

# yearly sales (per capita)

quaterly_receipts = read.csv("data/clean/liquor-taxes/quarterly_receipts.csv") %>% 
  group_by(year, type) %>% 
  summarise(
    total_receipts = sum(total_receipts)
  )

texas_population = read.csv("data/clean/population/texas_population_by_county_1969_2022_drinking_age.csv") %>% 
  group_by(year) %>% 
  summarise(population = sum(population))

combined_data = left_join(quaterly_receipts, texas_population, by = "year") %>% 
  mutate(receipts_capita = total_receipts/population,
         type = factor(type,
                       levels = c("total", "liquor", "beer_wine")))

ggplot(combined_data, aes(x = year, y = receipts_capita, color = type, group = type)) +
  geom_line(size =0.8) +
  labs(
    x = "",
    y = "$",
    title = "Annual on-premise per capita alochol sales"
  ) +
  scale_color_paletteer_d("ggthemr::greyscale") +
  scale_x_continuous(breaks = seq(1995, 2020, 5)) +
  #scale_color_paletteer_d("ggthemr::greyscale") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

ggsave("results/weekly updates/25-9/annual_sales.png", device = "png")

## treated only

data = read.csv("data/clean/merged/annual_city_receipts.csv")

temp = data %>% 
  group_by(type, year) %>% 
  summarise(
    population = sum(population, na.rm = T),
    total_receipts = sum(total_receipts, na.rm = T)
  ) %>% 
  mutate(
    receipts_capita = total_receipts/population,
    type = factor(type,
                  levels = c("total", "liquor", "beer_wine"))
  )

ggplot(temp, aes(x = year, y = receipts_capita, color = type, group = type)) +
  geom_line(size =0.8) +
  labs(
    x = "",
    y = "$",
    title = "Annual on-premise per capita alochol sales, treated cities"
  ) +
  scale_color_paletteer_d("ggthemr::greyscale") +
  scale_x_continuous(breaks = seq(1995, 2020, 5)) +
  #scale_color_paletteer_d("ggthemr::greyscale") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

ggsave("results/weekly updates/25-9/annual_sales_treated_only.png", device = "png")

##---------------------------------------------------------------
##                    LIQUOR SALES RAW DATA                     -
##---------------------------------------------------------------

data = read.csv("data/clean/merged/quarterly_city_receipts.csv")


min = -10
max = 10
summarytable = data %>% 
  filter(periods_from_treatment %in% seq(min, max)) %>% 
  group_by(periods_from_treatment, type) %>% 
  summarise(
    mean_receipts_capita = mean(receipts_capita, na.rm = T),
    Lower = mean_receipts_capita - 1.96*sd(receipts_capita, na.rm=T)/sqrt(n()),
    Upper = mean_receipts_capita + 1.96*sd(receipts_capita, na.rm=T)/sqrt(n()),
  ) %>% 
  mutate(
    type = factor(type,
                  levels = c("total", "liquor", "beer_wine"))
  )

ggplot(summarytable %>% filter(type == "total")) +
  geom_errorbar(mapping = aes(x = periods_from_treatment, ymin = Lower, ymax = Upper),
                width = 0.2, linewidth =0.5) +
  #geom_vline(xintercept = 0, linetype = "dashed") +
  geom_line(aes(x = periods_from_treatment, y = mean_receipts_capita))+
  geom_point(aes(x = periods_from_treatment, y = mean_receipts_capita), size = 1.5, shape = 21, fill = "black") +
  scale_x_continuous(breaks = seq(min, max, 2)) +
  labs(x = "Years",
       y = "On-premise alcohol sales per capita") +
  theme_bw()

ggsave("results/weekly updates/25-9/event_study_total_receipts_raw.png", device = "png")


##---------------------------------------------------------------
##            EVENT STUDY: ON PREMISE ALCOHOL SALES             -
##---------------------------------------------------------------

### TOTAL RECEIPTS

# simple TWFE : Effect of treatment on alochol receipts per capita

model.twfe.total = feols(receipts_capita ~ treatment_quarter|id +year,
                       data = data %>% filter(type== "total"), cluster = "id")

print(model.twfe.total)


## Simple event study



# Event Study-Calloway Sant'Anna

model.cs = att_gt(yname = "receipts_capita",
                  gname = "first_policy_change_quarter",
                  idname = "id",
                  tname = "quarter",
                  xformla = ~1,
                  data = data %>% filter(type== "total"),
                  control_group = "notyettreated",
                  base_period = "universal"
)

agg_cs_res = aggte(model.cs, type = "dynamic",min_e = -20, max_e = 20, na.rm = T)

agg_cs_res_calendar = aggte(model.cs, type = "calendar", na.rm = T)

ggdid(agg_cs_res_calendar, xgap = 2, ylab = "Estimate and 95% interval", xlab = "Quarters since change of status",
      theming = F) +
  geom_errorbar(color = "black", width = 0.2) +
  geom_point(color = "black") +
  labs(title = "") +
  theme_bw() +
  theme(
    legend.position = "none"
  )

ggsave("results/weekly updates/25-9/event_study_total_receipts.png", device = "png")


##---------------------------------------------------------------
##                          RD GRAPHS                           -
##---------------------------------------------------------------

data = read.csv("data/clean/merged/RDpanel_receipts_elections_city.csv")

time_periods = c(-1,1,5,10)

bin = 1
temp = data %>% 
  filter(
    periods_from_election %in% time_periods
  ) %>% 
  #(!city %in% c("Winona", "Coffee City") ) %>% 
  #filter(licensepop<5) %>% 
  #filter(for_vote_share<.92) %>% 
  # binning vote shares
  mutate(for_vote_share = floor(for_vote_share*100/bin)*bin) %>% 
  #filter(for_vote_share==31)
  #filter(between(for_vote_share, 20, 80)) %>% 
  group_by(for_vote_share, periods_from_election) %>% 
  summarise(
    n = n(),
    mean_receipts_capita = mean(receipts_capita, na.rm = T),
    Lower = mean_receipts_capita - 1.96*sd(receipts_capita, na.rm=T)/sqrt(n()),
    Upper = mean_receipts_capita + 1.96*sd(receipts_capita, na.rm=T)/sqrt(n()),
  )




ggplot(temp) +
  #geom_vline(xintercept = 50, linetype = "dashed", color = "red", linewidth = 0.5) +
  geom_hline(yintercept = 0) +
  #geom_errorbar(mapping = aes(x = for_vote_share, ymin = Lower, ymax = Upper),
  #             width = 0.3, linewidth =0.5) +
  geom_point(aes(x = for_vote_share, y = mean_receipts_capita), size = 1, shape = 21, fill = "grey") +
  geom_smooth(aes(x = for_vote_share, y = mean_receipts_capita),
              method = "lm", formula = y ~ x,
              data = subset(temp, for_vote_share < 50),
              color = "black", se = T) +
  geom_smooth(aes(x = for_vote_share, y = mean_receipts_capita),
              method = "lm", formula = y ~ x,
              data = subset(temp, for_vote_share >= 50),
              color = "black", se = T) +
  labs(x = "For vote percentage",
       y = "On-premise alcohol sales per capita") +
  facet_wrap(~periods_from_election, ncol = 2,
             labeller = labeller(periods_from_election = function(x) {
               paste("Years from election =", x)
             })) +
  theme_bw(base_size = 16)

ggsave("results/weekly updates/25-9/rd_style_full_receipts.png", device = "png")


##----------------------------------------------------------------
##                EVENT STUDY: LICENSES, CITY-YEAR               -
##----------------------------------------------------------------

# On-premise

data = read.csv("data/clean/merged/licenses-elections/on_premise_licenses_city_quarter.csv") %>% 
  group_by(city, year) %>% 
  summarise(
    id = unique(id),
    total_businesses = sum(total_businesses),
    population = unique(population),
    first_policy_change = unique(first_policy_change)
  ) %>% 
  mutate(
    licensepop = total_businesses/population*1000,
    treatment_year = ifelse(year >= first_policy_change, 1, 0),
    periods_from_treatment = year - first_policy_change
  )

model.twfe.on = feols(licensepop ~ treatment_year|id +year,
                       data = data, cluster = "id")

print(model.twfe.on)

# Event Study-Calloway Sant'Anna

model.cs = att_gt(yname = "licensepop",
                  gname = "first_policy_change",
                  idname = "id",
                  tname = "year",
                  xformla = ~1,
                  data = data,
                  control_group = "notyettreated",
                  base_period = "universal"
)

agg_cs_res = aggte(model.cs, type = "dynamic",min_e = -10, max_e = 10, na.rm = T)

ggdid(agg_cs_res, xgap = 4, ylab = "Estimate and 95% interval", xlab = "Years since change of status",
      theming = F) +
  geom_errorbar(color = "black", width = 0.2) +
  geom_point(color = "black") +
  labs(title = "") +
  theme_bw() +
  theme(
    legend.position = "none"
  )  

ggsave("results/weekly updates/25-9/event-study-licenses-annual-on-premise.png", device = "png")

# Off-premise

data = read.csv("data/clean/merged/licenses-elections/off_premise_licenses_city_quarter.csv") %>% 
  group_by(city, year) %>% 
  summarise(
    id = unique(id),
    total_businesses = sum(total_businesses),
    population = unique(population),
    first_policy_change = unique(first_policy_change)
  ) %>% 
  mutate(
    licensepop = total_businesses/population*1000,
    treatment_year = ifelse(year >= first_policy_change, 1, 0),
    periods_from_treatment = year - first_policy_change
  )

model.twfe.off = feols(licensepop ~ treatment_year|id +year,
                      data = data, cluster = "id")

print(model.twfe.off)

# Event Study-Calloway Sant'Anna

model.cs = att_gt(yname = "licensepop",
                  gname = "first_policy_change",
                  idname = "id",
                  tname = "year",
                  xformla = ~1,
                  data = data,
                  control_group = "notyettreated",
                  base_period = "universal"
)

agg_cs_res = aggte(model.cs, type = "dynamic",min_e = -10, max_e = 10, na.rm = T)

ggdid(agg_cs_res, xgap = 4, ylab = "Estimate and 95% interval", xlab = "Years since change of status",
      theming = F) +
  geom_errorbar(color = "black", width = 0.2) +
  geom_point(color = "black") +
  labs(title = "") +
  theme_bw() +
  theme(
    legend.position = "none"
  )  

ggsave("results/weekly updates/25-9/event-study-licenses-annual-off-premise.png", device = "png")
