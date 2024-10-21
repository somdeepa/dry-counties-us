# All Tables and Graphs for Area Exam


##---------------------------------------------------------------
##                    All licenses, quarterly                   -
##---------------------------------------------------------------

data = read.csv("data/clean/merged/licenses-elections/all_licenses_city_quarter.csv") 

# simple TWFE : Effect of treatment on licenses/population*1000

model.twfe.all = feols(licensepop ~ treatment_quarter|id +quarter,
                     data = data, cluster = "id")

# calculate pre-treatment mean
my0.all = mean(data$licensepop[data$treatment_quarter == 0], na.rm = T)

print(model.twfe.all)

# Event Study-Calloway Sant'Anna

model.cs = att_gt(yname = "licensepop",
                gname = "first_policy_change_quarter",
                idname = "id",
                tname = "quarter",
                xformla = ~1,
                data = data,
                control_group = "notyettreated",
                base_period = "universal"
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

ggsave("results/area-exam/event-study-all-licenses-quaterly.png", device = "png")

# plot raw data

summarytable = data %>% 
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

ggsave("results/area-exam/license_event_study_raw_data_all_licenses.png", device = "png")

##---------------------------------------------------------------
##                    On-premise, Quarterly                     -
##---------------------------------------------------------------

data = read.csv("data/clean/merged/licenses-elections/on_premise_licenses_city_quarter.csv") 

# simple TWFE : Effect of treatment on licenses/population*1000

model.twfe.on = feols(licensepop ~ treatment_quarter|id +quarter,
                     data = data, cluster = "id")

# calculate pre-treatment mean
my0.on = mean(data$licensepop[data$treatment_quarter == 0], na.rm = T)

print(model.twfe.on)

# Event Study-Calloway Sant'Anna

model.cs = att_gt(yname = "licensepop",
                  gname = "first_policy_change_quarter",
                  idname = "id",
                  tname = "quarter",
                  xformla = ~1,
                  data = data,
                  control_group = "notyettreated",
                  base_period = "universal"
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

ggsave("results/area-exam/event-study-on-premise-licenses-quaterly.png", device = "png")

summarytable = data %>% 
  filter(periods_from_treatment %in% seq(-20,20)) %>% 
  group_by(periods_from_treatment) %>% 
  summarise(
    mean_licensepop = mean(licensepop, na.rm = T),
    Lower = mean_licensepop - 1.96*sd(licensepop, na.rm=T)/sqrt(n()),
    Upper = mean_licensepop + 1.96*sd(licensepop, na.rm=T)/sqrt(n()),
  )

ggplot(summarytable) +
  
  geom_line(aes(x = periods_from_treatment, y = mean_licensepop))+
  geom_errorbar(mapping = aes(x = periods_from_treatment, ymin = Lower, ymax = Upper),
                width = 0.3, linewidth =0.5) +
  geom_point(aes(x = periods_from_treatment, y = mean_licensepop), size = 2, shape = 21, fill = "black") +
  scale_x_continuous(breaks = seq(-20,20, 4)) +
  labs(x = "Quarters",
       y = "On-Premise Licenses per 1000 population") +
  theme_minimal()

ggsave("results/area-exam/license_event_study_raw_data_on_premise_quaterly.png", device = "png")

##---------------------------------------------------------------
##                    Off-premise, Quarterly                    -
##---------------------------------------------------------------

data = read.csv("data/clean/merged/licenses-elections/off_premise_licenses_city_quarter.csv") 

# simple TWFE : Effect of treatment on licenses/population*1000

model.twfe.off = feols(licensepop ~ treatment_quarter|id +quarter,
                     data = data, cluster = "id")

# calculate pre-treatment mean
my0.off = mean(data$licensepop[data$treatment_quarter == 0], na.rm = T)

print(model.twfe.off)

# Event Study-Calloway Sant'Anna

model.cs = att_gt(yname = "licensepop",
                  gname = "first_policy_change_quarter",
                  idname = "id",
                  tname = "quarter",
                  xformla = ~1,
                  data = data,
                  control_group = "notyettreated",
                  base_period = "universal"
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

ggsave("results/area-exam/event-study-off-premise-licenses-quaterly.png", device = "png")

# plot raw data

summarytable = data %>% 
  filter(periods_from_treatment %in% seq(-20,20)) %>% 
  group_by(periods_from_treatment) %>% 
  summarise(
    mean_licensepop = mean(licensepop, na.rm = T),
    Lower = mean_licensepop - 1.96*sd(licensepop, na.rm=T)/sqrt(n()),
    Upper = mean_licensepop + 1.96*sd(licensepop, na.rm=T)/sqrt(n()),
  )

ggplot(summarytable) +
  
  geom_line(aes(x = periods_from_treatment, y = mean_licensepop))+
  geom_errorbar(mapping = aes(x = periods_from_treatment, ymin = Lower, ymax = Upper),
                width = 0.3, linewidth =0.5) +
  geom_point(aes(x = periods_from_treatment, y = mean_licensepop), size = 2, shape = 21, fill = "black") +
  scale_x_continuous(breaks = seq(-20,20, 4)) +
  labs(x = "Quarters",
       y = "Off-Premise Licenses per 1000 population") +
  theme_minimal()

ggsave("results/area-exam/license_event_study_raw_data_off_premise_quaterly.png", device = "png")

##----------------------------------------------------------------
##                  First Stage regression table                 -
##----------------------------------------------------------------

models = list(All = model.twfe.all, `On-Premise` = model.twfe.on, `Off-Premise` = model.twfe.off)

# create row for pre-treatment means
pre_treatment_means <- data.frame(matrix(c(
  "Pre-treatment mean", 
  round(my0.all, 3), 
  round(my0.on, 3), 
  round(my0.off, 3)
), nrow = 1))

attr(pre_treatment_means, 'position') = 3

modelsummary(models,
             title = "Status changes and Alcohol Licenses in Texas, 1990-2019",
             coef_rename = c("treatment_quarter" = "Status Change==1"),
             gof_omit = 'R2 Adj.|R2 Within|R2 Within Adj.|AIC|BIC|RMSE|Std',
             add_rows = pre_treatment_means,
             gof_map = list(
               list("raw" = "nobs", "clean" = "N", "fmt" = 0),
               list("raw" = "r.squared", "clean" = "$R^2$", "fmt" = 2),
               list("raw" = "FE: id", "clean" = "City fixed effects", fmt = 0),
               list("raw" = "FE: quarter", "clean" = "Time fixed effects", fmt = 0)),
             "modelsummary_format_numeric_latex" = "plain",
             align = "lccc",
             output = "results/area-exam/FirstStageTable.docx",
             notes = "Each column represents results from a separate OLS regression with city and quarter fixed effects. The dependent variable is equal to the number of active licenses per 1000 population in city $i$ at quarter $t$. Standard errros are clustered at the city level.")
