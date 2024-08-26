# Area exam non-regression graphs


##---------------------------------------------------------------
##                Elections and election issues                 -
##---------------------------------------------------------------


electiondata = read.csv("data/clean/elections-data/elections-data-97-2020-cleaned.csv") %>% 
  mutate(
    for_share = for_vote/(for_vote + against_vote),
    id = case_when(
      !is.na(city) ~ str_c(county1,city),
      !is.na(precinct) & is.na(city) ~ str_c(county1,precinct),
      is.na(precinct) & is.na(city) ~ str_c(county1)
    )
  ) %>% 
  group_by(id, fiscal_year) %>% 
  mutate(isUniqueElection = ifelse(result == "passed" & row_number() == n(), "Unique", NA)) %>% 
  ungroup() %>% 
  mutate(
    Geography = case_when(
      !is.na(isUniqueElection) & !is.na(city) ~ "City-level",
      !is.na(isUniqueElection) & !is.na(precinct) & is.na(city) ~ "Precinct-level",
      !is.na(isUniqueElection) & is.na(precinct) & is.na(city) ~ "County-level",
      TRUE ~ NA_character_
    ),
    type_change = case_when(
      !is.na(isUniqueElection) & result == "passed" & status_before == "dry" & status_current == "wet" ~ "Dry to wet",
      !is.na(isUniqueElection) & result == "passed" & status_before == "wet" & status_current == "wet" ~ "Wet to more wet",
      !is.na(isUniqueElection) & result == "passed" & status_before == "wet" & status_current == "dry" ~ "Wet to dry",
      TRUE ~ NA_character_
    )
  )


# status changes summary

datasummary(
  Heading("Successful elections")*(result == "passed") +
    Heading("Unique Status changes")*(isUniqueElection == "Unique") +
              (Geography*Heading("Type of Change")*type_change) ~ 
              N * DropEmpty(), 
    data = electiondata,
    align = "llllr",
    output = "results/area-exam/StatusChanges.tex",
    title = "Summary of Unique Status Changes in Texas jurisdictions, 1997-2019",
    notes = "Many jurisdictions vote on multiple issues at the same time – for example “for the sale of beer and wine for off-premises consumption” and “for the sale of mixed beverages in restaurants with food and beverage certificates. I treat multiple elections in a jurisdiction on the same date as a single status change. This is referred to as a unique status change.
"
    )

datasummary(
  Heading("Election issue")*issue.consolidated ~ 1 + result,
  fmt = 0,
  data = electiondata,
  align = "lrrr",
  output = "results/area-exam/ElectionIssues.tex",
  title = "Election issues in Local Options Elections, 1997-2019",
  notes = "There are 10 prohibitory elections in this period, which are excluded from the above table. Many jurisdictions vote on multiple issues at the same time. All alcohol includes sales of beer, wine and distilled spirits. Mixed beverages refer to mixed-drink sales in restaurants and bars."
)


##---------------------------------------------------------------
##                  elections over time (graph)                 -
##---------------------------------------------------------------

summary_elections_overall = electiondata %>% 
  filter(!is.na(result)) %>% 
  group_by(fiscal_year, result) %>% 
  summarise(
    total = n()
  )%>% 
  mutate(
    group = factor(result, levels = c("passed","failed"))
  )

ggplot(summary_elections_overall, aes(x = fiscal_year, y = total, fill = result)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("passed" = "gray20", "failed" = "gray90")) +
  theme_minimal() +
  labs(
    x = "",
    y = "Number ofelections"
  ) +
  theme(legend.position = "bottom")

ggsave("results/area-exam/N_elections_overall.png", device = "png")


##----------------------------------------------------------------
##                status changes over time (graph)               -
##----------------------------------------------------------------

temp = electiondata %>% filter(result == "passed") %>% 
  group_by(fiscal_year) %>% 
  summarise(total = n_distinct(id))

ggplot(temp, aes(x = fiscal_year, y = total)) +
  geom_bar(stat = "identity", fill = "gray20") +
  theme_minimal() +
  labs(
    x = "",
    y = "Number of status changes"
  ) +
  theme(legend.position = "bottom")

ggsave("results/area-exam/status_changes.pdf", device = "pdf")


##----------------------------------------------------------------
##                        Alcohol Licenses                       -
##----------------------------------------------------------------

total_businesses_per_year = read.csv("data/clean/liquor-licenses/total_businesses_by_year_1990_2019.csv")
# Aggregate total number of businesses per year for each category across all counties
businesses_per_category = read.csv("data/clean/liquor-licenses/total_businesses_by_year_category_detail_1990_2019.csv")
# Add a column to distinguish the total businesses line from category lines
total_businesses_per_year$category_detail = "Total"
businesses_per_category$category_detail = as.character(businesses_per_category$category_detail)

# per capita version

# unit: 1000s of persons
texas_population = read.csv("data/clean/population/texas_population_by_county_1969_2022.csv") %>% 
  group_by(year) %>% 
  summarise(population = sum(population))


combined_data = bind_rows(total_businesses_per_year, businesses_per_category) %>% 
  left_join(texas_population, by = "year") %>% 
  mutate(licenses_rate = total_businesses/population*1000)

ggplot(combined_data, aes(x = year, y = licenses_rate, color = category_detail, group = category_detail)) +
  geom_line(size =0.8) +
  geom_point(aes(shape = category_detail), size = 1.5) +
  labs(
    x = "",
    y = "Liquor Licenses per 1000 population"
  ) +
  scale_color_paletteer_d("ggthemr::greyscale") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )
  

ggsave("results/area-exam/licenses_times_series_per_capita_categories.png", device = "png")

# OFF/ON

total_businesses_per_year = read.csv("data/clean/liquor-licenses/total_businesses_by_year_1990_2019.csv")
# Aggregate total number of businesses per year for each category across all counties
businesses_per_category = read.csv("data/clean/liquor-licenses/total_businesses_by_year_category_1990_2019.csv")
# Add a column to distinguish the total businesses line from category lines
total_businesses_per_year$category = "Total"
businesses_per_category$category = as.character(businesses_per_category$category)


combined_data = bind_rows(total_businesses_per_year, businesses_per_category) %>% 
  left_join(texas_population, by = "year") %>% 
  mutate(licenses_rate = total_businesses/population*1000)

ggplot(combined_data, aes(x = year, y = licenses_rate, color = category, group = category)) +
  geom_line(size =0.8) +
  geom_point(aes(shape = category), size = 1.5) +
  labs(
    x = "",
    y = "Retail Liquor Licenses per 1000 population",
    title = ""
  ) +
  scale_color_paletteer_d("ggthemr::greyscale") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

ggsave("results/area-exam/licenses_over_time_retail_only_per_capita.png", device = "png")

