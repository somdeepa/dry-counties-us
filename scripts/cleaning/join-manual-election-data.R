# combine elections data (dta from Tong (2011 onwards) and my Excel file (1997-2010), )

# combine elections data (dta from Tong and my Excel file)

data2 = read_dta("data/clean/elections-data/manual/FINAL/elections2011-2020.dta") %>% 
  select(c(1:4, 6:14)) %>% 
  mutate(
    across(everything(), ~ifelse(.== ".", NA, .)),
    election_date = as.Date(election_date)
  )

# clean up one voting figure because it is improbable
data2[data2$city == "Haslet" & data2$for_vote == 3337, 'for_vote'] = 337

data1 = read_excel("data/clean/elections-data/manual/FINAL/local-option-elections-97-2020-manual.xlsx", sheet = "elections") %>% 
  select(1:13) %>% 
  filter(fiscal_year <= 2010) %>% 
  rename(precinct = jp_precinct) %>% 
  mutate(
    across(everything(), ~ifelse(.== "NA", NA, .)),
    for_vote = as.numeric(for_vote),
    against_vote = as.numeric(against_vote),
    election_date = mdy(election_date),
    city = str_replace(city, "City of ", "")
  )


# Make joined dataset

joined_data = bind_rows(data1, data2)

# separate counties column 
joined_data = joined_data %>% 
  mutate(
    county = str_replace_all(county, " Counties", ""),
    county = str_replace_all(county, " and |,| & ", ", ")
  ) %>% 
  separate(county, into = str_c("county", seq(1,3)), sep = ", ", fill = "right",
           extra = "merge") %>% 
  mutate(across(starts_with("county"), ~ str_trim(str_remove_all(.x, ","))))


# clean some county names
joined_data$county1 = str_replace(joined_data$county1, "Sabube", "Sabine")
joined_data$county2 = str_replace(joined_data$county2, "Woods", "Wood")

# Three pairs of municipalities share the same name: Lakeside, Oak Ridge, and Reno
# will modify city name to reflect this

duplicate_cities = c("Lakeside", "Oak Ridge", "Reno")

joined_data = joined_data %>%
  mutate(
    city = ifelse(city %in% duplicate_cities, str_c(city, " (", county1, ")"), city),
    # lakeside in Archer county is called Lakeside City
    city = ifelse(city == "Lakeside (Archer)", "Lakeside City", city)
  )
  
# clean issues

issues = read.csv("data/clean/elections-data/issues.csv")

temp = data.frame(
  issue.clean = c(
    "legal sale of beer and wine for off-premises consumption only",
    "legal sale of mixed beverages in restaurants by food and beverage certificate holders only",
    "legal sale of all alcoholic beverages including mixed beverages",
    "legal sale of all alcoholic beverages for off-premises consumption only",
    "legal sale of beer and wine",
    "legal sale of mixed beverages.",
    "legal sale of all alcoholic beverages except mixed beverages",
    "legal sale of beer for off-premises consumption only",
    "legal sale of wine for off-premises consumption only",
    "legal sale of beer",
    "legal sale of wine by a holder of a winery permit"
  ),
# Short names for the issues
issue.short = c(
  "Beer & Wine Off-Premises",
  "Mixed Beverages in Restaurants",
  "All Alcoholic Beverages",
  "All Alcohol Off-Premise",
  "Beer & Wine",
  "Mixed Beverages",
  "All Alcohol except Mixed",
  "Beer Off-Premises",
  "Wine Off-Premises",
  "Beer",
  "Wine by Winery Permit"
)
)

issues = left_join(issues, temp, by = "issue.clean")

joined_data = left_join(joined_data, issues, by = "issue") %>% 
  mutate(
    # consolidate issue categories
    issue.consolidated = case_when(
      issue.short %in% c("Beer & Wine Off-Premises", "Beer Off-Premises", "Wine Off-Premises") ~ "Beer/Wine Off-Premise",
      issue.short %in% c("Beer & Wine", "Beer", "Wine by Winery Permit") ~ "Beer/Wine On-Premise",
      issue.short %in% c("All Alcoholic Beverages Off-Premises", "All Alcohol except Mixed") ~ "All alcohol Off-Premise",
      issue.short %in% c("All Alcoholic Beverages") ~ "All alcohol On-Premise",
      issue.short %in% c("Mixed Beverages", "Mixed Beverages in Restaurants") ~ "Mixed Beverages On-Premise",
      TRUE ~ issue.short
    )
  )

write.csv(joined_data, "data/clean/elections-data/elections-data-97-2020-cleaned.csv", row.names = F)
