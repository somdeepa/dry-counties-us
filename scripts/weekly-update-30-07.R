
##-------------------------------------------------------------------------------------
##                             Odd vs even year elections                             -
##                        (follow up from last weekly update)                         -
##  Observation: Election years have higher turnouts compared to non election years   -
##-------------------------------------------------------------------------------------

data = read.csv("data/clean/merged/election_level_with_turnout_and_population.csv") %>% 
  mutate(
    year_type = ifelse(election_year%%2==0, "Even Year", "Odd year")
  )

temp = data %>%
  group_by(year_type) %>%
  summarise(
    total = n(),
    passed = sum(result == "passed"),
    average_turnout = mean(actual_turnout, na.rm = T),
    avergae_turnout_sd = sd(actual_turnout, na.rm = T)/sqrt(n())*1.96,
    average_for_share = mean(for_vote_share, na.rm =T),
    average_for_share_sd = sd(for_vote_share, na.rm = T)/sqrt(n())*1.96
  ) %>%
  mutate(win_rate = passed / total)

table = table(data$issue.short, data$year_type)

if(any(result$expected<5)){
  fisher.test(table)
}


##----------------------------------------------------------------
##                  Alcohol licenses exploration                 -
##----------------------------------------------------------------



total_businesses_per_year = read.csv("data/clean/liquor-licenses/total_businesses_by_year_1990_2019.csv")
# Aggregate total number of businesses per year for each category across all counties
businesses_per_category = read.csv("data/clean/liquor-licenses/total_businesses_by_year_category_detail_1990_2019.csv")
# Add a column to distinguish the total businesses line from category lines
total_businesses_per_year$category_detail = "Total"
businesses_per_category$category_detail = as.character(businesses_per_category$category_detail)


# Combine the data frames
combined_data = bind_rows(total_businesses_per_year, businesses_per_category)
ggplot(combined_data, aes(x = year, y = total_businesses, color = category_detail, group = category_detail)) +
  geom_line(size =0.8) +
  geom_point(aes(shape = category_detail), size = 1.5) +
  labs(
    x = "",
    y = "",
    title = "Total # of Alcohol Licenses in Texas, 1990-2019"
  ) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

ggsave("results/weekly updates/13-08/licenses_times_series)categories.png", device = "png")



total_businesses_per_year = read.csv("data/clean/liquor-licenses/total_businesses_by_year_1990_2019.csv")
# Aggregate total number of businesses per year for each category across all counties
businesses_per_category = read.csv("data/clean/liquor-licenses/total_businesses_by_year_category_1990_2019.csv")
# Add a column to distinguish the total businesses line from category lines
total_businesses_per_year$category = "Total"
businesses_per_category$category = as.character(businesses_per_category$category)


# Combine the data frames
combined_data = bind_rows(total_businesses_per_year, businesses_per_category)
ggplot(combined_data, aes(x = year, y = total_businesses, color = category, group = category)) +
  geom_line(size =0.8) +
  geom_point(aes(shape = category), size = 1.5) +
  labs(
    x = "",
    y = "",
    title = "Total # of Retail Alcohol Licenses in Texas, 1990-2019"
  ) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

ggsave("results/weekly updates/13-08/licenses_over_time_retail_only.png", device = "png")

