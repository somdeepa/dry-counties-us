
##----------------------------------------------------------------
##            LICENSE TRENDS FOR TREATED COUNTIES ONLY           -
##----------------------------------------------------------------

## BY CATEGORY

total_businesses_per_year = read.csv("data/clean/liquor-licenses/total_businesses_by_year_treated_only_1990_2019.csv")
# Aggregate total number of businesses per year for each category across all counties
businesses_per_category = read.csv("data/clean/liquor-licenses/total_businesses_by_year_treated_only_category_detail_1990_2019.csv")
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
    title = "Total # of Alcohol Licenses in Treated Cities, 1990-2019"
  ) +
  scale_color_paletteer_d("ggthemr::greyscale") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

ggsave("results/weekly updates/4-9/licenses_over_time_treated_only.png", device = "png")

## BY OFF/ON

total_businesses_per_year = read.csv("data/clean/liquor-licenses/total_businesses_by_year_treated_only_1990_2019.csv")
# Aggregate total number of businesses per year for each category across all counties
businesses_per_category = read.csv("data/clean/liquor-licenses/total_businesses_by_year_category_treated_only_1990_2019.csv")
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
    title = "Total # of Retail Alcohol Licenses in Treated Cities, 1990-2019"
  ) +
  theme_bw() +
  scale_color_paletteer_d("ggthemr::greyscale") +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )
ggsave("results/weekly updates/4-9/licenses_over_time_retail_only_treated_only.png", device = "png")


##----------------------------------------------------------------
##                        R-D style graphs                       -
##----------------------------------------------------------------

data = read.csv("data/clean/merged/licenses-elections/RDpanel_quaterly_city.csv")

time_periods = seq(-1, 1)

temp = data %>% 
  filter(
    periods_from_treatment %in% time_periods
  ) %>% 
  # rounding down vote shares
  mutate(for_vote_share = floor(for_vote_share*100)) %>% 
  group_by(for_vote_share, periods_from_treatment) %>% 
    summarise(
      n = n(),
      mean_licensepop = mean(licensepop, na.rm = T),
      Lower = mean_licensepop - 1.96*sd(licensepop, na.rm=T)/sqrt(n()),
      Upper = mean_licensepop + 1.96*sd(licensepop, na.rm=T)/sqrt(n()),
    )



ggplot(temp) +
  #geom_line(aes(x = for_vote_share, y = mean_licensepop))+
  #geom_errorbar(mapping = aes(x = for_vote_share, ymin = Lower, ymax = Upper),
                width = 0.3, linewidth =0.5) +
  geom_point(aes(x = for_vote_share, y = mean_licensepop), size = 2, shape = 21, fill = "black") +
  #scale_y_continuous(breaks = seq(0,2, 0.2)) +
  #scale_x_continuous(breaks = seq(20,100, 5)) +
  labs(x = "For vote percentage",
       y = "Licenses per 1000 population") +
  facet_wrap(~periods_from_treatment, ncol = 1) +
  theme_minimal()
