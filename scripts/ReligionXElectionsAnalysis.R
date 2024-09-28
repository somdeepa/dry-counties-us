# Religion X Turnout (Precincts are ignored in this analysis)

# load data 

religion = read.csv("data/clean/religion/texas_religion.csv")

elections = read.csv("data/clean/merged/election_level_with_turnout_and_population.csv")


##---------------------------------------------------------------
##                      create county panel                     -
##---------------------------------------------------------------

temp1 = elections %>% 
  mutate(
    year = round(year(election_date)/10)*10
  ) %>% 
  group_by(county1, year) %>% 
  summarise(
    elections = n()
  )

temp2 = elections %>% 
  filter(result == "passed") %>% 
  mutate(
    year = round(year(election_date)/10)*10
  ) %>% 
  group_by(county1, year) %>% 
  summarise(
    status_changes = n()
  )

temp3 = elections %>% 
  mutate(
    year = round(year(election_date)/10)*10
  ) %>% 
  group_by(county1, year) %>% 
  summarise(
    for_share = mean(for_vote/(for_vote+against_vote))
  )

temp4 = elections %>% 
  mutate(
    year = round(year(election_date)/10)*10
  ) %>% 
  group_by(county1, year) %>% 
  summarise(
    turnout = mean(actual_turnout, na.rm = T)
  )

temp = left_join(temp1, temp2, by = c("county1", "year")) %>% 
  left_join(temp3, by = c("county1", "year")) %>% 
  left_join(temp4, by = c("county1", "year")) %>% 
  rename(county = county1)

final_data = religion %>% 
  left_join(temp, by = c("county", "year"))

final_data$elections = ifelse(is.na(final_data$elections), 0, final_data$elections)

final_data$status_changes = ifelse(is.na(final_data$status_changes), 0, final_data$status_changes)

# create all Texas trends

temp = final_data %>% 
  group_by(year, reltrad) %>% 
  summarise(
    proportion = sum(total_adherents)/sum(population)
  )

temp %>% 
  ggplot(aes(x = year, y = proportion, color = reltrad)) +
  geom_point(aes(shape = reltrad), size = 2) +
  geom_line(size = 1) + 
  theme_bw() +
  theme(
    legend.position = "bottom",  # Position the legend at the bottom
    legend.title = element_blank()  # Remove the legend title
  )
  
ggsave("results/weekly-update-9-7/religious_groups_time_series.png", device = "png")

# Scatter Plots

final_data %>% 
  ggplot(aes(x = for_share, y = proportion))+
  geom_point() +
  geom_smooth(method = "lm", se = T)+
  facet_wrap(~reltrad, ncol = 2)+
  theme_bw() +
  ylim(0,1)

ggsave("results/weekly-update-9-7/religious_groups_for_vote_share.png", device = "png")

final_data %>% 
  filter(turnout<=1) %>% 
  ggplot(aes(x = turnout, y = proportion))+
  geom_point() +
  geom_smooth(method = "lm", se = T)+
  facet_wrap(~reltrad, ncol = 2)+
  theme_bw() +
  ylim(0,1)

ggsave("results/weekly-update-9-7/religious_groups_turnout.png", device = "png")