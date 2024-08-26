# Graphs and tables for Research methods presentation on 25-03
library(sf)
library(RColorBrewer)
# Last updated: 1-05-2024
# ==============================================================================
# Map of wet and dry laws in Texas
# ==============================================================================



electionresults = read.csv("data/clean/local-option-elections-97-2020.csv")

years = seq(1997,2020)

status_evolution <- status1996 %>% 
  mutate(
    status = case_when(
      Wet == 1 & Dry_in_part == 0 | Moist == 1 ~ "Wet/moist",
      Dry == 1 ~ "Dry",
      Dry_in_part == 1 ~ "Dry in parts"
    ),
    year = 1996
  ) %>% 
  select(County.Name, status, year)

curr_year = 1997

data = electionresults %>% filter(fiscal_year == curr_year)
# initialize current year
current_year = status_evolution %>% filter(year == curr_year -1)


popByCounty = read.csv("data/clean/texas_population_by_county")

# ==============================================================================
# Choropleth map
# ==============================================================================

status1996 = read.csv("data/clean/county_status_1996.csv")

status2020 = read.csv("data/clean/county_status_2020.csv")

status_merged = inner_join(status1996, status2020, by = "county")

countyfips = read.csv("data/clean/texas_county_fips.csv")

status_merged = left_join(status_merged, countyfips, by="county")

county_shapefile = st_read("data/raw/geography/Texas_County_Boundaries_Detailed_6830276903728206656/County.shp") %>% 
  mutate(countyfips = as.integer(`CNTY_FIPS`)-48000)

merged_data <- left_join(county_shapefile, status_merged,  by = "countyfips")

p1=ggplot() +
  geom_sf(data = merged_data, aes(fill = status_1996)) +
  scale_fill_manual(values = c("gray10", "gray60", "gray90")) +
  labs(title = "1997") +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        legend.title = element_blank())

ggsave("results/research-methods-presentation-2/1996_status.pdf", device = "pdf")

p2=ggplot() +
  geom_sf(data = merged_data, aes(fill = status_2020)) +
  scale_fill_manual(values = c("gray10", "gray60", "gray90")) +
  labs(title = "2020") +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        legend.title = element_blank())

ggsave("results/research-methods-presentation-2/2020_status.pdf", device = "pdf")

(p1 + p2) + plot_layout(guides = 'collect')

ggsave("results/research-methods-presentation-2/status1996-2020.pdf", device = "pdf")
