# Spatial project (AG Econ spatial and Yong Bao course)

# Load data

# read fips codes (county)

countyfips = read.csv("data/clean/texas_county_fips.csv")

# population by county
popdata = read.csv("data/clean/texas_population_by_county_1997_2022.csv") %>% select(2:4) %>% 
  filter(year %in% seq(2003, 2019))

county_shapefile = st_read("data/raw/Texas_County_Boundaries_Detailed_6830276903728206656/County.shp") %>% 
  mutate(countyfips = as.integer(`CNTY_FIPS`)-48000)

# Crashes data
crashes = read.csv("data/clean/texas_crashes_by_county_2003_2019.csv") %>% select(2:4)

# liquor licenses by county-year
licenses = read.csv("data/clean/active-licenses-by-county.csv") %>% select(2:4)

# make final panel with crashes and licenses

panel_data = left_join(popdata, countyfips, by="countyfips") %>% 
  left_join(., crashes, by = c("county", "year")) %>% 
  left_join(., licenses, by = c("county", "year")) %>% 
  mutate(
    licenses = total_businesses/population*1000,
    crashrate = annual_dui_crashes/population*1000
  ) %>% 
  filter(year >= 2006)


##---------------------------------------------------------------
##                        MAKE SOME MAPS!                       -
##---------------------------------------------------------------

# prepare the data, join with boundaries

panel_data_merge = panel_data %>% 
  group_by(year) %>% 
  mutate(
    crashrate_quartile = cut(crashrate, 
                             breaks = quantile(crashrate),
                             include.lowest = T),
    licenses_quartile = cut(licenses,
                            breaks = quantile(licenses),
                            include.lowest = T)
  ) %>% 
  ungroup() %>% 
  left_join(., county_shapefile, by = "countyfips")

# crashrate map

panel_data_merge %>% 
  filter(year == 2006) %>% 
  ggplot(aes(fill = as.factor(crashrate_quartile))) +
  geom_sf(aes(geometry = geometry)) +
  scale_fill_brewer(palette = "Oranges") +
  theme_minimal() +
  labs(title = "2006") +
  theme(
    legend.title = element_blank(),
    axis.text = element_blank()
  )

ggsave("results/spatial-project/crashrate_map_2006.pdf", device = "pdf")

panel_data_merge %>% 
  filter(year == 2019) %>% 
  ggplot(., aes(fill = as.factor(crashrate_quartile))) +
  geom_sf(aes(geometry = geometry)) +
  scale_fill_brewer(palette = "Oranges") +
  theme_minimal() +
  labs(title = "2019") +
  theme(
    legend.title = element_blank(),
    axis.text = element_blank()
  )

ggsave("results/spatial-project/crashrate_map_2019.pdf", device = "pdf")

# licenses map

panel_data_merge %>% 
  filter(year == 2006) %>% 
  ggplot(aes(fill = as.factor(licenses_quartile))) +
  geom_sf(aes(geometry = geometry)) +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal() +
  labs(title = "2006") +
  theme(
    legend.title = element_blank(),
    axis.text = element_blank()
  )

ggsave("results/spatial-project/licenses_map_2006.pdf", device = "pdf")

panel_data_merge %>% 
  filter(year == 2019) %>% 
  ggplot(., aes(fill = as.factor(licenses_quartile))) +
  geom_sf(aes(geometry = geometry)) +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal() +
  labs(title = "2019") +
  theme(
    legend.title = element_blank(),
    axis.text = element_blank()
  )

ggsave("results/spatial-project/licenses_map_2019.pdf", device = "pdf")


##----------------------------------------------------------------
##                  WEIGHT MATRIX construction                   -
##----------------------------------------------------------------

nb_contiguity = poly2nb(county_shapefile)

# contiguity matrix (row normalized)

W = nb2mat(nb_contiguity, style = "W")

listW = mat2listw(W)

##----------------------------------------------------------------
##                          REGRESSION                           -
##----------------------------------------------------------------

# TWFE

twfe_results = feols(crashrate ~ licenses | countyfips + year, panel_data,
                         cluster = "countyfips")

# Bayesian posterior probabilities

model_comparison<-blmpSDPD(formula = crashrate ~ licenses, data = panel_data, W = W,
               index = c("countyfips","year"),
               model = list("ols","sar","sdm","sem","sdem","slx"), 
               effect = "individual",
               prior = "uniform")

model_comparison

sdm<-SDPDm(formula = crashrate ~ licenses, data = panel_data, W = W,
              index = c("countyfips","year"),
              model = "sdm", 
              effect = "twoways")

sar<-SDPDm(formula = crashrate ~ licenses, data = panel_data, W = W,
           index = c("countyfips","year"),
           model = "sar", 
           effect = "twoways")

sem = SDPDm(formula = crashrate ~ licenses, data = panel_data, W = W,
            index = c("countyfips","year"),
            model = "sdm", 
            effect = "time")

impact_sdm = impactsSDPDm(sdm)

impact_sem = impactsSDPDm(sem)

impact_sar = impactsSDPDm(sar)
