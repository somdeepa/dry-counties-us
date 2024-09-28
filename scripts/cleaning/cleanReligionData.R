# clean religion data from U.S. Religion Census

religion = read_dta("data/raw/religion/Longitudinal Religious Congregations and Membership File, 1980-2010 (County Level).DTA") %>% 
  tibble() %>% filter(stateab == "TX")

countyfips = read.csv("data/clean/fips codes/texas_county_fips.csv")

# create county-wide dataset 

final_data = religion %>% 
  group_by(fipsmerg, year, reltrad) %>% 
  summarise(
    total_adherents = sum(adherent, na.rm = T),
    proportion = total_adherents/unique(totpop)
  ) %>% 
  #rename(countyfips = fipsmerg) %>% 
  mutate(
    countyfips = fipsmerg - 48000,
    reltrad = case_when(
      reltrad == 1 ~ "Evangelical Protestant",
      reltrad == 2 ~ "Mainline Protestant",
      reltrad == 3 ~ "Catholic",
      reltrad == 4 ~ "Other",
      reltrad == 5 ~ "Orthodox",
      reltrad == 6 ~ "Black Protestant"
    )
  ) %>% 
  left_join(countyfips, by = "countyfips")

write.csv(final_data, "data/clean/religion/texas_religion.csv", row.names = F)
