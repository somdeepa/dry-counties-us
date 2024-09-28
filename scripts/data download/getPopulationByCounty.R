# Downloads population-by-county and city data

# last updated: 23-3-2024

url = "https://txcip.org/tac/census/towns.html"

tables = read_html(url) %>% 
  html_nodes("table") %>% 
  html_table()

popByCounty <- bind_rows(tables)

write.csv(popByCounty, "data/clean/texas_population_by_county.csv", rownames = F)
