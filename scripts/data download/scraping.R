# scrape some data!

# will scrape some data on moratorium ordinance dates from the following webpage

url =  "https://www.chicago.gov/city/en/depts/bacp/supp_info/liquor_license_restrictionsandspecialregulations.html"


table = html_nodes(read_html(url), "table") %>% 
  html_table()

write.csv(table[[1]], "data/raw/Liquor/ConsumptionOnPremisesMoratorium")

write.csv(table[[2]], "data/raw/Liquor/PackagedGoodsMoratorium")
