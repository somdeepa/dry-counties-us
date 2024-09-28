
##----------------------------------------------------------------------------
##  Download registered voters and turnout data for Texas counties by year   -
##----------------------------------------------------------------------------

# https://www.sos.state.tx.us/elections/historical/counties.shtml

countynames = read.csv("data/clean/fips codes/texas_county_fips.csv")

urls = str_c("https://www.sos.state.tx.us/elections/historical/", 
             tolower(str_remove(countynames$county, " ")), ".shtml")


get_county_data = function(county_name) {
  url = str_c("https://www.sos.state.tx.us/elections/historical/", 
              tolower(str_replace_all(county_name, " ", "")), ".shtml")
  
  # Read the HTML table from the URL
  data = read_html(url) %>%
    html_table(fill = TRUE) %>%
    .[[1]] %>% 
    select(1,2,3,5) %>% 
    mutate(
      registered_voters = clean_numeric(`Reg Voters`),
      voted = clean_numeric(Voted),
      early_vote = clean_numeric(`Early Vote`)
    ) %>% 
    select(YEAR, registered_voters, voted, early_vote)
  
  # Add the county name to the data
  data$county = county_name
  return(data)
}

panel_data = lapply(countynames$county, get_county_data) %>% 
  bind_rows()

# Add data from Coleman county manually

coleman <- data.frame(
  year = c(1988, 1990, 1992, 1994, 1996, 1998, 2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022),
  registered_voters = c(6428, 6011, 6357, 6348, 6968, 7123, 7240, 7220, 7498, 7696, 8027, 7917, 7793, 7610, 7656, 7520, 7959, 8066),
  voted = c(4340, 3106, 5028, 3477, 4160, 2944, 4637, 3355, 5288, 3532, 5339, 3454, 5057, 2842, 5013, 4326, 5736, 4487),
  county = "Coleman"
) %>% 
  mutate(
    turnout = voted/registered_voters
  )

final_data = panel_data %>% 
  mutate(
    turnout = voted/registered_voters
  ) %>% 
  rename(year = YEAR) %>% 
  arrange(county, year) %>% 
  select(!early_vote) %>% 
  filter(county!="Coleman") %>% 
  bind_rows(., coleman)

# fix data issues

final_data[final_data$county=="Hockley",'year'] =  c(seq(1988,2006,2), 2010, 2008, seq(2012, 2022, 2))

write.csv(final_data, "data/clean/voting/voter-turnout-by-county-texas-1988-2022.csv", row.names = F)



##----------------------------------------------------------------
##                Download all Texas voting data                 -
##----------------------------------------------------------------

url = "https://www.sos.state.tx.us/elections/historical/70-92.shtml"

texas_data = read_html(url) %>%
  html_table(fill = TRUE, header = F) %>%
  .[[1]]


indices = which(grepl("^[0-9]", texas_data$X1))

texas_turnout = tibble(
  election = texas_data$X1[indices],
  registered_voters = clean_numeric(texas_data$X2[indices+1]),
  voted = clean_numeric(texas_data$X2[indices + 4])
) %>% 
  mutate(
    year = as.numeric(substr(election, 1, 4)),
    election_type = case_when(
      grepl("Primary.*Presidential", election) ~ "primary_presidential",
      grepl("Primary.*Gubernatorial", election) ~ "primary_gubernatorial",
      grepl("Gubernatorial", election) ~ "gubernatorial",
      grepl("Presidential|General", election) ~ "presidential",
      grepl("Constitutional", election) ~ "constitutional",
      TRUE ~ "other"
    )
  ) %>% 
  filter(election_type %in% c("presidential", "gubernatorial")) %>% 
  mutate(all_texas_turnout = voted/registered_voters) %>% 
  select(year, all_texas_turnout)

write.csv(texas_turnout, "data/clean/voting/1970-2022-turnout-texas-all.csv", row.names = F)
