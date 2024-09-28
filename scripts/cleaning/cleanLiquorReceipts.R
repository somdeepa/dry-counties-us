
##################################################################
##                  CLEAN LIQUOR RECEIPTS DATA                  ##
##################################################################


##----------------------------------------------------------------
##        1993-2007 (Data obtained from Texas comptroller)       -
##----------------------------------------------------------------


filenames = c(str_c("data/raw/liquor-taxes/MBDataLBW", seq(1993, 1999), ".csv"),
              str_c("data/raw/liquor-taxes/MBDtataLBW", seq(2000,2007), ".csv"))
  
column_names <- c("Taxpayer.Number", "Taxpayer.Name", "Taxpayer.Address", "Taxpayer.City", 
                  "Taxpayer.State", "Taxpayer.Zip", "Taxpayer.County", "Taxpayer.Phone",
                  "Location.Number", "Location.Name", "Location.Address", "Location.City", 
                  "Location.State", "Location.Zip", "Location.County", "Location.Phone", 
                  "TABC.Permit.Number", "Responsibility.Begin.Date", "Responsibility.End.Date",
                  "Obligation.End.Date", "Liquor.Receipts", "Wine.Receipts", 
                  "Beer.Receipts", "Total.Receipts")

read_files = function(file){
  data = read.csv(file)
  names(data) = column_names
  return(data)
}

select_vars = c("Location.Name", "Location.Address", "Location.City", 
                "Location.State", "Location.Zip", "Location.County",
                "TABC.Permit.Number", "Responsibility.Begin.Date", "Responsibility.End.Date",
                "Obligation.End.Date", "Liquor.Receipts", "Wine.Receipts", 
                "Beer.Receipts", "Total.Receipts")

pre2007 = do.call(rbind,lapply(filenames, read_files)) %>% 
  select(all_of(select_vars)) %>% 
  mutate(
    Responsibility.Begin.Date = as.Date(as.character(Responsibility.Begin.Date), format = "%Y%m%d"),
    Responsibility.End.Date = as.Date(as.character(Responsibility.End.Date), format = "%Y%m%d"),
    Obligation.End.Date = as.Date(as.character(Obligation.End.Date), format = "%Y%m%d")
  )



##---------------------------------------------------------------
##            2007 onwards (Texas Open Data Portal)             -
##---------------------------------------------------------------

post2007 = read.csv("data/raw/liquor-taxes/Mixed_Beverage_Gross_Receipts_20240909.csv") %>% 
  select(all_of(select_vars)) %>% 
  mutate(
    Responsibility.Begin.Date = as.Date(as.character(Responsibility.Begin.Date), format = "%m/%d/%Y"),
    Responsibility.End.Date = as.Date(as.character(Responsibility.End.Date), format = "%m/%d/%Y"),
    Obligation.End.Date = as.Date(as.character(Obligation.End.Date), format = "%m/%d/%Y")
  ) %>% 
  filter(year(Obligation.End.Date) != 2007) # double counting 2007 so filter it out

# put data sets together

# Three pairs of municipalities share the same name: Lakeside, Oak Ridge, and Reno
# will modify city name to reflect this
# Also resolve duplicate cities with different spellings

duplicate_cities = c("lakeside", "oak ridge", "reno")


joined_data = bind_rows(pre2007, post2007) %>% 
  distinct() %>% 
  rename(
    name = Location.Name,
    address = Location.Address,
    city = Location.City,
    state = Location.State,
    zip = Location.Zip,
    county = Location.County,
    combined_number = TABC.Permit.Number,
    responsibility_begin_date = Responsibility.Begin.Date,
    responsibility_end_date = Responsibility.End.Date,
    obligation_end_date = Obligation.End.Date,
    liquor_receipts = Liquor.Receipts,
    wine_receipts = Wine.Receipts,
    beer_receipts = Beer.Receipts,
    total_receipts = Total.Receipts
  ) %>% 
  mutate(
    year = year(obligation_end_date),
    month = month(obligation_end_date),
    quarter = sapply(obligation_end_date, convert_to_quarter),
    
    # combine beer and wine
    beer_wine_receipts = beer_receipts + wine_receipts,
    
    # city name corrections
    city = str_trim(city),
    city = ifelse(city %in% duplicate_cities, str_c(city, " (", county, ")"), city),
    city = ifelse(city == "LAKESIDE VILLAGE", "LAKESIDE", city),
    city = ifelse(city == "LAKESIDE CITY", "LAKESIDE", city),
    city = ifelse(city == "CANYON CITY", "CANYON", city)
  ) %>% 
  select(-c(beer_receipts, wine_receipts)) %>% 
  # wide-to-long
  pivot_longer(
    cols = ends_with("receipts"),
    names_to = "type",
    values_to = "receipts",
    names_transform = list(type = ~gsub("_receipts", "", .))
  )

saveRDS(joined_data, "data/clean/liquor-taxes/liquor-receipts.rds")
