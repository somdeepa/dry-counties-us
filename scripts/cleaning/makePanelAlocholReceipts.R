
#################################################################
##       Make panel and time series of tax receipts data       ##
#################################################################

receipts = readRDS("data/clean/liquor-taxes/liquor-receipts.rds")

# Inflation adjust all receipts!

cpi = read.csv("data/raw/economy/price index/South-urban-cpi.csv") %>% 
  rename(year = Year, cpi = Value) %>% 
  mutate(month = month(ym(Label))) %>% 
  select(year, month, cpi)

target_cpi = cpi[cpi$year == 2010 & cpi$month == 1, "cpi"]

receipts = receipts %>% 
  # filtering out 1993 as only data for November and December
  filter(year!= 1993) %>% 
  left_join(., cpi, by = c("year", "month")) %>% 
  mutate(receipts_adjusted = receipts*target_cpi/cpi)

