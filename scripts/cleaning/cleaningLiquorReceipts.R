
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
  filter(year(Obligation.End.Date) != 2007)

joined_data = bind_rows(pre2007, post2007) %>% 
  distinct() %>% 
  mutate(
    year = year(Obligation.End.Date)
  )

summarytable = joined_data %>% 
  group_by(year) %>% 
  summarise(
    count = n(),
    Total = sum(Total.Receipts, na.rm = T)
  )

ggplot(summarytable, aes(x = year, y = Total)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "Total Receipts by Year",
    x = "Year",
    y = "Total Receipts"
  ) +
  theme_minimal()
0