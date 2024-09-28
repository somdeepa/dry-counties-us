# make quick and dirty panel of elections with results that are present in csv's
# only years 2004-2018
# only pass/fail results

# Last updated : 21-3-2024

years = as.character(seq(2004, 2018))
data_list <- list()

filenames = list.files("data/raw/elections-csv-temporary/")

for (year in years){
  data = read.csv(paste0("data/raw/elections-csv-temporary/", filenames[grep(year, filenames)]), header = F)
  data$year = year
  data_list[[year]] = data
}

combined_data <- do.call(rbind, data_list)

# rename columns

names(combined_data)[1:6] = c("county", "city/precinct", "type", "petition_status",
                              "election_date", "result")


write.csv(combined_data, "data/clean/elections_2004_18.csv")

# Explore the data

# Petitions per year

counts_by_year <- combined_data %>%
  filter(petition_status == "Returned") %>%
  group_by(year) %>%
  count()

# Create a bar plot
ggplot(counts_by_year, aes(x = year, y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Count", title = "Number of elections per year") +
  theme_minimal()
