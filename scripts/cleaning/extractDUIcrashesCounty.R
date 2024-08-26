# extracts the crashes data from TXDOT data pdfs (county level)
# Note: from 2003-05, data not present for 5-7 counties
# Last updated: 17-04-2024

years = seq(2003, 2019)

# extract data from all files 

crashdata = lapply(years, extractAllTables)
crashdata = as.data.frame(do.call(rbind, crashdata))
crashdata[,2] = as.integer(gsub(",", "", crashdata[,2])) 

# clean county names (De Witt and La Salle)
crashdata = crashdata %>% 
  mutate(
    county = ifelse(county == "Lasalle", "La Salle", county),
    county = ifelse(county == "Dewitt", "De Witt", county)
  )

# save dataset

write.csv(crashdata, "data/clean/texas_crashes_by_county_2003_2019.csv")


##---------------------------------------------------------------
##                          FUNCTIONS                           -
##---------------------------------------------------------------

# extract data from a file

extractAllTables = function(year){
  
  filename = str_c("data/raw/dui-crashes-and-fatalities/county/", year, ".pdf")
  
  pdf_text = strsplit(pdf_text(filename), "\n")
  
  pdf_data = lapply(pdf_text, extractTable)
  
  df = do.call(rbind, pdf_data)
  
  # extract only the last column (total crashes), exclude total row
  
  df = head(df[, c(1,ncol(df))],-1)
  
  names(df) = c("county","annual_dui_crashes")
  
  df$year = year
  
  return(df)
}

# extract data from a single page

extractTable = function(page){
  
  page = page[(grep("^County", page)+1):(grep("^(Information|Effective)", page)-1)]
  
  page_data = lapply(page, function(x) unlist(strsplit(x, " {2,}")))
  
  df = as.data.frame(do.call(rbind, page_data))
  
  return(df)
  
}