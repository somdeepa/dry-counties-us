# Clean Election Results for Local Elections in Texas from pdf files
# No OCR (Data from years 1997 onwards)
# Last updated: 30-4-2024

# clean filenames of election results

filenames = list.files("data/raw/election-results/", full.names = T)

new_filenames = str_extract_all(filenames, "\\d+")

new_filenames = ifelse(as.numeric(new_filenames) < 25, str_c("20", new_filenames), new_filenames)

new_filenames = ifelse(as.numeric(new_filenames) < 100, str_c("19", new_filenames), new_filenames)

new_filenames = str_c("data/raw/election-results/",new_filenames,".pdf")

file.rename(from = filenames, to = new_filenames)

# read PDFs of election results post 1997

years = seq(1997, 2020)

data = do.call(rbind,lapply(years, parse_pdfs))

names(data)[1:12] = c("county", "city", "jp_precinct", "election_date", "issue", 
                     "result", "for_vote", "against_vote", "status_before", 
                     "status_current", "status_before_detail", "status_current_detail")
# savedata

write.csv(data, "data/clean/local-option-elections-97-2020.csv")

# Data validation and notes

# Some county names are invalid, as the cities belong to multiple counties

which(is.na(data$city) & is.na(data$jp_precinct) & is.na(data$county))

data[which(is.na(status_before_detail) & status_before=="wet"),]
data[which(is.na(status_current_detail) & status_current=="wet"),]


##---------------------------------------------------------------
##                          FUNCTIONS                           -
##---------------------------------------------------------------

# function to read PDF files, extract text, and filter relevant paragraphs

parse_pdfs = function(year){
  
  filename = str_c("data/raw/election-results/", year, ".pdf")
  text = paste(pdf_text(filename), collapse = "")
  paragraphs = str_split(text, "\n\n")[[1]]
  
  selected_paragraphs = paragraphs[grepl("election was held", paragraphs)]
  
  data = lapply(selected_paragraphs,extract_info)
  
  df = as.data.frame(do.call(rbind, data))
  
  df = df %>% mutate(
    fiscal_year = year
  )
  
  return(df)
}

# Function extracts information from a single paragraph

extract_info = function(paragraph) {
  
  paragraph = gsub("\n", " ", paragraph)
  
  county = sub(".*\\b(\\w+)\\s+County.*", "\\1", paragraph)
  
  city = regmatches(paragraph, regexpr("City\\s+[^,]+", paragraph))
  
  #city = regmatches(paragraph, regexpr("City\\s+\\w+\\s+\\w+\\b", paragraph))
  
  jp_precinct = regmatches(paragraph, regexpr("Precinct \\d+", paragraph, perl = TRUE))
  
  date = regmatches(paragraph, regexpr("\\b(?:January|February|March|April|May|June|July|August|September|October|November|December)\\s+\\d{1,2},\\s+\\d{4}\\b", paragraph, ignore.case = TRUE))
  
  issue = regmatches(paragraph, regexpr("\\blegal sale of\\b.*?\\.", paragraph, 
                                        ignore.case = TRUE, perl = TRUE))
  
  for_vote = as.integer(gsub(",", "", regmatches(paragraph, regexpr("\\b\\d{1,3}(?:,\\d{3})*\\b(?=\\sFOR)", paragraph, perl = TRUE))))
  
  against_vote = as.integer(gsub(",", "",regmatches(paragraph, regexpr("\\b\\d{1,3}(?:,\\d{3})*\\b(?=\\sAGAINST)", paragraph, perl = TRUE))))
  
  result = ifelse(for_vote>against_vote, "passed", "failed")
  
  status = regmatches(paragraph, gregexpr("\\bdry\\b|\\bwet\\b", paragraph, ignore.case = TRUE))[[1]]
  status_before = status[1]
  status_current = status[2]
  
  status_before_detail = NA
  status_current_detail = NA
  
  # extract detailed status
  # Extract status_before_detail (if wet)
  if(!is.na(status_before) && status_before == "wet"){
    status_before_detail <- regmatches(paragraph, regexec("(?<=sale of\\s)[^.]*(?=\\bbefore\\b)", paragraph, perl = TRUE))[[1]]
  }
  
  if(!is.na(status_current) && status_current == "wet"){
    status_current_detail <- regmatches(paragraph, regexec("(?<=now\\s)(.*)", paragraph, perl = TRUE))[[1]][1]
  }
  
  return(c(
    ifelse(length(county) == 0, NA, county),
    ifelse(length(city) == 0, NA, city),
    ifelse(length(jp_precinct) == 0, NA, jp_precinct),
    ifelse(length(date) == 0, NA, date),
    ifelse(length(issue) == 0, NA, issue),
    ifelse(length(result) == 0, NA, result),
    ifelse(length(for_vote) == 0, NA, for_vote),
    ifelse(length(against_vote) == 0, NA, against_vote),
    ifelse(length(status_before) == 0, NA, status_before),
    ifelse(length(status_current) == 0, NA, status_current),
    ifelse(length(status_before_detail) == 0, NA, status_before_detail),
    ifelse(length(status_current_detail) == 0, NA, status_current_detail)
  ))
}


