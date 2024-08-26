# Cleaning election results prior to 1997
# Requires OCR
# Last updated 23-03-2004

# work on 1996 file only
# Get status of all counties in Texas

data = pdf_ocr_text("data/raw/election-results/1996.pdf",
             pages = c(1,2,3)) %>% unlist()

paragraphs = str_split(data[3], "\n\n")[[1]]

selected_paragraphs = paragraphs[grepl("election was held", paragraphs)]

cleaned_text <- gsub("\n", " ", data[3])
cleaned_text <- gsub("\\s+", " ", cleaned_text)

# Use regex to split into paragraphs based on pattern
paragraphs <- unlist(strsplit(cleaned_text, sprintf("(?<=%s)", "A local option election was held on"), perl = TRUE))
paragraphs = grep("^A local option election was held on", paragraphs, value = TRUE)

# Prepend the pattern back to each paragraph (except the first one)
paragraphs[-1] <- paste("A local option election was held on", paragraphs[-1], sep = "")

# Trim leading and trailing spaces
paragraphs <- trimws(paragraphs)
