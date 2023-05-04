# Title: Practicum II Part 1
# Author: Zhongwei Zhang
# Date: April 18, 2023
# Course: CS5200


# Note: You might see warnings such as "There were 50 or more warnings (use warnings() to see the first 50)",
# which is totally normal in test cases and won't affect the final results
 


# Load necessary packages
library(RSQLite)
library(XML)
library(DBI)


# Get the current working directory and create the database file path
# fpath <- getwd()
dname <- "pubmed.db"
db_path <- dname
xml_path <- "pubmed-tfm-xml/pubmed22n0001-tf.xml"

# Create a SQLite database connection to the "pubmed.db" file
con <- dbConnect(RSQLite::SQLite(), db_path)

# Load XML file
xml_file <- xml_path
xml_data <- xmlParse(xml_file)

# Drop tables if they already exist
table_names <- c("Article_Authors", "Authors", "Articles", "Journals")
for (table_name in table_names) {
  if (dbExistsTable(con, table_name)) {
    dbExecute(con, paste0("DROP TABLE ", table_name, ";"))
  }
}

# UML
knitr::include_graphics("https://i.imgur.com/gnCNfAy.png")


# Create the tables
dbExecute(con, "CREATE TABLE journals (id INTEGER PRIMARY KEY, issn TEXT, issn_type TEXT, title TEXT, iso_abbreviation TEXT, volume INTEGER, issue INTEGER, pub_date DATE);")
dbExecute(con, "CREATE TABLE articles (id INTEGER PRIMARY KEY, pmid INTEGER, title TEXT, journal_id INTEGER, FOREIGN KEY(journal_id) REFERENCES journals(id));")
dbExecute(con, "CREATE TABLE authors (id INTEGER PRIMARY KEY, last_name TEXT, fore_name TEXT, initials TEXT);")
dbExecute(con, "CREATE TABLE article_authors (id INTEGER PRIMARY KEY, article_id INTEGER, author_id INTEGER, FOREIGN KEY(article_id) REFERENCES articles(id), FOREIGN KEY(author_id) REFERENCES authors(id));")

# Check if the "pubmed.db" file exists
if (file.exists(db_path)) {
  cat("The pubmed.db file exists in the current working directory.\n")
} else {
  cat("The pubmed.db file does not exist in the current working directory.\n")
}


# *****************************************************************

# Helper functions

# to help extract journal
extract_journal <- function(journal_node) {
  issn_node <- journal_node[["ISSN"]]
  if (length(issn_node) > 0) {
    issn <- xmlValue(issn_node)
    issn_type <- xmlGetAttr(issn_node, "IssnType")
  } else {
    issn <- NA
    issn_type <- NA
  }
  title <- xmlValue(journal_node[["Title"]])
  iso_abbreviation <- xmlValue(journal_node[["ISOAbbreviation"]])
  
  list(issn = issn, issn_type = issn_type, title = title, iso_abbreviation = iso_abbreviation)
}

# to help extract journal issue - date
extract_journal_issue <- function(journal_issue_node) {
  volume <- as.integer(xmlValue(journal_issue_node[["Volume"]]))
  issue <- as.integer(xmlValue(journal_issue_node[["Issue"]]))
  pub_date_node <- journal_issue_node[["PubDate"]]
  
  year <- as.integer(xmlValue(pub_date_node[["Year"]]))
  month_text <- as.character(xmlValue(pub_date_node[["Month"]]))
  
  if (is.na(year) || is.na(month_text) || year == "" || month_text == "") {
    medline_date <- as.character(xmlValue(pub_date_node[["MedlineDate"]]))
    if (!is.na(medline_date) && medline_date != "") {
      year <- as.integer(substr(medline_date, 1, 4))
      
      month_regex <- "\\b(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\\b"
      month_match <- regmatches(medline_date, regexpr(month_regex, medline_date))
      
      if (length(month_match) > 0) {
        month_text <- month_match[[1]][1]
      } else {
        month_text <- NA
      }
    }
  }
  
  # Use "01" if the month is missing
  if (is.na(month_text) || month_text == "") {
    month <- "01"
  } else {
    # Convert the month name to its corresponding integer value
    month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    month <- match(month_text, month_names)
    # Ensure the month value has a leading zero if it's a single digit
    month <- formatC(month, width = 2, flag = "0")
  }
  
  # Use "01" if the day is missing
  day <- as.integer(xmlValue(pub_date_node[["Day"]]))
  if (is.na(day) || day == "") {
    day <- "01"
  } else {
    # Ensure the day value has a leading zero if it's a single digit
    day <- formatC(day, width = 2, flag = "0")
  }
  
  date <- paste(year, month, day, sep = "-")
  
  list(volume = volume, issue = issue, pub_date = date)
}




# to help extract author
extract_author <- function(author_node) {
  last_name <- xmlValue(author_node[["LastName"]])
  fore_name <- xmlValue(author_node[["ForeName"]])
  initials <- xmlValue(author_node[["Initials"]])
  
  # Check if the last name contains a mix of letters and uppercase initials, and split them
  last_name_split <- strsplit(last_name, "((?<=[a-z])(?=[A-Z]))|((?<=[A-Z])(?=[A-Z][a-z]))", perl = TRUE)[[1]]
  if (length(last_name_split) > 1 && (fore_name == "" || is.na(fore_name))) {
    last_name <- last_name_split[1]
    initials <- last_name_split[2]
  }
  
  list(last_name = last_name, fore_name = fore_name, initials = initials)
}



# *****************************************************************


# Iterate through the top-level nodes
article_nodes <- getNodeSet(xml_data, "//Article")

for (article_node in article_nodes) {
  if (is.null(article_node)) {
    cat("Skipping article node due to NULL article_node\n")
    next
  }
  
  pmid_attr <- xmlAttrs(article_node)["PMID"] 
  if (is.null(pmid_attr)) {
    cat("Skipping article node due to missing PMID attribute\n")
    next
  }
  pmid <- as.integer(pmid_attr)
  
  
  if (is.null(pmid) || is.na(pmid)) {
    cat("Skipping article node due to missing PMID\n")
    next
  }
  
  pub_details_node <- article_node[["PubDetails"]]
  
  if (is.null(pub_details_node)) {
    cat("Skipping article node due to missing PubDetails node\n")
    next
  }
  
  title_node <- getNodeSet(pub_details_node, ".//ArticleTitle")
  title <- if (length(title_node) > 0) xmlValue(title_node[[1]]) else NA
  journal_node <- pub_details_node[["Journal"]]
  journal_issue_node <- journal_node[["JournalIssue"]]
  author_list_node <- pub_details_node[["AuthorList"]]
  
  # Initialize author_nodes as an empty list to handle cases where AuthorList is NULL
  author_nodes <- list()
  
  # Check if author_list_node is not NULL before calling getNodeSet
  if (!is.null(author_list_node)) {
    author_nodes <- getNodeSet(author_list_node, "Author")
  }
  
  # Extract Journal, Journal Issue, and Author information
  journal_info <- extract_journal(journal_node)
  journal_issue_info <- extract_journal_issue(journal_issue_node)
  
  # Insert Journal data into the database
  if (!is.na(journal_info$issn)) {
    journal_id <- dbGetQuery(con, paste0("SELECT id FROM journals WHERE issn = ", dbQuoteString(con, journal_info$issn), ";"))
    
    if (nrow(journal_id) == 0) {
      dbExecute(con, "INSERT INTO journals (issn, issn_type, title, iso_abbreviation, volume, issue, pub_date) VALUES (?, ?, ?, ?, ?, ?, ?);",
                params = c(journal_info$issn, journal_info$issn_type, journal_info$title, journal_info$iso_abbreviation, journal_issue_info$volume, journal_issue_info$issue, journal_issue_info$pub_date))
      journal_id <- dbGetQuery(con, paste0("SELECT id FROM journals WHERE issn = ", dbQuoteString(con, journal_info$issn), ";"))
    }
  } else {
    journal_id <- data.frame(id = NA)
  }
  
  # Insert Article data into the database
  dbExecute(con, "INSERT INTO articles (pmid, title, journal_id) VALUES (?, ?, ?);",
            params = c(pmid, title, ifelse(is.na(journal_id$id), NA, journal_id$id)))

  # Iterate through Authors and insert Author data into the database
  if (!is.null(author_list_node)) {
    author_nodes <- getNodeSet(author_list_node, "Author")
  }
  
  for (author_node in author_nodes) {
    author_info <- extract_author(author_node)
    
    # Check if the author exists in the database
    author_id <- dbGetQuery(con, paste0("SELECT id FROM authors WHERE last_name = ", dbQuoteString(con, author_info$last_name),
                                        " AND fore_name = ", dbQuoteString(con, author_info$fore_name), " AND initials = ", dbQuoteString(con, author_info$initials), ";"))
    
    if (nrow(author_id) == 0) {
      dbExecute(con, "INSERT INTO authors (last_name, fore_name, initials) VALUES (?, ?, ?);",
                params = c(author_info$last_name, author_info$fore_name, author_info$initials))
      author_id <- dbGetQuery(con, paste0("SELECT id FROM authors WHERE last_name = ", dbQuoteString(con, author_info$last_name),
                                          " AND fore_name = ", dbQuoteString(con, author_info$fore_name), " AND initials = ", dbQuoteString(con, author_info$initials), ";"))
      
    }
    
    # Only insert the Article-Author relationship into the article_authors table if pmid and author_id$id are not NA
    if (!is.na(pmid) && nrow(author_id) > 0 && !is.na(author_id$id)) {
      dbExecute(con, "INSERT INTO article_authors (article_id, author_id) VALUES (?, ?);",
                params = c(pmid, author_id$id))
    }
    
  }
  
}

# Close the SQLite connection
dbDisconnect(con)
  