# Install necessary packages (if not already installed)
if (!require("RMySQL")) {
  install.packages("RMySQL")
}
if (!require("DBI")) {
  install.packages("DBI")
}

# Load necessary packages
library(DBI)
library(RMySQL)

# Connect to the SQLite and MySQL databases
source_db_path <- "pubmed.db"
source_con <- dbConnect(RSQLite::SQLite(), source_db_path)

mysql_host <- "localhost"
mysql_user <- "root"
mysql_password <- "Zzw@1299123"
mysql_db <- "practicum2db"

mysql_con <- dbConnect(RMySQL::MySQL(),
                       host = mysql_host,
                       user = mysql_user,
                       password = mysql_password,
                       dbname = mysql_db)

# Create a temporary table in SQLite for author facts
dbExecute(source_con, "
CREATE TEMPORARY TABLE author_facts_temp AS
SELECT
  a.id AS author_id,
  a.last_name || ' ' || a.fore_name AS author_name,
  COUNT(DISTINCT aa.article_id) AS num_articles,
  COUNT(DISTINCT coa.author_id) - COUNT(DISTINCT aa.article_id) AS num_coauthors
FROM authors a
LEFT JOIN article_authors aa ON a.id = aa.author_id
LEFT JOIN article_authors coa ON aa.article_id = coa.article_id
GROUP BY a.id;
")

# Drop the 'author_facts' table if it exists
dbExecute(mysql_con, "DROP TABLE IF EXISTS author_facts;")

# Create the author fact table in MySQL
dbExecute(mysql_con, "CREATE TABLE author_facts (author_id INTEGER, author_name VARCHAR(255), num_articles INTEGER, num_coauthors INTEGER);")

# Fetch the author fact data from the temporary table in SQLite in smaller chunks
limit <- 1000
offset <- 0
rows_inserted <- 0

repeat {
  # Retrieve a chunk of data from the temporary table in SQLite
  author_facts_chunk <- dbGetQuery(source_con, sprintf("SELECT * FROM author_facts_temp LIMIT %d OFFSET %d", limit, offset))
  
  # If the chunk is empty, break the loop
  if (nrow(author_facts_chunk) == 0) {
    break
  }
  
  # Insert the chunk into the author_facts table in MySQL
  dbWriteTable(mysql_con, "author_facts", author_facts_chunk, append = TRUE, row.names = FALSE)
  
  # Update the number of rows inserted and the offset for the next chunk
  rows_inserted <- rows_inserted + nrow(author_facts_chunk)
  offset <- rows_inserted
}

# Drop the temporary table in SQLite
dbExecute(source_con, "DROP TABLE author_facts_temp;")

# Close the SQLite and MySQL connections
dbDisconnect(source_con)
dbDisconnect(mysql_con)






# Load necessary packages
library(RMySQL)
library(DBI)
library(RSQLite)

# Connect to the MySQL database
con_mysql <- dbConnect(RMySQL::MySQL(), 
                       dbname = "practicum2db",
                       host = "localhost",
                       user = "root",
                       password = "Zzw@1299123")

# Connect to the SQLite database
con_sqlite <- dbConnect(RSQLite::SQLite(), "pubmed.db")

# Drop tables if they already exist in MySQL
table_names <- c("author_fact", "journal_fact")
for (table_name in table_names) {
  if (dbExistsTable(con_mysql, table_name)) {
    dbExecute(con_mysql, paste0("DROP TABLE ", table_name, ";"))
  }
}

# Create and populate the star schema for author facts
# Create the author_fact table
dbExecute(con_mysql, "CREATE TABLE IF NOT EXISTS author_fact (
                       author_id INTEGER,
                       author_name TEXT,
                       num_articles INTEGER,
                       num_coauthors INTEGER,
                       PRIMARY KEY(author_id)
                      );")

# Insert author_fact_data into the MySQL author_fact table (using SQL)
dbExecute(con_mysql, "TRUNCATE TABLE author_fact;")
dbExecute(con_mysql, "INSERT INTO author_fact (author_id, author_name, num_articles, num_coauthors)
                      SELECT a.id AS author_id,
                             a.last_name || ', ' || a.fore_name AS author_name,
                             COUNT(aa.article_id) AS num_articles,
                             SUM((SELECT COUNT(*) - 1
                                  FROM article_authors
                                  WHERE article_id = aa.article_id
                                 )) AS num_coauthors
                      FROM SQLITE_DB.authors a
                      JOIN SQLITE_DB.article_authors aa ON a.id = aa.author_id
                      GROUP BY a.id;", params = list(SQLITE_DB = dbQuoteIdentifier(con_mysql, "pubmed.db")))



# Create and populate the star schema for journal facts
# Create the journal_fact table
dbExecute(con_mysql, "CREATE TABLE IF NOT EXISTS journal_fact (
                       journal_name TEXT,
                       year INTEGER,
                       quarter INTEGER,
                       month INTEGER,
                       num_articles INTEGER
                      );")

# Insert journal_fact_data into the MySQL journal_fact table (using SQL)
dbExecute(con_mysql, "TRUNCATE TABLE journal_fact;")
dbExecute(con_mysql, "INSERT INTO journal_fact (journal_name, year, quarter, month, num_articles)
                      SELECT j.title AS journal_name,
                             STRFTIME('%Y', j.pub_date) AS year,
                             STRFTIME('%m', j.pub_date) - 1 DIV 3 + 1 AS quarter,
                             STRFTIME('%m', j.pub_date) AS month,
                             COUNT(a.id) AS num_articles
                      FROM SQLITE_DB.journals j
                      JOIN SQLITE_DB.articles a ON j.id = a.journal_id
                      GROUP BY j.title, year, quarter, month;", params = list(SQLITE_DB = dbQuoteIdentifier(con_mysql, "pubmed.db")))


# Close the MySQL and SQLite connections
dbDisconnect(con_mysql)
dbDisconnect(con_sqlite)









# to help extract journal issue - date
extract_journal_issue <- function(journal_issue_node) {
  volume <- as.integer(xmlValue(journal_issue_node[["Volume"]]))
  issue <- as.integer(xmlValue(journal_issue_node[["Issue"]]))
  pub_date_node <- journal_issue_node[["PubDate"]]
  
  year <- as.integer(xmlValue(pub_date_node[["Year"]]))
  
  # Use "01" if the month is missing
  month_text <- as.character(xmlValue(pub_date_node[["Month"]]))
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

