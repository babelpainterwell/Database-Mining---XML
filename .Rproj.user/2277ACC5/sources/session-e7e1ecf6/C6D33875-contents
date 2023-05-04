# Title: Practicum II Part 2
# Author: Zhongwei Zhang
# Date: April 18, 2023
# Course: CS5200


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

# Get author fact data from SQLite
author_fact_data <- dbGetQuery(con_sqlite, "
  SELECT a.id AS author_id,
         a.last_name || ', ' || a.fore_name AS author_name,
         COUNT(aa.article_id) AS num_articles,
         SUM((SELECT COUNT(*) - 1
              FROM article_authors
              WHERE article_id = aa.article_id
             )) AS num_coauthors
  FROM authors a
  JOIN article_authors aa ON a.id = aa.author_id
  GROUP BY a.id;")

# Insert author_fact_data into the MySQL author_fact table
dbWriteTable(con_mysql, "author_fact", author_fact_data, append = FALSE, overwrite=TRUE, row.names = FALSE)


# Create and populate the star schema for journal facts
# Create the journal_fact table
dbExecute(con_mysql, "CREATE TABLE IF NOT EXISTS journal_fact (
                       journal_name TEXT,
                       year INTEGER,
                       quarter INTEGER,
                       month INTEGER,
                       num_articles INTEGER
                      );")

# Get journal fact data from SQLite
journal_fact_data <- dbGetQuery(con_sqlite, "
  SELECT j.title AS journal_name,
         STRFTIME('%Y', j.pub_date) AS year,
         (STRFTIME('%m', j.pub_date) - 1) / 3 + 1 AS quarter,
         STRFTIME('%m', j.pub_date) AS month,
         COUNT(a.id) AS num_articles
  FROM journals j
  JOIN articles a ON j.id = a.journal_id
  GROUP BY j.title, year, quarter, month;")

# Insert journal_fact_data into the MySQL journal_fact table
dbWriteTable(con_mysql, "journal_fact", journal_fact_data, append = FALSE, overwrite=TRUE, row.names = FALSE)


# Close the MySQL and SQLite connections
dbDisconnect(con_mysql)
dbDisconnect(con_sqlite)
