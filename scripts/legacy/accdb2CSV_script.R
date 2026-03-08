# Load required packages
install.packages("odbc")
library(odbc)
install.packages("DBI")
library(DBI)

# Connect to the Access database
con <- dbConnect(odbc::odbc(),
                 .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=h:\\Mi unidad\\Tesis\\Datos con R\\SIAE 2010-2020\\2021\\2021 SIAE anonimizada a 01-02-2023.accdb;")
# Get the list of tables in the database
tables <- dbListTables(con)

# Filter out system tables (these typically start with "MSys")
user_tables <- tables[!grepl("^MSys", tables)]

# Export each user-defined table to a CSV file with semicolon separator
for (table_name in user_tables) {
  # Read the table into a data frame
  data <- dbReadTable(con, table_name)
  
  # Create a file name for the CSV (e.g., "table_name.csv")
  file_name <- paste0("h:\\Mi unidad\\Tesis\\Datos con R\\SIAE 2010-2020\\2021\\", table_name, ".csv")
  
  # Write the data frame to CSV with semicolon separator using write.table
  write.table(data, file_name, row.names = FALSE, sep = ";", dec = ".", col.names = TRUE)
}

# Close the connection
dbDisconnect(con)
