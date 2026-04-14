library(mongolite)
library(jsonlite)

host <- Sys.getenv("MONGO_HOST")
user <- Sys.getenv("MONGO_USER")
pass <- Sys.getenv("MONGO_PASS")
db_name <- "cybersecurity"
collection_name <- "articles"

url <- sprintf("mongodb://%s:%s@%s:27017/%s?authSource=admin", 
               user, pass, host, db_name)

con <- mongo(
  collection = collection_name,
  db = db_name,
  url = url
)

upload_cyber_data <- function(file_path) {
  data <- jsonlite::read_json(file_path)
  # data$upload_date <- Sys.time()
  
  con$insert(data)
  message(paste("Файл", file_path, "успешно загружен"))
}

con$insert('{"test": "connection successful", "project": "cybersecurity_r"}')

print(con$find())