#!/usr/bin/env Rscript

library(devtools)

devtools::load_all(".")

args <- commandArgs(trailingOnly=TRUE)

json_file_path <- args[1]
cat("Reading JSON file: ", json_file_path)

tte_arguments    <- rjson::fromJSON(file = json_file_path)
output_directory <- file.path(getwd(), "tmp")
output_path      <- paste0(file.path(output_directory, "test_output"))
csv_path         <- paste0(output_path)

if (file.exists(csv_path)) {
  query_path <- paste0(output_path, ".sql")

  file.remove(csv_path)
  file.remove(query_path)
}

cat("Generating SQL query from JSON arguments")

tte_retriever <- TTERetriever$new(output_directory, "localhost", "postgres", "devpass")

cat("Executing query")

tte_retriever$run("test_output", tte_arguments)

cat("All done!")
