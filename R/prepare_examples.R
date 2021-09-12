
#' @title Prepare examples
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description This is a function that read a google sheet file and prepare
#' other data files into the project directory folder called data in order to
#' be used as examples on how to read data into R
#' 
prepare_examples <- function() {
  
  if(!fs::dir_exists("data")) {
    fs::dir_create("data")
  }
  
  if(!file_exists("data/tropi_dry_principe_tower.csv")) {
    principe_drive <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1e6_LeSP9LjDBFSKvcCbM3yoUhLOCSK2_SqmkoAl7nzo/edit?usp=sharing") %>% 
      readr::write_csv("data/tropi_dry_principe_tower.csv")
  }
  
  principe_drive %>% 
    janitor::clean_names() %>% 
    tidyr::separate(col = date_time, into = c("date", "time"), sep = " ") %>% 
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::mutate(year = lubridate::year(date)) %>% 
    tidyr::nest(-year) %>% 
    purrr::pwalk(function(year, data) 
      readr::write_csv(data, 
                       file.path("data/",
                                 paste0("principe_",year, ".csv")
                       )
      )
    )
}