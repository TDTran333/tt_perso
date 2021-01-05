library(tidyverse)
library(janitor)

download_pbp <- function(year) {
  temp <- tempfile()
  download.file(paste0("http://peter-tanner.com/moneypuck/downloads/shots_", year, ".zip"), temp)
  shots <- read_csv(unz(temp, paste0("shots_", year, ".csv")))
  unlink(temp)
  return(shots)
}

shots <- map_dfr(2017:2019, download_pbp) %>% 
  clean_names

saveRDS(shots, here::here("random", "shots_2017_2019.RDS"))
