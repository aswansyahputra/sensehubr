library(SensoMineR)
library(readr)
library(janitor)
library(usethis)

perfume_qda_consumers <- read_csv("data-raw/perfumes_qda_consumers.csv") %>% clean_names()
perfume_qda_consumers
use_data(perfume_qda_consumers, overwrite = TRUE)

perfume_qda_experts <- read_csv("data-raw/perfumes_qda_experts.csv") %>% clean_names()
perfume_qda_experts
use_data(perfume_qda_experts, overwrite = TRUE)

data(chocolates)
sensochoc %>% 
  clean_names() %>% 
  mutate(
    panelist = paste0("id_", stringr::str_pad(panelist, width = 3, pad = "0")),
    session = paste0("session_", stringr::str_pad(session, width = 2, pad = "0")),
    rank = paste0("order_", stringr::str_pad(rank, width = 2, pad = "0"))
  ) %>% 
  write_csv("data-raw/sensochoc.csv")
sensochoc <- read_csv("data-raw/sensochoc.csv")
sensochoc
use_data(sensochoc, overwrite = TRUE)


perfume_jar <- read_csv("data-raw/perfume_jar") %>% clean_names()
perfume_jar
use_data(perfume_jar, overwrite = TRUE)
