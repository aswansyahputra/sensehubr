library(readr)
library(janitor)
library(usethis)

perfume_qda_consumers <- read_csv("data-raw/perfumes_qda_consumers.csv") %>% clean_names()
perfume_qda_consumers
use_data(perfume_qda_consumers, overwrite = TRUE)

perfume_qda_experts <- read_csv("data-raw/perfumes_qda_experts.csv") %>% clean_names()
perfume_qda_experts
use_data(perfume_qda_experts, overwrite = TRUE)
