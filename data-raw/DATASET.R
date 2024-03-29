## code to prepare datasets goes here
arctic <- read.csv("data-raw/arctic.csv", header = TRUE, row.names = 1)
usethis::use_data(arctic, overwrite = FALSE)

hongite <- read.csv("data-raw/hongite.csv", header = TRUE, row.names = 1)
usethis::use_data(hongite, overwrite = FALSE)

slides <- read.csv("data-raw/slides.csv", header = TRUE, row.names = NULL)
usethis::use_data(slides, overwrite = FALSE)

## Can Sora datasets
chemistry <- read.csv("data-raw/cansora_chemistry.csv", header = TRUE, row.names = 1)
usethis::use_data(chemistry, overwrite = FALSE)

petrography <- read.csv("data-raw/cansora_petrography.csv", header = TRUE, row.names = 1)
usethis::use_data(petrography, overwrite = FALSE)
