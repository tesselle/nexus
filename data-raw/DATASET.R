## Data from Aitchison (1986)
hongite <- read.csv("data-raw/hongite.csv", header = TRUE, row.names = 1)
usethis::use_data(hongite, overwrite = FALSE)

kongite <- read.csv("data-raw/kongite.csv", header = TRUE, row.names = 1)
usethis::use_data(kongite, overwrite = FALSE)

boxite <- read.csv("data-raw/boxite.csv", header = TRUE, row.names = 1)
usethis::use_data(boxite, overwrite = FALSE)

coxite <- read.csv("data-raw/coxite.csv", header = TRUE, row.names = 1)
usethis::use_data(coxite, overwrite = FALSE)

arctic <- read.csv("data-raw/arctic.csv", header = TRUE, row.names = 1)
usethis::use_data(arctic, overwrite = FALSE)

lava <- read.csv("data-raw/lava.csv", header = TRUE, row.names = 1)
usethis::use_data(lava, overwrite = FALSE)

predator <- read.csv("data-raw/predator.csv", header = TRUE, row.names = 1)
usethis::use_data(predator, overwrite = FALSE)

slides <- read.csv("data-raw/slides.csv", header = TRUE, row.names = NULL)
usethis::use_data(slides, overwrite = FALSE)
