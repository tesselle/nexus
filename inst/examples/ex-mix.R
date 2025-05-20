\donttest{
## Can Sora datasets
## Data from Cau (1999) and Cau et al. (2007)
path_chem <- system.file("extdata", "cansora_chemistry.csv", package = "nexus")
chemistry <- read.csv(path_chem, header = TRUE, row.names = 1)
path_petro <- system.file("extdata", "cansora_petrography.csv", package = "nexus")
petrography <- read.csv(path_petro, header = TRUE, row.names = 1)

## Prepare chemical data
major <- c("Fe2O3", "Al2O3", "MnO", "P2O5", "TiO2",
           "MgO", "CaO", "Na2O", "K2O", "SiO2")
chem <- chemistry[-1, major]

## Prepare petrographic data
petro <- petrography[-c(7, 8), -1]
petro <- cdt(petro) # Get the complete disjunctive table

## First approach
mix1 <- mix(as.matrix(chem), as.matrix(petro), lambda = 2)
mds1 <- pcoa(mix1) # Multi-Dimensional Scaling
plot(mds1)
}
