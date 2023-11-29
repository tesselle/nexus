\donttest{
## Prepare chemical data
data("chemistry")
major <- c("Fe2O3", "Al2O3", "MnO", "P2O5", "TiO2", "MgO", "CaO", "Na2O", "K2O", "SiO2")
chem <- chemistry[-1, major]

## Prepare petrographic data
data("petrography")
petro <- petrography[-c(7, 8), -1]
petro <- cdt(petro) # Get the complete disjunctive table

## First approach
mix1 <- mix(as.matrix(chem), as.matrix(petro), lambda = 2)
mds1 <- stats::cmdscale(mix1) # Multi-Dimensional Scaling
plot(mds1)
}
