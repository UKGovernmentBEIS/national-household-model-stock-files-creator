source("constants.R");

## Distribute Welsh weights over the English rows
wales.distributeWeights <- function(englishSubset, welshWeight) {
    if (nrow(englishSubset) == 0) {
        stop("No english rows matched for row.");

    } else if (nrow(englishSubset) == 1) {
        englishSubset[[ehs.weightCol]] <- welshWeight;
        
    } else {
        ## Normalize and distribute the weights.
        englishWeights = sum(englishSubset[,ehs.weightCol]);

        weightMultiplier = welshWeight/englishWeights;

        englishSubset[ehs.weightCol] <- englishSubset[,ehs.weightCol] * weightMultiplier;
    }

    return(
        englishSubset[,c(ehs.idCol, ehs.weightCol)]
    );
}
