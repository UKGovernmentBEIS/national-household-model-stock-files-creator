## Get the external wall construction type column for the English stock.

source("constants.R");
library(plyr);

england.externalwallconstructiontype <- function() {

    ## Load elevations data from the English stock.
    england.data.elevations <- england.load("elevations.csv");
    
    ## Get externalwallconstructiontype from English data.
    externalElevations <- england.data.elevations[
        ## Exclude party walls.
        england.data.elevations[["tenthsattached"]] < 10,
        ## Only include relevant columns.
        c(ehs.idCol, "externalwallconstructiontype")
    ];

    return(
        ddply(
            externalElevations,
            ## Group by aacode
            c(ehs.idCol),
            function(elevations) {
                first <- elevations[1, "externalwallconstructiontype"];

                ## External consturction type must be the same for all walls of the dwelling.
                if (!all(
                         elevations[["externalwallconstructiontype"]] == first
                     )) {
                    stop("Dwelling has multiple different wall construction types", elevations[1, ehs.idCol]);
                }

                ## Return the construction type.
                return(c(externalwallconstructiontype = first));
            }
        )
    );
};
