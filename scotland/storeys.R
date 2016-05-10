#' ---
#' title: "Storeys functions"
#' author: "Centre for Sustainable Energy"
#' date: "2015"
#' output: pdf_document
#' ---

#+ echo=FALSE
#Sources (makes available) the common functions required to make the stock.
source("common.R", chdir=T)

#'##Make and save cases data
#'
#'Create a .csv file using the function make.storeys, which creates a dataframe 
#'containing a complete set of populated variables for the storeys.csv stock file.
#'
#'The storeys file contains a set of information on each habitable storey in each 
#'dwelling, including the location of each storey and its' dimensions.
#'
#'@param shcs - the scottish survey data
#' 
#'@param output - the path to the output file including the required file name and
#' the extension .csv
save.storeys <- function(shcs, output) {
  write.csv(make.storeys(shcs), file=output, row.names=FALSE,na = "NULL")
}

#'Make the dataframe that contains all the information required for the storeys.csv
#' file
#'
#'The dataframe is constructed and returned (as it is the last thing in the function
#' that is assigned)
#' 
#'@param shcs - the scottish survey data
make.storeys <- function(shcs) {
  #First all possible storeys are made (1 to 5) from the survey, if a floor does
  #not exist then all the information about that storey will be zero. These storeys
  #are then filtered and storeys are assigned the actual name of the storey.
  #Any additional storeys (i.e. basements) are added.
  #Finally the story height is made by adding 0.25 to the ceiling height of all
  #floors (except basement and ground floor) following SAP methodology.
  storeys <- make.polypointsandceilingheights(shcs) 
  storeys <- assigning.polypointsandceilingheights(shcs,storeys)
  storeys <- make.storeyheight(storeys)
  
  storeys <- data.frame(aacode = storeys$uprn_new
             ,ceilingheight = storeys$ceilingheight
             ,type = storeys$type
             ,polygonxpoints = storeys$polygonxpoints
             ,polygonypoints = storeys$polygonypoints
             ,polypoints = storeys$polypoints
             ,storyheight = storeys$storyheight)
             
   #additional removal of anyfloors that don;t exist
   storeys <- subset(storeys, ceilingheight != 0, drop = FALSE)

  return(storeys)
}

#' \pagebreak
#' 
#'##Create storeys geometry
#' 
#'Function creates storeys geometries
#'
#'All geometry (polygon points and ceiling heights) are created for all storeys 
#' available in the scottish survey. There is floor area and ceiling height available
#' for the 'ground floor/lowest floor', 'first floor', 'second floor', 'third floor 
#' and above', 'room in roof' and 'extension 1' and 'extension 2'. These are used to 
#' create the coordinates of a polygon which describes the floor plan of each storey.
#' 
#'There is no information available in the survey about the location of extension 1
#' or extension 2. It has been assumed that extension 1 is always located at the 
#' front of the building at level one, as this is always an exposed unattached face.
#' Extension 2, if it exists in a dwelling, has also been located on at the front of 
#' the building but at level two unless there isn't a level two, in which case
#' it has been located at the back of level one (all buildings to which this applies
#' ~200 have unattached back elevations).
#' 
#'@param shcs - the scottish survey data
make.polypointsandceilingheights <- function(shcs){
  #Select columns of interest (area and height for all floor areas)
  storeys <- subset(shcs,select = c(uprn_new
                                    ,N1_A,N1_C
                                    ,N2_A,N2_C
                                    ,N3_A,N3_C
                                    ,N4_A,N4_C
                                    ,N5_A,N5_C
                                    ,N6_A,N6_C
                                    ,N7_A,N7_C))
  storeys <- gather(storeys,key,value,-uprn_new)
  storeys <- separate(storeys,variable,into = c("floor","dimension"),sep="_")
  storeys <- spread(storeys,dimension,value)
  
  #If data is not applicable or unobtainable then set dimensions to zero
  storeys$A[storeys$A== 888 | storeys$A == 999 | is.na(storeys$A) == "TRUE"] <- 0
  storeys$C[storeys$C == 8.8 | storeys$C == 9.9| is.na(storeys$C) == "TRUE"] <- 0
  
  #To calculate the perimeter from the area the ratio of length to width must be set
  #This value has been based on common ratios of length to width in houses (i.e the 
  #length of a dwelling is 50% greater than the width, this is a commonly used ratio)
  dimension.ratio <- 1.5
  storeys$mainlength <- sqrt(dimension.ratio*storeys$A)
  storeys$mainwidth <- sqrt(storeys$A/dimension.ratio)
  
  #The dataframe is reorganised to match extension 1 with level 1 and extension 2
  #with level 2 as no data is available as to which floor each extension is on
  floors <- subset(storeys, floor != "N6" & floor != "N7",select=c(uprn_new,floor
                                                                   ,C,mainlength
                                                                   ,mainwidth))
  colnames(floors)[names(floors) == "C"] <- "mainheight"
  floors$floor <- as.factor(checked.revalue(
    as.factor(floors$floor),c("N1" = "1"
                              ,"N2" = "2"
                              ,"N3" = "3"
                              ,"N4" = "4"
                              ,"N5" = "5")))
  extension <- subset(storeys, floor == "N6" | floor == "N7",select=c(uprn_new,floor
                                                                      ,C,mainlength
                                                                      ,mainwidth))
  extension$floor <- as.factor(checked.revalue(
    as.factor(extension$floor),c("N6" = "1"
                                 ,"N7" = "2")))
  colnames(extension)[names(extension) == "mainlength"] <- "frontextlength"
  colnames(extension)[names(extension) == "mainwidth"] <- "frontextwidth"
  colnames(extension)[names(extension) == "C"] <- "frontextheight"
  storeys <- join(floors,extension,by=c("uprn_new","floor"))
  storeys[is.na(storeys)] <- 0
  
  #Some (~200 cases) have an extension 2 but no level 2, these cases are assumed to 
  #have a second extension at the rear of the house (as all dwellings this applies 
  #to are dwellings with an unattached back face)
  backextension <- subset(storeys,storeys$frontextheight > 0 
                          & storeys$mainheight == 0,
                          select = c(uprn_new, floor, frontextlength, frontextwidth
                                     , frontextheight))
  colnames(backextension)[names(backextension) == "frontextlength"] <- 
    "backextlength"
  colnames(backextension)[names(backextension) == "frontextwidth"] <- 
    "backextwidth"
  colnames(backextension)[names(backextension) == "frontextheight"] <- 
    "backextheight"
  backextension$floor <- 1
  
  storeys$frontextlength[storeys$frontextheight > 0 & storeys$mainheight == 0] <- 0
  storeys$frontextwidth[storeys$frontextheight > 0 & storeys$mainheight == 0] <- 0
  storeys$frontextheight[storeys$frontextheight > 0 & storeys$mainheight == 0] <- 0

  storeys <- join(storeys,backextension, by = c("uprn_new","floor"))
  storeys[is.na(storeys)] <- 0
  
  #Convert units of dimensions from m into mm (multiply by 100)
  storeys$mainwidth <- storeys$mainwidth  * 100
  storeys$mainlength <- storeys$mainlength  * 100
  storeys$frontextwidth <- storeys$frontextwidth  * 100
  storeys$frontextlength <- storeys$frontextlength  * 100
  storeys$backextwidth <- storeys$backextwidth  * 100
  storeys$backextlength <- storeys$backextlength  * 100
  
  #Polypoints
  #polypoints are a series of corresponding x and y coordinates on a cartesian plane
  #that detail the layout of each storey, and 'draw' the shape of each storey. 
  #Dwellings with no externsions have just five sets polypoints as the storey is a 
  #rectagular shape (points 1 and 5 have the same locations in order to complete the 
  #shape); dwellings with one extension have seven sets of polypoints and dwellings 
  #with two extensions on the same storey have nine sets of polypoints.
  #(dimensions in mm). 
    storeys$polygonxpoints <- paste("{",round(0,0)
                                  ,",",round(0,0)
                                  ,",",round(storeys$backextwidth,0)
                                  ,",",round(storeys$backextwidth,0)
                                  ,",",round(storeys$mainwidth,0)
                                  ,",",round(storeys$mainwidth,0)
                                  ,",",round(storeys$mainwidth
                                             -storeys$frontextwidth,0)
                                  ,",",round(storeys$mainwidth
                                             -storeys$frontextwidth,0)
                                  ,",",round(0,0)
                                  ,"}",sep="")
  
  
  storeys$polygonypoints <- paste("{",round(0,0)
                                  ,",",round(storeys$mainlength
                                             +storeys$backextlength,0)
                                  ,",",round(storeys$mainlength
                                             +storeys$backextlength,0)
                                  ,",",round(storeys$mainlength,0)
                                  ,",",round(storeys$mainlength,0)
                                  ,",",round(-storeys$frontextlength,0)
                                  ,",",round(-storeys$frontextlength,0)
                                  ,",",round(0,0)
                                  ,",",round(0,0)
                                  ,"}",sep="")
  storeys$polypoints <- 9
  storeys$ceilingheight <- storeys$mainheight
  
  storeys <- subset(storeys,select=c(uprn_new,floor,polygonxpoints
                                     ,polygonypoints,polypoints,ceilingheight))

  return (storeys)
}

#' \pagebreak
#' 
#'##Assign storeys geometry
#' 
#'Function assigns the storeys geometry created previously, the correct label for the
#' type of storey (i.e. level 1 becomes ground for houses, but may become any of 
#' ground, higher or top_floor for flat).
#' 
#'In addition, if column J4 in the scottish survey (rooms in basement) indicates
#' the existence of a basement, one is created by replicating the floor plan of the 
#' ground floor. If column J2 (habitable rooms excluding room in roof) indicates that 
#' there are storeys higher than the second floor the level 3 and above floor plan
#' is replicated as required.  
#'
#'@param shcs - the scottish survey data
#'
#'@param storeys - the storey geometry created by the 
#' make.polypointsandceilingheights function
assigning.polypointsandceilingheights <- function(shcs,storeys){
  
  #Remove floors that don't physically exist
  floortype <- subset(storeys,ceilingheight != 0.0,select=c(uprn_new
                                                            ,floor,ceilingheight)) 
  floortype <- spread(floortype,floor,ceilingheight)

  additional.info <- subset(shcs,select = c(uprn_new,J2,J4,E5,E6,C2))
  floortype <- join(floortype,additional.info,by="uprn_new")
  
  #If level 5 and not flat then room in roof
  floortype$lvl.5[is.na(floortype[6]) == FALSE 
                  & floortype$C2 == "Not flat"] <- "room_in_roof" 
  #if level 5 and flat then top floor
  floortype$lvl.5[is.na(floortype[6]) == FALSE 
                  & floortype$C2 != "Not flat"] <- "top_floor" 
  
  # if level 1 and either not flat or exposed floor then ground
  floortype$lvl.1[is.na(floortype[2]) == FALSE 
                  & (floortype$C2 == "Not flat" | (floortype$E5 == "Ground floor"
                                        |floortype$E5 == "Exposed")) ] <- "ground" 
  # if level 1 and flat and exposed roof but no room in roof then top floor
  floortype$lvl.1[is.na(floortype[2]) == FALSE & is.na(floortype$lvl.1) == TRUE
                  & floortype$C2 != "Not flat" & floortype$J2 <= 1
                  & floortype$lvl.5 != "top_floor"
                  & (floortype$E6 == "Pitched roof" |
                       floortype$E6 == "Flat roof")] <- "top_floor"
  #if level 1 exists and flats but not yet filled in then first floor
  floortype$lvl.1[is.na(floortype[2]) == FALSE & is.na(floortype$lvl.1) == TRUE
                  & floortype$C2 != "Not flat"] <- "first_floor" 
  
  #if level 2 exists and not a flat then first floor
  floortype$lvl.2[is.na(floortype[3]) == FALSE 
                  & floortype$C2 == "Not flat"] <- "first_floor"
  #if level 2 exists, flat and exposed roof and no room in roof then top floor
  floortype$lvl.2[is.na(floortype[3]) == FALSE 
                  & floortype$C2 != "Not flat"
                  & (floortype$E6 == "Pitched roof" | floortype$E6 == "Flat roof")
                  & is.na(floortype$lvl.5) == TRUE] <- "top_floor"
  #if level 2 exists, flat and not yet filled in then higher_floor 
  floortype$lvl.2[is.na(floortype[3]) == FALSE & is.na(floortype$lvl.2) == TRUE
                  & floortype$C2 != "Not flat"] <- "higher_floor"
  
  #If level 3 and 4 exist then assign correct floor type
  #No flats in the survey have more than 2 floors
  floortype$lvl.3[is.na(floortype[4]) == FALSE 
                  & floortype$C2 == "Not flat"] <- "second_floor"
  #3 cases with 4 floors, no cases with more floors
  floortype$lvl.4[is.na(floortype[4]) == FALSE 
                  & floortype$C2 == "Not flat"] <- "higher_floor" 
  
  #Transform dataframe to correct format, join to the storeys dataframe
  #and remove any floors that don't exist
  floortype <- subset(floortype,select = c(uprn_new,J4,lvl.1,lvl.2,lvl.3
                                           ,lvl.4,lvl.5))
  floortype <- gather(floortype,key,type,-uprn_new, -J4)
  floortype <- separate(floortype,variable,into=c("key","floor"))
  names(floortype) [5] <- "type"
  
  floortype <- subset(floortype,select = c(uprn_new,floor,type,J4))
  floortype <- join(floortype,storeys,by=c("uprn_new","floor"))
  floortype <- subset(floortype,is.na(floortype$type)==FALSE)
  
  #If there are rooms in the basement then a basement is created from the
  #ground floor plan
  basements <- subset(floortype,floortype$type == "ground" 
                      & floortype$J4 >= 1 & floortype$J4 <88)
  basements$floor <- 0
  basements$type <- "basement"
  
  #Basements are then joined into the floortype again
  floortype <- rbind(floortype,basements)
  
  return(floortype)
}

#' \pagebreak
#' 
#'##Storey height
#'
#'Make storey height
#'
#'Storey heights are made by adding 0.25 to the ceiling height, unless the storey
#'is the ground or basement, as the lowest storey height will also be the ceiling 
#'height. 0.25m is the RD SAP dimension given for the thickness of internal ceilings/
#'floors to add to additional storeys (see Appendix S RD SAP 9.91 section S3.5).
#' 
#'@param storeys - the storey geometry dataframe created from the scottish survey
#' data in the previous two functions
make.storeyheight <- function(storeys){
  storeys$storyheight <- storeys$ceilingheight
  storeys$storyheight[storeys$type != "ground" & storeys$type != "basement"] <-
    storeys$ceilingheight[storeys$type != "ground" & storeys$type != "basement"] + 0.25
  return(storeys)
}
