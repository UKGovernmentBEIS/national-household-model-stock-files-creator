#' ---
#' title: "Elevations functions"
#' author: "Centre for Sustainable Energy"
#' date: "2015"
#' output: pdf_document
#' ---

#+ echo=FALSE
#Sources (makes available) the common functions required to make the stock.
source("common.R", chdir=T)

#'##Make and save elevations data
#'
#'Create a .csv file using the function make.elevations, which creates a dataframe 
#'containing a complete set of populated variables for the elevations.csv stock 
#'file.
#'
#'The elevations file contains four rows for each dwelling case, one for each 
#'elevation of a dwelling (front, back, left and right). The information in the 
#'survey is used to determine which of these elevations are attached to party walls, 
#'the construction types of the unattached walls and levels of insulation on each 
#'elevation. Also included in data on types of window (glazing and frame type) and 
#'what proportion of each elevation is windows, and how many and the type of doors 
#'present in each elevation.
#'
#'Each elevation can have only one wall type and can only have full or no insulation 
#'but multiple types of insulation are allowed (i.e. an external wall on an elevation
#'could have both external and internal wall insulation present).
#'
#' Attachments and proportion of windows are expressed in tenths. 
#'
#' @param shcs - the scottish survey data
#' 
#' @param output - the path to the output file including the required file name and
#' the extension .csv
save.elevations <- function(shcs, output) {
  write.csv(make.elevations(shcs), file=output, row.names=FALSE,na = "")
}

#'Make the dataframe that contains all the information required for the 
#' elevations.csv file
#'
#'The dataframe is constructed and returned (as it is the last thing in the function
#' that is assigned)
#' 
#'@param shcs - the scottish survey data
make.elevations <- function(shcs) {
  #The tenths attached for each wall (front,left,back,right) are created.
  elevations <- make.tenthsattached(shcs)
  
  #The construction type of the primary and secondary walls are created, followed by
  #creation of the existance of wall insulation (cavity,internal,external).
  #These wall types and insulations are then assigned to each of the elevations based
  #on the physical presence of an external wall calculated from the tenthsattached
  #columns.
  elevations <- make.externalwallconstructiontype(elevations)
  elevations <- make.wallinsulation(elevations)
  elevations <- assigning.walltypeandinsulation(elevations)
  
  #The tenths openings are calculated in the make.tenthsopening function
  elevations <- make.tenthsopening(elevations)
  
  #The type of window frame for each case are made and these are then assigned to
  #each elevation. The number of each type of doorframe on each elevation are then
  #assigned.
  elevations <- make.windowframes(elevations)
  elevations <- assigning.windowframes(elevations)
  elevations <- assigning.doorframes(elevations)
  
  #The dataframe created by the functions above is then changed into the correct
  #format. The relevent columns are selected and then turned into a long dataframe
  #The key is then split into a description of the value in that row and the
  #elevation to which the value belongs. The dataframe is then changed from this long
  #format into a wide format.
  elevations <- select(elevations,uprn_new,contains("front."),contains("left."),
                       contains("back."),contains("right."))
  elevations <- gather(elevations,key,value,-uprn_new)
  elevations <- separate(elevations,variable,into=c("elevationtype","key"),sep="\\.")
  elevations <- spread(elevations,key,value)
  
  #creates a dataframe containing column names (word before the = sign) and a vector
  #to fill that column (vector after the = sign)
  #Due to the characters (: and ,) that appear in the column names for doorframes the
  #command check.names = FALSE is included to prevent the format of these names being
  #altered
  elevations <- data.frame(aacode = elevations$uprn_new
                 ,cavitywallinsulation = elevations$cavitywallinsulation
                 ,`doorframe:wood,doortype:solid` = 
                   elevations$`doorframe:wood,doortype:solid`
                 ,`doorframe:wood,doortype:glazed` = 
                   elevations$`doorframe:wood,doortype:glazed`
                 ,`doorframe:metal,doortype:solid` = 
                   elevations$`doorframe:metal,doortype:solid`
                 ,`doorframe:metal,doortype:glazed` = 
                   elevations$`doorframe:metal,doortype:glazed`
                 ,`doorframe:upvc,doortype:solid` = 
                   elevations$`doorframe:upvc,doortype:solid`
                 ,`doorframe:upvc,doortype:glazed` = 
                   elevations$`doorframe:upvc,doortype:glazed`
                 ,doubleglazedwindowframe = elevations$doubleglazedwindowframe
                 ,elevationtype = elevations$elevationtype
                 ,externalwallinsulation = elevations$externalwallinsulation
                 ,externalwallconstructiontype = elevations$externalwallconstructiontype
                 ,internalinsulation = elevations$internalinsulation
                 ,percentagedoubleglazed = elevations$percentagedoubleglazed
                 ,singleglazedwindowframe = elevations$singleglazedwindowframe 
                 ,tenthsattached = elevations$tenthsattached
                 ,tenthsopening = elevations$tenthsopening
                 ,tenthspartywall = elevations$tenthsattached
                 ,check.names = FALSE
                 ,angleFromNorth = ""
                 )
  #correct insulation values where no external wall exists
#   elevations$cavitywallinsulation <- ifelse(elevations$tenthsattached == 10 & 
#                                           is.na(elevations$cavitywallinsulation), "",
#                                           elevations$cavitywallinsulation)
#   elevations$externalwallinsulation <- ifelse(elevations$tenthsattached == 10 & 
#                                           is.na(elevations$externalwallinsulation), "",
#                                           elevations$externalwallinsulation)
#   elevations$internalinsulation <- ifelse(elevations$tenthsattached == 10 & 
#                                           is.na(elevations$internalinsulation), "",
#                                           elevations$internalinsulation)
  return(elevations)
}

#'
#' \pagebreak
#' 
#' ##Tenths attached
#'
#'Create the tenths attached for all elevations
#'
#'There is limited information available in the scottish survey to create this.
#' Therefore it is created based on assumptions about the house from it's house type
#' and for flats from the variable E7 which contains the number of walls exposed
#' It is assumed that the front of the house or flat is always exposed and then for
#' flats the walls are assigned clockwise (i.e. front,left,back,right). A flat with
#' three exposed walls would have the front, left and back walls exposed and the
#' right wall would be attached.
#' 
#' Mid-terraced dwellings have the front and back exposed by the left and right face
#' attached, semi-detached and end-terrace houses have only the right face attached 
#' and detached houses have no faces attached
#' 
#'@param stock - this is the whole of the scottish survey
make.tenthsattached <- function(stock){
  #combine housetype information with information about wall exposure in flats using
  #the building type function
  stock$buildinginfo <- building.type(stock$C1,stock$E7) 
  
  stock$front.tenthsattached <- as.factor(checked.revalue(
    stock$buildinginfo,c(
      "Detached" = 0
      ,"End terrace" = 0
      ,"Semi-detached" = 0   
      ,"Mid-terrace with passage" = 0   
      ,"Mid-terrace" = 0   
      ,"Corner / enclosed end" = 0   
      ,"Enclosed mid" = 0
      ,"1 wall exposed" = 0
      ,"1 to 2 walls exposed" = 0
      ,"2 walls exposed" = 0
      ,"2 to 3 walls exposed" = 0
      ,"3 walls exposed" = 0
      ,"3 to 4 walls exposed" = 0
      ,"4 walls exposed" = 0
    )))
  
  stock$left.tenthsattached <- as.factor(checked.revalue(
    stock$buildinginfo,c(
      "Detached" = 0
      ,"End terrace" = 0
      ,"Semi-detached" = 0   
      ,"Mid-terrace with passage" = 0   
      ,"Mid-terrace" = 10   
      ,"Corner / enclosed end" = 0   
      ,"Enclosed mid" = 10
      ,"1 wall exposed" = 10
      ,"1 to 2 walls exposed" = 10
      ,"2 walls exposed" = 10
      ,"2 to 3 walls exposed" = 5 
      ,"3 walls exposed" = 0
      ,"3 to 4 walls exposed" = 0
      ,"4 walls exposed" = 0)))
  
  stock$back.tenthsattached <- as.factor(checked.revalue(
    stock$buildinginfo,c(
      "Detached" = 0
      ,"End terrace" = 0
      ,"Semi-detached" = 0   
      ,"Mid-terrace with passage" = 0   
      ,"Mid-terrace" = 0   
      ,"Corner / enclosed end" = 10   
      ,"Enclosed mid" = 10
      ,"1 wall exposed" = 10
      ,"1 to 2 walls exposed" = 5
      ,"2 walls exposed" = 0
      ,"2 to 3 walls exposed" = 0
      ,"3 walls exposed" = 0
      ,"3 to 4 walls exposed" = 0
      ,"4 walls exposed" = 0)))
  
  stock$right.tenthsattached <- as.factor(checked.revalue(
    stock$buildinginfo,c(
      "Detached" = 0
      ,"End terrace" = 10
      ,"Semi-detached" = 10   
      ,"Mid-terrace with passage" = 10   
      ,"Mid-terrace" = 10   
      ,"Corner / enclosed end" = 10   
      ,"Enclosed mid" = 10
      ,"1 wall exposed" = 10
      ,"1 to 2 walls exposed" = 10
      ,"2 walls exposed" = 10
      ,"2 to 3 walls exposed" = 10
      ,"3 walls exposed" = 10
      ,"3 to 4 walls exposed" = 5
      ,"4 walls exposed" = 0
    )))
  return(stock)
}

#'
#' \pagebreak
#' 
#' ##Wall construction and insulation
#' 
#'Create the primary and secondary wall types and calculate the number walls that
#' should be made out of the primary and secondary wall types
#'
#'The wall type information is stored across several values of the survey data, this
#' is combined where necessary to create a vector which is then mapped to possible
#' wall types for the NHM.
#' 
#' The stock data details two wall types: primary and secondary as well as providing 
#' information on the coverage (in tenths) of the primary wall. Where a secondary 
#' wall type exists, the function below determines the proportion of exposed walls to
#' allocate to the details (construction and insulation) described in the primary 
#' wall variables. The secondary wall details are then used to assign information to 
#' the remaining faces of each case/dwelling.
#' 
#' Any cavity construction type that is built with blockwork or brick is allocated as
#' a 'cavity' wall, which in the NHM covers all masonry cavities. Other cavity walls 
#' constructed of different materials are assigned accordingly and as shown below.
#' 
#' So to calculate the number of primary walls that should exist, the number of 
#' unattached walls is combined with the proportion of walls that are primary walls.
#'
#'@param stock - this is the whole of elevations created by other functions in this
#' script, which is comprised of the scottish survey with additional columns
make.externalwallconstructiontype <- function(stock){
  #Combine secondary wall types recorded across several columns into one column -
  #Q2 is required for wall types of brick or blockwork, Q5 is required for stone
  #walls
  stock$primarywall <- ifelse(stock$Q3 != "Brick" & stock$Q3 != "Blockwork"
                              ,levels(stock$Q3)[stock$Q3]
                              ,paste(stock$Q3,stock$Q2,sep="."))
  stock$primarywall <- as.factor(ifelse(stock$primarywall != "Stone"
                                        ,stock$primarywall
                                        ,paste(stock$primarywall,stock$Q5,sep=".")))
  stock$primarywall <- as.factor(checked.revalue(
    stock$primarywall,c(
      "Blockwork.Cavity" = "cavity"
      ,"Blockwork.Other" = "solidbrick"
      ,"Blockwork.Solid" = "solidbrick"
      ,"Brick.Cavity" = "cavity"
      ,"Brick.Other" = "solidbrick"
      ,"Brick.Solid" = "solidbrick"      
      ,"Clay / earth" = "cob"
      ,"Concrete" = "systembuild"
      ,"Metal" = "metalframe"
      ,"Other" = "systembuild"
      ,"Stone.Granite" = "graniteorwhinstone"
      ,"Stone.Not stone" = "graniteorwhinstone"
      ,"Stone.Other" = "graniteorwhinstone"
      ,"Stone.Sandstone" = "sandstone"
      ,"Stone.Whin" = "graniteorwhinstone"
      ,"Timber" = "timberframe"            
    )))
  
  
  #Combine secondary wall types recorded across several columns into one column -
  #Q11 is required for wall types of brick or blockwork, Q14 is required for stone
  #walls
  stock$secondarywall <- ifelse(stock$Q12 != "Brick" & stock$Q12 != "Blockwork"
                                ,levels(stock$Q12)[stock$Q12]
                                ,paste(stock$Q12,stock$Q11,sep="."))
  stock$secondarywall <- as.factor(ifelse(stock$secondarywall != "Stone"
                                          ,stock$secondarywall
                                          ,paste(stock$secondarywall,stock$Q14
                                                 ,sep=".")))
  #One case has an unobtainable secondary wall type, this has been set to sandstone
  #this is because it is the most common construction type and material in the survey
  stock$secondarywall <- as.factor(checked.revalue(
    stock$secondarywall,c(
      "Blockwork.Cavity" = "cavity"
      ,"Blockwork.Other" = "solidbrick"
      ,"Blockwork.Solid" = "solidbrick"
      ,"Brick.Cavity" = "cavity"
      ,"Brick.Other" = "solidbrick"
      ,"Brick.Solid" = "solidbrick"      
      ,"Brick.Unobtainable" = "solidbrick"
      ,"Clay / earth" = "cob"
      ,"Concrete" = "systembuild"
      ,"Metal" = "metalframe"
      ,"Other" = "systembuild"
      ,"Stone.Granite" = "graniteorwhinstone"
      ,"Stone.Not stone" = "graniteorwhinstone"
      ,"Stone.Other" = "graniteorwhinstone"
      ,"Stone.Sandstone" = "sandstone"
      ,"Stone.Unobtainable" = "graniteorwhinstone"
      ,"Stone.Whin" = "graniteorwhinstone"
      ,"Timber" = "timberframe"            
      ,"Unobtainable" = "sandstone"    
    )))
  
  # This final part of the function calculates the number of elevations of the house 
  # which are of primary wall type, rounded to the nearest whole number and assigns 
  # it to 'numprimarywalls' (Note: each elevation can only be assigned one wall type, 
  # hence the majhority of the front elevations are assigned the primary wall details
  stock$numprimarywalls <-
    round2(((40-
              (as.numeric(levels(stock$front.tenthsattached)[stock$front.tenthsattached])
               +as.numeric(levels(stock$left.tenthsattached)[stock$left.tenthsattached])
               +as.numeric(levels(stock$back.tenthsattached)[stock$back.tenthsattached])
               +as.numeric(levels(stock$right.tenthsattached)[stock$right.tenthsattached])))
           *stock$Q10)/100,0)
  
  return(stock) 
}

#'
#' \pagebreak
#'
#'Create the existance of cavity wall insulation, internal insulation and external
#' insulation
#'
#'This is created for both the primary and secondary walls and some logic is included
#' to read just the survey data where inconsistent (i.e a solid wall with cavity
#' wall insulation)
#'
#'@param stock - this is the whole of elevations created by other functions in this
#' script, which is comprised of the scottish survey with additional columns
make.wallinsulation <- function(stock){
  stock$primary.cavityinsulation <- as.factor(checked.revalue(
    stock$Q8,c(
      "None" = "FALSE"
      ,"cavity" = "TRUE"
      ,"Internal"  = "FALSE"            
      ,"External" = "FALSE"
      ,"cavity and internal" = "TRUE"
      ,"cavity and external" = "TRUE"  
      ,"internal and external" = "FALSE"
      ,"Not applicable" = "FALSE"
      ,"Unobtainable"  = "FALSE"
    )))
  #Only cavity,timberframe,metalframe and systembuild wall types can have cavity wall
  #insulation, so if PRIMARY WALL cases exist where other wall types have cavity wall
  #insulation, cavity wall insualtion is set to FALSE
  stock$primary.cavityinsulation[stock$primary.cavityinsulation == "TRUE" &
                                   stock$primarywall != "cavity" &
                                   stock$primarywall != "timberframe"
                                 & stock$primarywall != "metalframe" &
                                   stock$primarywall != "systembuild" ] <- "FALSE"
  stock$secondary.cavityinsulation <- as.factor(checked.revalue(
    stock$Q17,c(
      "None" = "FALSE"
      ,"cavity" = "TRUE"
      ,"Internal"  = "FALSE"            
      ,"External" = "FALSE"
      ,"cavity and internal" = "TRUE"
      ,"cavity and external" = "TRUE"  
      ,"internal and external" = "FALSE"
      ,"Not applicable" = "FALSE"
      ,"Unobtainable"  = "FALSE"
    )))
  #Only cavity,timberframe,metalframe and systembuild wall types can have cavity wall
  #insulation, so if SECONDARY WALL cases exist where other wall types have cavity 
  #wall insulation, cavity wall insualtion is set to FALSE
  stock$secondary.cavityinsulation[stock$secondary.cavityinsulation == "TRUE" &
                                     stock$secondarywall != "cavity" &
                                     stock$secondarywall != "timberframe"
                                   & stock$secondarywall != "metalframe" &
                                     stock$secondarywall != "systembuild" ] <- "FALSE"
  stock$primary.externalinsulation <- as.factor(checked.revalue(
    stock$Q8,c(
      "None" = "FALSE"
      ,"cavity" = "FALSE"
      ,"Internal"  = "FALSE"            
      ,"External" = "TRUE"
      ,"cavity and internal" = "FALSE"
      ,"cavity and external" = "TRUE"  
      ,"internal and external" = "TRUE"
      ,"Not applicable" = "FALSE"
      ,"Unobtainable"  = "FALSE"
    )))
  stock$secondary.externalinsulation <- as.factor(checked.revalue(
    stock$Q17,c(
      "None" = "FALSE"
      ,"cavity" = "FALSE"
      ,"Internal"  = "FALSE"            
      ,"External" = "TRUE"
      ,"cavity and internal" = "FALSE"
      ,"cavity and external" = "TRUE"  
      ,"internal and external" = "TRUE"
      ,"Not applicable" = "FALSE"
      ,"Unobtainable"  = "FALSE"
    )))
  stock$primary.internalinsulation <- as.factor(checked.revalue(
    stock$Q8,c(
      "None" = "FALSE"
      ,"cavity" = "FALSE"
      ,"Internal"  = "TRUE"            
      ,"External" = "FALSE"
      ,"cavity and internal" = "TRUE"
      ,"cavity and external" = "FALSE"  
      ,"internal and external" = "TRUE"
      ,"Not applicable" = "FALSE"
      ,"Unobtainable"  = "FALSE"
    )))
  stock$secondary.internalinsulation <- as.factor(checked.revalue(
    stock$Q17,c(
      "None" = "FALSE"
      ,"cavity" = "FALSE"
      ,"Internal"  = "TRUE"            
      ,"External" = "FALSE"
      ,"cavity and internal" = "TRUE"
      ,"cavity and external" = "FALSE"  
      ,"internal and external" = "TRUE"
      ,"Not applicable" = "FALSE"
      ,"Unobtainable"  = "FALSE"
    )))
  return(stock)
}

#'
#' \pagebreak
#'
#'Each elevation is assigned the correct wall type and insulation based on the tenths
#' attached and the number of walls that are made from the primary wall type
#'
#'Walls are assigned in the following order: front, left, back, right
#'
#'@param stock - this is the whole of elevations created by other functions in this
#' script, which is comprised of the scottish survey with additional columns
assigning.walltypeandinsulation <- function(stock){
  
  #columns are turned into correct format for next section
  stock$usedwalls <- 0
  stock$front.tenthsattached <- 
    as.numeric(levels(stock$front.tenthsattached)[stock$front.tenthsattached])
  stock$left.tenthsattached <- 
    as.numeric(levels(stock$left.tenthsattached)[stock$left.tenthsattached])
  stock$back.tenthsattached <- 
    as.numeric(levels(stock$back.tenthsattached)[stock$back.tenthsattached])
  stock$right.tenthsattached <- 
    as.numeric(levels(stock$right.tenthsattached)[stock$right.tenthsattached])
  stock$primarywall <- 
    as.character(levels(stock$primarywall)[stock$primarywall])
  stock$secondarywall <- 
    as.character(levels(stock$secondarywall)[stock$secondarywall])
  
  #Assign wall types and insulation to each elevation: if wall is an external wall
  #indicated by tenths attached being less than 10 and the number of walls that have
  #already been assigned as the primary wall type is less than the total number of
  #primary walls then primary wall type is assigned, otherwise secondary wall type
  #is assigned.
  #if wall is not an external wall then no wall type is assigned.
  #Order of wall assignment: front, left, back, right
  #Walls are assigned using the wall.typeandinsulation function
  stock <- wall.typeandinsulation(stock,"front")
  stock <- wall.typeandinsulation(stock,"left")
  stock <- wall.typeandinsulation(stock,"back")
  stock <- wall.typeandinsulation(stock,"right")
  return(stock)
}

#'
#' \pagebreak
#'
#'Each elevation is assigned the correct wall type and insulation based on the tenths
#' attached and the number of walls that are made from the primary wall type
#'
#'A test is carried out to see if the wall exists and if all the primary walls have
#' been used up, this is used to assign the wall type, cavitywall, internal and
#' external insulation details
#'
#'@param stock - this is the whole of elevations created by other functions in this
#' script, which is comprised of the scottish survey with additional columns
#' 
#'@param elevation - a string which contains the type of elevation, i.e. "front"
wall.typeandinsulation <- function(stock,elevation){
  #test to indicate if the primary or secondary wall type should be assigned.
  
  tenths.attached.column <- paste(elevation,"tenthsattached",sep=".")
  has.exposed.wall.in.elevation <-  stock[,tenths.attached.column] < 10
  
  prim.test <- has.exposed.wall.in.elevation & stock$usedwalls < stock$numprimarywalls
  sec.test  <- has.exposed.wall.in.elevation & stock$usedwalls >= stock$numprimarywalls
  
  stock[prim.test, paste(elevation,"externalwallconstructiontype",sep=".")] <-
    stock$primarywall[prim.test]
  stock[sec.test,paste(elevation,"externalwallconstructiontype",sep=".")] <-
    stock$secondarywall[sec.test]
  
  stock[prim.test,paste(elevation,"cavitywallinsulation",sep=".")] <-
    stock$primary.cavityinsulation[prim.test]
  stock[sec.test,paste(elevation,"cavitywallinsulation",sep=".")] <- 
    stock$secondary.cavityinsulation[sec.test]
  
  stock[prim.test,paste(elevation,"externalwallinsulation",sep=".")] <-
    stock$primary.externalinsulation[prim.test]
  stock[sec.test,paste(elevation,"externalwallinsulation",sep=".")] <-
    stock$secondary.externalinsulation[sec.test]
  
  stock[prim.test,paste(elevation,"internalinsulation",sep=".")] <- 
    stock$primary.internalinsulation[prim.test]
  stock[sec.test,paste(elevation,"internalinsulation",sep=".")] <-
    stock$secondary.internalinsulation[sec.test]
  #The number of primary walls used is increased by one each time the function is
  # called if the prim.test is true
  stock$usedwalls[prim.test] <- (stock$usedwalls[prim.test] + 1)
  return(stock)
}

#'
#'\pagebreak
#'
#' ##Tenths opening
#'
#'Create the tenths opening for all elevations
#'
#'There is limited information available in the scottish survey to create this. It
#' has been created by dividing the window area as calculated using the calculations
#' detailed in the RD-SAP methodology (SAP 9.91 Appendix S Table S4: Windowbarea (m?)
#' ) by the total wall area calculated by multipling the external wall perimeter by 
#' the height for all storeys.
#'
#'@param stock - this is the whole of elevations created by other functions in this
#' script, which is comprised of the scottish survey with additional columns
make.tenthsopening <- function(stock){
  #Set all floors sizes that do not exist to a floor size of 0 for summation
  #These should not be altered
  stock$N1_A[stock$N1_A == 888] <- 0
  stock$N2_A[stock$N2_A == 888] <- 0
  stock$N2_A[stock$N2_A == 999] <- 0
  stock$N3_A[stock$N3_A == 888] <- 0
  stock$N4_A[stock$N4_A == 888] <- 0
  stock$N5_A[stock$N5_A == 888] <- 0
  stock$N6_A[stock$N6_A == 888] <- 0
  stock$N7_A[stock$N7_A == 888] <- 0
  
  #Set all heights that do not exist to a heights of 0 for summation
  #These should not be altered
  
  stock$N1_C[stock$N1_C == 8.8 | is.na(stock$N1_C) == "TRUE"] <- 0
  stock$N2_C[stock$N2_C == 8.8 | stock$N2_C == 9.9 |is.na(stock$N2_C) == "TRUE"] <- 0
  stock$N3_C[stock$N3_C == 8.8 | stock$N3_C == 9.9 |is.na(stock$N3_C) == "TRUE"] <- 0
  stock$N4_C[stock$N4_C == 8.8 | stock$N4_C == 9.9 |is.na(stock$N4_C) == "TRUE"] <- 0
  stock$N5_C[stock$N5_C == 8.8 | stock$N5_C == 9.9 |is.na(stock$N5_C) == "TRUE"] <- 0
  stock$N6_C[stock$N6_C == 8.8 | stock$N6_C == 9.9 |is.na(stock$N6_C) == "TRUE"] <- 0
  stock$N7_C[stock$N7_C == 8.8 | stock$N7_C == 9.9 |is.na(stock$N7_C) == "TRUE"] <- 0
  
  #Set all external perimeters that do not exist external perimeters of 0 for 
  #summation
  #These should not be altered
  stock$N1_D[stock$N1_D == 888 | is.na(stock$N1_D) == "TRUE"] <- 0
  stock$N2_D[stock$N2_D == 888 | stock$N2_D == 999 |is.na(stock$N2_D) == "TRUE"] <- 0
  stock$N3_D[stock$N3_D == 888 | stock$N3_D == 999 |is.na(stock$N3_D) == "TRUE"] <- 0
  stock$N4_D[stock$N4_D == 888 | stock$N4_D == 999 |is.na(stock$N4_D) == "TRUE"] <- 0
  stock$N6_D[stock$N6_D == 888 | stock$N6_D == 999 |is.na(stock$N6_D) == "TRUE"] <- 0
  stock$N7_D[stock$N7_D == 888 | stock$N7_D == 999 |is.na(stock$N7_D) == "TRUE"] <- 0
  
  #Calculate the number of floors that the dimensions from the level3 and higher
  #columns need to be replicated for.
  #This is set so that if the number of floors (excluding room in roof) is 3, then
  #the number of times that the level 3 and higher columns need to be replicated is
  #once.
  #If the number of times that the level 3 and higher columns need to be replicated
  #is less than zero (i.e for a 1 story house) then set this flag to 0.
  stock$levelthreefloors <- stock$J2 - 2
  stock$levelthreefloors[stock$levelthreefloors < 0] <- 0
  
  #Calculate total floor area using the floor area for each level, the room in roof
  #and extension 1 and extension 2. Level 3 floor areas are multiplied by the number
  #of times that level 3 and above information needs to be replicated
  stock$totalfloorarea <- stock$N1_A+stock$N2_A+
    stock$N3_A+(stock$N4_A*stock$levelthreefloors) +
    stock$N5_A+stock$N6_A+stock$N7_A
  
  
  #The next section calculates the window area for each case based on the
  #calculations detailed in SAP 9.91 Appendix S Table S4: Windowbarea (m?) - there 
  #are a number of calculations based on the building type (flat or house), the age 
  #of the property, and the total floor area.
  
  #Test if case is flat - TRUE, if case is not flat (i.e. house) - FALSE
  flat.test <- levels(stock$C1)[stock$C1] =="Not house"
  #Window areas of houses
  stock$windowarea[!flat.test & (levels(stock$M1)[stock$M1]=="Pre 1919" |
                                   levels(stock$M1)[stock$M1]=="1919 - 1929" |
                                   levels(stock$M1)[stock$M1]=="1930 - 1949")] <-
    0.1220*stock$totalfloorarea[!flat.test & 
                                  (levels(stock$M1)[stock$M1]=="Pre 1919" |
                                   levels(stock$M1)[stock$M1]=="1919 - 1929" |
                                   levels(stock$M1)[stock$M1]=="1930 - 1949")]+6.875
  stock$windowarea[!flat.test & levels(stock$M1)[stock$M1]=="1950 - 1964"] <-
    0.1294*stock$totalfloorarea[!flat.test &
                                  levels(stock$M1)[stock$M1]=="1950 - 1964"]+5.515
  stock$windowarea[!flat.test & levels(stock$M1)[stock$M1]=="1965 - 1975"] <-
    0.1239*stock$totalfloorarea[!flat.test &
                                  levels(stock$M1)[stock$M1]=="1965 - 1975"]+7.332
  stock$windowarea[!flat.test & levels(stock$M1)[stock$M1]=="1976 - 1983"] <-
    0.1252*stock$totalfloorarea[!flat.test &
                                  levels(stock$M1)[stock$M1]=="1976 - 1983"]+5.520
  stock$windowarea[!flat.test & levels(stock$M1)[stock$M1]=="1984 - 1991"] <-
    0.1356*stock$totalfloorarea[!flat.test &
                                  levels(stock$M1)[stock$M1]=="1984 - 1991"]+5.242
  stock$windowarea[!flat.test & levels(stock$M1)[stock$M1]=="1992 - 1996"] <-
    0.0948*stock$totalfloorarea[!flat.test &
                                  levels(stock$M1)[stock$M1]=="1992 - 1996"]+6.534
  stock$windowarea[!flat.test & levels(stock$M1)[stock$M1]=="1999 - 2002"] <-
    0.1382*stock$totalfloorarea[!flat.test &
                                  levels(stock$M1)[stock$M1]=="1999 - 2002"]-0.027
  stock$windowarea[!flat.test & (levels(stock$M1)[stock$M1]=="2003 - 2007" |
                                   levels(stock$M1)[stock$M1]=="2008 onwards")] <-
    0.1435*stock$totalfloorarea[!flat.test & 
                                  (levels(stock$M1)[stock$M1]=="2003 - 2007" |
                                  levels(stock$M1)[stock$M1]=="2008 onwards")]-0.403  
  #Window areas of flats
  stock$windowarea[flat.test & (levels(stock$M1)[stock$M1]=="Pre 1919" |
                                  levels(stock$M1)[stock$M1]=="1919 - 1929" |
                                  levels(stock$M1)[stock$M1]=="1930 - 1949")] <-
    0.0801*stock$totalfloorarea[flat.test 
                                & (levels(stock$M1)[stock$M1]=="Pre 1919" |
                                   levels(stock$M1)[stock$M1]=="1919 - 1929" |
                                   levels(stock$M1)[stock$M1]=="1930 - 1949")]+5.580
  stock$windowarea[flat.test & levels(stock$M1)[stock$M1]=="1950 - 1964"] <-
    0.0341*stock$totalfloorarea[flat.test 
                                & levels(stock$M1)[stock$M1]=="1950 - 1964"]+8.562
  stock$windowarea[flat.test & levels(stock$M1)[stock$M1]=="1965 - 1975"] <-
    0.0717*stock$totalfloorarea[flat.test 
                                & levels(stock$M1)[stock$M1]=="1965 - 1975"]+6.560
  stock$windowarea[flat.test & levels(stock$M1)[stock$M1]=="1976 - 1983"] <-
    0.1199*stock$totalfloorarea[flat.test 
                                & levels(stock$M1)[stock$M1]=="1976 - 1983"]+1.975
  stock$windowarea[flat.test & levels(stock$M1)[stock$M1]=="1984 - 1991"] <-
    0.0510*stock$totalfloorarea[flat.test 
                                & levels(stock$M1)[stock$M1]=="1984 - 1991"]+4.554
  stock$windowarea[flat.test & levels(stock$M1)[stock$M1]=="1992 - 1996"] <-
    0.0813*stock$totalfloorarea[flat.test 
                                & levels(stock$M1)[stock$M1]=="1992 - 1996"]+3.744
  stock$windowarea[flat.test & levels(stock$M1)[stock$M1]=="1999 - 2002"] <-
    0.1148*stock$totalfloorarea[flat.test 
                                & levels(stock$M1)[stock$M1]=="1999 - 2002"]+0.392
  stock$windowarea[flat.test & (levels(stock$M1)[stock$M1]=="2003 - 2007" |
                                  levels(stock$M1)[stock$M1]=="2008 onwards")] <-
    0.1148*stock$totalfloorarea[flat.test 
                                & (levels(stock$M1)[stock$M1]=="2003 - 2007" |
                                  levels(stock$M1)[stock$M1]=="2008 onwards")]+0.392
  
  #Calculate the total wall area using all floors apart from the room in roof
  stock$wallarea <- (stock$N1_C*stock$N1_D) + 
    (stock$N2_C*stock$N2_D) +
    (stock$N3_C*stock$N3_D) +
    (stock$N4_C*stock$N4_D*stock$levelthreefloors) +
    (stock$N6_C*stock$N6_D) +
    (stock$N7_C*stock$N7_D)
  #Calculate the total wall area for the few cases which are all flats where the
  #only level is a room in roof
  stock$wallarea[stock$wallarea == 0 & stock$E7 == "1 wall exposed"] <- 
    stock$N5_C[stock$wallarea == 0 & stock$E7 == "1 wall exposed"] *
    (sqrt(stock$N5_A[stock$wallarea == 0 & stock$E7 == "1 wall exposed"]/1.5))
  
  stock$wallarea[stock$wallarea == 0 & stock$E7 == "1 to 2 walls exposed"] <- 
    stock$N5_C[stock$wallarea == 0 & stock$E7 == "1 to 2 walls exposed"] *
    (((sqrt(stock$N5_A[stock$wallarea == 0 & stock$E7 == "1 to 2 walls exposed"]
            *1.5))*0.5)+
    (sqrt(stock$N5_A[stock$wallarea == 0 & stock$E7 == "1 to 2 walls exposed"]/1.5)))
 
  stock$wallarea[stock$wallarea == 0 & stock$E7 == "2 walls exposed"] <- 
    stock$N5_C[stock$wallarea == 0 & stock$E7 == "2 walls exposed"] *
    (sqrt(stock$N5_A[stock$wallarea == 0 & stock$E7 == "2 walls exposed"]*1.5)+
       sqrt(stock$N5_A[stock$wallarea == 0 & stock$E7 == "2 walls exposed"]/1.5))
  
  stock$wallarea[stock$wallarea == 0 & stock$E7 == "2 to 3 walls exposed"] <- 
    stock$N5_C[stock$wallarea == 0 & stock$E7 == "2 to 3 walls exposed"] *
    (sqrt(stock$N5_A[stock$wallarea == 0 & stock$E7 == "2 to 3 walls exposed"]*1.5)+
    ((sqrt(stock$N5_A[stock$wallarea == 0 & stock$E7 == "2 to 3 walls exposed"]/1.5))
     *1.5))
  
  stock$wallarea[stock$wallarea == 0 & stock$E7 == "3 walls exposed"] <- 
    stock$N5_C[stock$wallarea == 0 & stock$E7 == "3 walls exposed"] *
    (sqrt(stock$N5_A[stock$wallarea == 0 & stock$E7 == "3 walls exposed"]*1.5)+
    ((sqrt(stock$N5_A[stock$wallarea == 0 & stock$E7 == "3 walls exposed"]/1.5))*2))
  
  stock$wallarea[stock$wallarea == 0 & stock$E7 == "3 to 4 walls exposed"] <- 
    stock$N5_C[stock$wallarea == 0 & stock$E7 == "3 to 4 walls exposed"] *
    (((sqrt(stock$N5_A[stock$wallarea == 0 & stock$E7 == "3 to 4 walls exposed"]*1.5))
      *1.5)+
    ((sqrt(stock$N5_A[stock$wallarea == 0 & stock$E7 == "3 to 4 walls exposed"]/1.5))
     *2))
  
  stock$wallarea[stock$wallarea == 0 & stock$E7 == "4 walls exposed"] <- 
    stock$N5_C[stock$wallarea == 0 & stock$E7 == "4 walls exposed"] *
    (((sqrt(stock$N5_A[stock$wallarea == 0 & stock$E7 == "4 walls exposed"]*1.5))*2)+
       ((sqrt(stock$N5_A[stock$wallarea == 0 & stock$E7 == "4 walls exposed"]/1.5))
        *2))
  
  #Calculate the total openings of each case by dividing the window area by the wall
  #area, multiplying by 10 then rounding to turn into tenths as required by the NHM.
  #Cases with areas less than 1 are set to 1 tenth
  #Cases with areas higher than 7 (2 cases with greater than 10 tenths) are set to
  #7 tenths
  stock$totalopenings <- signif((stock$windowarea/stock$wallarea)*10,1)
  stock$totalopenings[stock$totalopenings < 1] <- 1
  stock$totalopenings[stock$totalopenings > 7] <- 7
  
  #These tenths are then assigned to each elevation using a calculation based on the
  #amount of tenths unattached (i.e external wall available for windows)
  stock$front.tenthsopening <- 
    round((stock$totalopenings*(10-stock$front.tenthsattached)/10),0)
  stock$left.tenthsopening <- 
    round((stock$totalopenings*(10-stock$left.tenthsattached)/10),0)
  stock$back.tenthsopening <- 
    round((stock$totalopenings*(10-stock$back.tenthsattached)/10),0)
  stock$right.tenthsopening <- 
    round((stock$totalopenings*(10-stock$right.tenthsattached)/10),0)
  return(stock)
}

#'
#'\pagebreak
#'
#' ##Windows
#'
#'Create the types of double glazing and single glazing window frame and the
#' percentage of double glazing
#'
#'The single glazing window frame is set to the same type as the type of double
#' glazing as no information about single glazing window types is available in the 
#' SHCS stock variables
#'
#'@param stock - this is the whole of elevations created by other functions in this
#' script, which is comprised of the scottish survey with additional columns
make.windowframes <- function(stock){
  stock$doubleglazed <- as.factor(checked.revalue(
    stock$Q45,c(
      "Wood" = "wood"
      ,"Metal (therm break)" = "metal"
      ,"Metal (no therm break)" = "metal"
      ,"UPVC" = "upvc"
      ,"Not applicable" = "NULL"    
      ,"Unobtainable" = "NULL"
    )))
  #Double glazing percentage is turned from tenths (as presented in the SHCS) into a 
  #percentage
  stock$percentagedoubleglazing <- stock$Q47*10
  
  stock$singleglazed <- as.factor(checked.revalue(
    stock$Q45,c(
      "Wood" = "wood"
      ,"Metal (therm break)" = "metal"
      ,"Metal (no therm break)" = "metal"
      ,"UPVC" = "upvc"
      ,"Not applicable" = "NULL"    
      ,"Unobtainable" = "NULL"
    )))
  
  #if the double glazing area is 100% then single glazing percentage is set to NULL
  stock$singleglazed[stock$percentagedoubleglazing == 100] <- "NULL"
  
  return(stock)
}

#'
#'\pagebreak
#'
#'Each elevation is assigned window frame type and the percentage of double glazing
#'No information is available about the window area on different faces of the 
#'dwelling so each elevation has the same double glazing percentage as the overall 
#'dwellings
#'
#'Calls the window.frames function
#'
#'@param stock - this is the whole of elevations created by other functions in this
#' script, which is comprised of the scottish survey with additional columns
assigning.windowframes <- function(stock){
  #columns in correct format for next section
  stock <- window.frames(stock,"front")
  stock <- window.frames(stock,"left")
  stock <- window.frames(stock,"back")
  stock <- window.frames(stock,"right")
  return(stock)
}

#'An elevation is assigned window frames and percentage of double glazing
#'
#'This is independent of if the elevation is attached or not as required by the NHM
#'
#'@param stock - this is the whole of elevations created by other functions in this
#' script, which is comprised of the scottish survey with additional columns
#' 
#'@param elevation - a string which contains the type of elevation, i.e. "front"
window.frames <- function(stock,elevation){
  #If opening is present on elevation then assign window types and percentage
  #of double glazed
  stock[,paste(elevation,"doubleglazedwindowframe",sep=".")] <- stock$doubleglazed
  stock[,paste(elevation,"singleglazedwindowframe",sep=".")] <- stock$singleglazed
  stock[,paste(elevation,"percentagedoubleglazed",sep=".")] <- 
    stock$percentagedoubleglazing
  return(stock)
}

#'
#'\pagebreak
#'
#' ##Door frames
#'
#'Each elevation is assigned door frames
#'
#'Calls the door.frames function
#'
#'@param stock - this is the whole of elevations created by other functions in this
#' script, which is comprised of the scottish survey with additional columns
assigning.doorframes <- function(stock){
  stock <- door.frames(stock,"front")
  stock <- door.frames(stock,"left")
  stock <- door.frames(stock,"back")
  stock <- door.frames(stock,"right")
  return(stock)
}

#'An elevation is assigned external door frames
#'
#'An elevation can only be assigned an external door if there is some tenthsopening
#' on that elevation.
#'
#'There is no data in the scottish survey about doors. It has been assumed that the
#' type of door frame is the same material as the windows and all doors are solid.
#' It has been assumed that doors only exist on either the front or if applicable
#' the back (for houses with an exposed back wall, so for example, enclosed terraces 
#' are not assigned a second external door in the back wall).
#'
#'@param stock - this is the whole of elevations created by other functions in this
#' script, which is comprised of the scottish survey with additional columns
#' 
#'@param elevation - a string which contains the type of elevation, i.e. "front"
door.frames <- function(stock,elevation){
  #If opening is present on an elevation and the elevation is back and the case is
  #a house or the elevation is the front and not a towerblock unless all 4 walls are
  #exposed then a door can be placed on the elevation
  door.test <- ((stock[,paste(elevation,"tenthsopening",sep=".")] > 0) & 
                  ((elevation == "back" & stock$C1 != "Not house") |
                   (elevation == "front" & (stock$C2 != "Tower or slab" |
                                          stock$buildinginfo == "4 walls exposed"))))
  
  #Set all the doorframe types to 0 for all cases
  stock[,paste(elevation,"doorframe:wood,doortype:glazed",sep=".")] <- 0
  stock[,paste(elevation,"doorframe:metal,doortype:glazed",sep=".")] <- 0
  stock[,paste(elevation,"doorframe:upvc,doortype:glazed",sep=".")] <- 0
  stock[,paste(elevation,"doorframe:wood,doortype:solid",sep=".")] <- 0
  stock[,paste(elevation,"doorframe:metal,doortype:solid",sep=".")] <- 0
  stock[,paste(elevation,"doorframe:upvc,doortype:solid",sep=".")] <- 0
  
  #If the door.test is true (i.e. there can be a door on elevation) then one door is
  #assigned to the elevation of a material to match the double glazing.
  stock[(door.test == "TRUE" & stock$doubleglazed == "wood"),
        paste(elevation,"doorframe:wood,doortype:solid",sep=".")] <- 1
  stock[(door.test == "TRUE" & stock$doubleglazed == "metal"),
        paste(elevation,"doorframe:metal,doortype:solid",sep=".")] <- 1
  stock[(door.test == "TRUE" & stock$doubleglazed == "upvc"),
        paste(elevation,"doorframe:upvc,doortype:solid",sep=".")] <- 1
  
  return(stock)
}
