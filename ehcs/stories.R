#' ---
#' title: "Storeys functions"
#' author: "Centre for Sustainable Energy"
#' date: "2015"
#' output: pdf_document
#' ---

#' ## Functions for making polygons
#'
#' Each storey that is produced has a floorplan, which is defined by a
#' polygon. Ultimately, the polygon is represented as a list of
#' x-coordinates and a list of y-coordinates, but as an intermediate
#' form we use a 2xN matrix where the first column is called xs and
#' the second ys. This is easier to work with. All floorplans in the
#' EHS are composed of one or two rectangles; the first rectangle is
#' the "main module" and the second is the "additional module". The
#' additional module is joined to the main module in one of 12
#' positions. These 12 positions can be produced by rotations and
#' mirrorings of two canonical positions, which is how it is done
#' here:

#' ## Flip a polygon in the x-axis.
#'
#' For example, if you had a floor with an additional module on the
#' back elevation, and you wanted one on the front elevation instead,
#' you could flip it on the x-axis (this is the back-to-front axis).
poly.flipx <- function(a) {
    xs <- a[, 'xs']
    cbind(xs=-xs + max(xs),
          ys=a[, 'ys'])
}

#' ## Flip a polygon in the y-axis
#'
#' Analogous to poly.flipx above, but for y.
poly.flipy <- function(a) {
    ys <- a[, 'ys']
    cbind(xs=a[, 'xs'],
          ys=-ys + max(ys))
}

#' ## Reflect a polygon in y=x
#'
#' If you had a floor with a module on the front or back elevation,
#' and you wanted it on the bottom or top elevation, you could reflect
#' it in y=x; this is like rotating it 90 degrees clockwise.
poly.mirror <- function(a) cbind(xs=a[, 'ys'],ys=a[, 'xs'])

#' ## Make a rectangular polygon
#'
#' This makes a polygon with 4 points, having the given depth (x-dimension) and width (y-dimension)
#'
#' The polygon starts at 0,0
#'
#' @param md the depth (x extent)
#' @param mw the width (y extent)
poly.rect <- function(md, mw)
    cbind(xs=c(0, md, md, 0),
          ys=c(0, 0,  mw, mw))

#' ## Make a polygon for a house with an additional module on the left of the front elevation.
#'
#' This is one of the two canonical types of two-module house. It's a bit like an L-shape:
#'
#'           MD               AD
#'     +-----------------+--------+
#'     |                 |        |
#'  MW |      M          |    A   | AW
#'     |                 +--------+
#'     |                 |
#'     +-----------------+
#'
#' @param md the main module's depth
#' @param mw the main module's width.
#' @param ad the additional module's depth
#' @param aw the additional module's width  If this equals MW, the result is just a rectangle.
poly.leftmodule <- function(md, mw, ad, aw)
{
    if      (ad == 0 | aw == 0) {poly.rect(md, mw)} # module is empty
    else if (md == 0 | mw == 0) {poly.rect(ad, aw)} # main is empty
    else if (aw == mw)          {poly.rect(md + ad, mw)} # module is as wide as main
    else                        {cbind(xs=c(0, md + ad, md + ad, md, md, 0 ),
                                       ys=c(0, 0      , aw     , aw, mw, mw))}
}

#' ## Make a polygon for a house with an additional module in the middle of the front elevation.
#'
#' This is the other canonical type of two module house; it's a bit like a T-shape:
#'        MD
#' +--------------+
#' |              |  AD
#' |              +-------+
#'M|              |   A   | AW
#'W|      M       |       |
#' |              +-------+
#' |              |
#' +--------------+
#'
#' The horizontal centre line of the additional module is the horizontal centre line of the main module
#'
#' @param md main module depth
#' @param mw main width
#' @param ad additional depth
#' @param aw additional width. If this equals MW, the result is just a rectangle.
poly.midmodule <- function(md, mw, ad, aw)
{
    if      (ad == 0 | aw == 0) {poly.rect(md, mw)} # module is empty
    else if (md == 0 | mw == 0) {poly.rect(ad, aw)} # main is empty
    else if (aw == mw)          {poly.rect(md + ad, mw)} # module is as wide as main
    else                        {cbind(xs=c(0, md, md           , md + ad      , md + ad    , md         , md, 0 ),
                                       ys=c(0, 0 , (mw - aw) / 2, (mw - aw) / 2, (mw + aw)/2, (mw + aw)/2, mw, mw))}
}

#' ## Make a polygon for an arbitrary EHS house
#'
#' Create a polygon describing a floor, based on the main an additional module dimensions,
#' and the additional module location.
#'
#' This is done by flipping and rotating either poly.leftmodule or poly.midmodule.
#' Note that whenever a rotation is used (poly.mirror), the depth and width arguments
#' to the polygon that's been mirrored are swapped. This is because after the rotation,
#' depth is width and vice-versa.
#'
#' @param md main module depth
#' @param mw main module width
#' @param ad additional module depth
#' @param aw additional module width
#' @param a.location additional module location
floor.polygon <- function(md, mw,
                          ad, aw,
                          a.location)
    switch(as.character(a.location),
           "Back elevation: Left"    = poly.leftmodule(md, mw, ad, aw),
           "Back elevation: Centre"  = poly.midmodule(md, mw, ad, aw),
           "Back elevation: Right"   = poly.flipy(poly.leftmodule(md, mw, ad, aw)),
           "Front elevation: Left"   = poly.flipx(poly.leftmodule(md, mw, ad, aw)),
           "Front elevation: Centre" = poly.flipx(poly.midmodule(md, mw, ad, aw)),
           "Front elevation: Right"  = poly.flipx(poly.flipy(poly.leftmodule(md, mw, ad, aw))),
           "Left elevation: Back"    = poly.flipx(poly.mirror(poly.flipx(poly.leftmodule(mw, md, aw, ad)))),
           "Left elevation: Centre"  = poly.flipx(poly.mirror(poly.flipx(poly.midmodule(mw, md, aw, ad)))),
           "Left elevation: Front"   = poly.mirror(poly.flipx(poly.leftmodule(mw, md, aw, ad))),
           "Right elevation: Back"   = poly.flipx(poly.mirror(poly.leftmodule(mw, md, aw, ad))),
           "Right elevation: Centre" = poly.mirror(poly.midmodule(mw, md, aw, ad)),
           "Right elevation: Front"  = poly.mirror(poly.leftmodule(mw, md, aw, ad)),
           "No additional part"      = poly.rect(md, mw),
           "Unknown"                 = poly.rect(md, mw),
           poly.rect(md, mw))

#' ## Produce an output string from a vector of coordinates
#'
#' This is simple enough; coordinate.string(c(1,2,3)) is "{ 1, 2, 3 }"
coordinate.string <- function(vec) paste("{", paste(round(vec), collapse=","),"}")

#' ## Make a DTO storey type from a floor number
#'
#' When building the storeys, we use a number to keep track of what floor we are on.
#' The NHM wants to see a floor type, so we code the floor index up as a type.
#'
#' The convention is -1 for basement and 0 for ground.
floor.number.to.type <- function(n) {
    if (n == -1) {
        "BASEMENT"
    } else if (n == 0) {
        "GROUND"
    } else if (n == 1) {
        "FIRST_FLOOR"
    } else if (n == 2) {
        "SECOND_FLOOR"
    } else if (n == 99) {
        "TOP_FLOOR"
    } else if (n == 100) {
        "ROOM_IN_ROOF"
    } else {
        "HIGHER_FLOOR"
    }
}

#' ## Make a single floor (just the polygon and type)
#'
#' This function produces a single floor of a flat or a house, in a DTO form in a data frame.
#' It also throws in the area, just in case.
#'
#' @param floor - which floor we are looking at. This is always a
#'     number from 1 to N, representing which floor this is out of the
#'     total we are making. It's not which actual floor of the
#'     building we are on.
#' @param use.flatdets - if TRUE, we will try and find the dimensions
#'     in the flatdets SPSS file. Otherwise, they come from the
#'     physical dimensions file.
#' @param frame - A frame containing all the SPSS variables about one
#'     single house. Variables should have lowercase names.
make.one.floor <- function(floor, # floor number, from 1 to n
                           use.flatdets,
                           start.floor, # position of bottom floor (-1 for basement, 0 for 0, etc.)
                           frame) {
    if (use.flatdets) { # in a flat, we often use these variables:
        if (floor == 1) {
            maindepth <- frame$fdfmaind
            mainwidth <- frame$fdfmainw
            adddepth <- 0
            addwidth <- 0
        } else {
            maindepth <- frame$fdfnextd
            mainwidth <- frame$fdfnextw
            adddepth <- 0
            addwidth <- 0
        }
    } else { # in a house, we use the module information
        maindepth <- 0
        mainwidth <- 0
        adddepth <- 0
        addwidth <- 0

        ## we use the highest numbered set of floor data
        ## which (a) is good for this floor (i.e. not too high)
        ## and   (b) has a defined main module size.

        if (floor > 0 &
            !is.na(frame$fdhmdep1) &
            !is.na(frame$fdhmwid1)   ) {
            maindepth <- frame$fdhmdep1
            mainwidth <- frame$fdhmwid1
            adddepth  <- frame$fdhadep1
            addwidth  <- frame$fdhawid1
        }

        if (floor > 1  &
            !is.na(frame$fdhmdep2) &
            !is.na(frame$fdhmwid2)   ) {
            maindepth <- frame$fdhmdep2
            mainwidth <- frame$fdhmwid2
            adddepth  <- frame$fdhadep2
            addwidth  <- frame$fdhawid2
        }

        if (floor > 2 &
            !is.na(frame$fdhmdep3) &
            !is.na(frame$fdhmwid3)   ) {
            maindepth <- frame$fdhmdep3
            mainwidth <- frame$fdhmwid3
            adddepth  <- frame$fdhadep3
            addwidth  <- frame$fdhawid3
        }
    }

    ## cook up the polygon for the floor (times 100 as we are in cm)
    poly <- floor.polygon(maindepth*100, mainwidth*100,
                          adddepth*100, addwidth*100,
                          ## this the additional module location -
                          ## if the additional module data is missing we supply
                          ## "", which causes no additional module to be built.
                          ## otherwise fshaddit.
                          if (use.flatdets | is.na(adddepth) | is.na(addwidth)) ""
                          else frame$fshaddit)

    polyarea <- areapl(poly)

    data.frame(area=polyarea,
               type=floor.number.to.type(floor - 1 + start.floor),
               polygonxpoints=coordinate.string(poly[, 'xs']),
               polygonypoints=coordinate.string(poly[, 'ys']),
               polypoints=length(poly[, 1]))
}

#' ## Should we start with a basement storey?
#'
#' This is true if basement is 'yes', or if fdhmlev1 is bb or -1, or dwtype3x contains 'basement'
is.basement <- function(frame, flat)
    frame$basement == "yes" | # sometimes the basement field is yes
        tolower(frame$fdhmlev1) %in% c("bb", "-1") | ## sometimes there is bb in the coding
        (flat & grepl("basement", frame$finlopos, ignore.case=TRUE)) # and sometimes it says basement in the name

#' ## Now we're getting somewhere: make the DTO data for one house
#'
#' @param frame - a data frame containing the required variables and a
#'     single row which is just one aacode.
#'
#' @param floors.total - the total number of floors to make
#' @param start.floor - the index of the starting floor; -1 for
#'     basement, 0 for ground, 1 for first...
#'
#' @return a data frame containing several rows, one for each storey
#'     of the house
one.house.storeys <- function(frame, floors.total, start.floor) {
    floors.total <- ifelse(tolower(frame$attic) == "yes", floors.total -1, floors.total)
    
    ## this is just saying: for each integer from 1 to floors.total
    ## call make.one.floor(N, FALSE, start.floor, frame)
    ## and stick the results together in one frame.
    result <- do.call(rbind,
                      lapply(1:floors.total, make.one.floor,
                             FALSE, start.floor, frame))

    ## we need the top floor area in case there is a room in roof
    ## as the room in roof is modelled as a simple square on the top
    top.area <- result$area[length(result$area)]

    ## now we kill that columns as we don't want it to come out in the
    ## DTO.
    result$area <- NULL

    ## make a storey for the room in roof, if there is one!
    if ( tolower(frame$attic) == "yes" ) {
        ## room in roof area is top floor area / 2
        side <- sqrt(top.area / 2)
        poly <- poly.rect(side, side)
        ## could be a little neater, but there we go
        result <- rbind(result,
                        data.frame(
                            type="ROOM_IN_ROOF",
                            polygonxpoints=coordinate.string(poly[, 'xs']),
                            polygonypoints=coordinate.string(poly[, 'ys']),
                            polypoints=length(poly[, 1])))
    }

    ## and we are done
    result
}

#' ## The action hots up: make the storeys for a flat
#'
#' This is the analog of one.house.storeys but for flats.
#'
#' @param frame - a data frame with one row in, which is the SPSS data
#'     for one flat.
#' @return a data frame for the storeys in that flat
one.flat.storeys <- function(frame) {
    ## the fdffloor variable tells us how many floors are in the flat.
    ## sometimes it's NA, in which case we go for 1 floor
    floors <- ifelse(is.na(frame$fdffloor), 1, frame$fdffloor)

    ## How we determine the bottom floor's type:
    if (is.basement(frame, flat = TRUE)) {
        start.floor <- -1
    } else if (tolower(frame$finlopos) == "ground floor flat") {
        start.floor <- 0
    } else if (tolower(frame$finlopos) == "top floor flat") {
        ## We want the last floor to be floor 99, and assign the first floor number accordingly
        ## In practice, most top-floor flats have a single floor only.
        start.floor <- 99 - (floors - 1)
    } else {
        start.floor <- 1
    }

    ## An EHS oddity: some flats are described by the flatdets file,
    ## and the module information is for the building containing all
    ## the flats. Other flats are described by the module information;
    ## fdfsamed is "yes" when we should use the module information,
    ## but in certain cases this says "yes" but the module information
    ## is NA and the flatdets information is not. C'est la vie.
    if (tolower(frame$fdfsamed) == "no" & !is.na(frame$fdfmainw)) {
        ## So in this case we make a floor for each floor, telling
        ## make.one.floor to use flatdets information (the TRUE below).
        result <- do.call(rbind, lapply(1:floors,
                                        make.one.floor,
                                        TRUE, start.floor, frame))

        result$area <- NULL
        result
    } else {
        ## Otherwise we just treat it like a house (the module data is the same)
        one.house.storeys(frame, floors, start.floor)
    }
}

#' ## The inverse of coordinate.string
parse.coordinate.string <-
    function(ps) (gsub("[^0-9,-\\.]", "", as.character(ps)) %>%
                  strsplit(",", fixed=TRUE) %>%
                  unlist() %>%
                  as.numeric()) / 100

#' ## Compute the area of a polygon expressed in DTO format
coordinate.string.area <- function(xs, ys)
    mapply(function(xs, ys)
        areapl(cbind(parse.coordinate.string(xs),
                     parse.coordinate.string(ys))),
        xs, ys)

#' ## Re-scale the coordinates of a polygon
#'
#' @param xs - the coordinates, written in DTO form (i.e. a string)
#' @param factor - a scaling factor
#' @return a new DTO form coordinate list
rescale.coordinate.string <- function(xs, factor)
    mapply(function(xs, factor) coordinate.string(parse.coordinate.string(xs) * 100 * factor),
           xs, factor)

#' ## Re-scale an entire building
#'
#' @param result - the kind of frame made within one.building.storeys
#' @param target - the target total area for the building
#' @return a copy of result with the polygons scaled to have the target area
scale.building.storeys <- function(result, target) {
    area <- sum(with(result, coordinate.string.area(polygonxpoints, polygonypoints)))
    scaling.factor <- sqrt(target / area)

    mutate(result,
           polygonxpoints = rescale.coordinate.string(polygonxpoints, scaling.factor),
           polygonxpoints = rescale.coordinate.string(polygonxpoints, scaling.factor))
}

#' ## What you've all been waiting for: make the storeys for a house or a flat
#'
#' This makes the final DTO rows we need for any row in the EHS,
#'
#' @param frame - a data frame containing all the spss variables for a
#'     single case, lowercased column names.
#' @param scale - if true, scale total area to floorarea in frame
one.building.storeys <- function(frame, scale) {
    ## Find out if we are making a house
    if (is.a.house(frame$dwtype8x) == TRUE) {
    #if (grepl("house", frame$dwtype8x)) {
        ## so if we are making a house, we need to know how many storeys and
        ## whether there's a basement
        if (is.basement(frame, flat = FALSE)) {
            start.floor <- -1
            floors.total <- frame$storeyx + 1
        } else {
            start.floor <- 0
            floors.total <- frame$storeyx
        }

        result <- one.house.storeys(frame, floors.total, start.floor)
    } else {
        result <- one.flat.storeys(frame)
    }

    ## so now result is done, except it needs the ceiling and storey heights.

    ## set ceiling heights with mean ceiling height, for most cases
    result$ceilingheight <- mean(c(frame$finlivcl,
                                   frame$finbedcl,
                                   frame$finkitcl,
                                   frame$finbatcl),
                                 na.rm=TRUE)

    ## set exterior height to ceiling height plus a bit
    ## to start with it's at least the ceiling height + 0.25
    result$storyheight <- result$ceilingheight + 0.25

    ## except on the first floor
    result$storyheight[[1]] <- result$ceilingheight[[1]]

    ## and room in roof floor is also special.
    result$storyheight[result$type == "ROOM_IN_ROOF"] <- 2.45
    result$ceilingheight[result$type == "ROOM_IN_ROOF"] <- 2.45

    ## This is a technical detail: because this result is for a single
    ## house, and we are going to glue all the results for many houses
    ## together, we need to ensure that the text columns are actually
    ## text rather than factors. Gluing factor columns together
    ## produces weird results, because they are really integers coding
    ## for the factor levels and those will be different for each
    ## house, blah blah.

    result$polygonxpoints <- as.character(result$polygonxpoints)
    result$polygonypoints <- as.character(result$polygonypoints)
    result$type <- as.character(result$type)

    if (scale) {
        result <- scale.building.storeys(result, frame$floorarea)
    }

    result
}

#' ## The main method
#'
#' This makes all the storeys for the whole EHS in one go.
#'
#' @param base.directory - where to read the SPSS files from
#' @param output.file - where to write the storeys.csv file to.
#' @param scale - if TRUE, scale the total floor area for each house to derived/dimensions/FloorArea
generate.all.storeys <- function(base.directory, output.file, scale) {
    read.input <- function(p) {
        r <- read.spss(file.path(base.directory, p),
                       to.data.frame=TRUE,
                       reencode='utf-8')
        colnames(r) <- tolower(colnames(r))
        r$caseno <- NULL
        r$aacode <- trimws(as.character(r$aacode))
        r
    }

    join.aacode <- function(a, b) merge(a, b, by="aacode")

    big.data <- Reduce(join.aacode,
                       list(
                           read.input("derived/detailed/dimensions_13plus14_sl_protect.sav"),
                           read.input("derived/physical_13plus14_sl_protect.sav"),
                           read.input("physical/shape_sl_protect.sav"),
                           read.input("physical/flatdets_sl_protect.sav"),
                           read.input("physical/interior_sl_protect.sav"),
                           read.input("physical/services_sl_protect.sav")))

    ## for speed, retain only the variables we are actually using
    ## and make a data.table rather than a data.frame.
    big.data <- big.data[, c(
        "aacode",   "attic",    "basement", "dwtype3x", "dwtype8x",
        "fdffloor", "fdfmaind", "fdfmainw", "fdfnextd", "fdfnextw",
        "fdfsamed", "fdhadep1", "fdhadep2", "fdhadep3", "fdhawid1",
        "fdhawid2", "fdhawid3", "fdhmdep1", "fdhmdep2", "fdhmdep3",
        "fdhmlev1", "fdhmwid1", "fdhmwid2", "fdhmwid3", "finbatcl",
        "finbedcl", "finkitcl", "finlivcl", "fshaddit", "finlopos",
        "storeyx",  "floorarea"
    )]

    big.data <- data.table(big.data)

    setkey(big.data, "aacode")

    ## this is the bit where we say, split the table by aacode, and then
    ## run one.building.storeys for each subset, and then join them
    ## all together again. For more on this, read the data.table manual
    all.result <- big.data[, one.building.storeys(.SD, scale), by=aacode]

    write.csv(all.result, output.file, row.names=FALSE)
}
