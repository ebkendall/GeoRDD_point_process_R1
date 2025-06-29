# NOTE: this code is quite old but it gives you a sense of how we break up the
# initial street data to obtain more street segments that can serve as null streets

# The results of this function are already saved data files that can be found in 
# the directory ../RealData/Data/Street_Seg
  
streetBreaker_optimized <- function(bufferWidth, filteredStreets) { 
  
    returnList <- vector(mode = "list", length = 77)
    index1 <- bufferWidth / 10

    for (i in 1:length(filteredStreets)) {

        returnList[[i]] = vector(mode = "list", length = length(filteredStreets[[i]]))
        precNum <- nyc$Precinct[i]
        print(paste0("i: ", i))
        for(j in 1:length(filteredStreets[[i]])) {
            
            print(j)
            returnList[[i]][[j]] = vector(mode = "list", length = 1)
            newNum=1
            offenseData <- dplyr::filter(dataOffFiltered, dataOffFiltered$precinct == precNum)
            arrestData <- dplyr::filter(dataArrF, dataArrF$precinct == precNum)
            
            if(length(filteredStreets[[i]][[j]]@lines[[1]]@Lines) > 1) {
            
                # (A) Start by checking each individual component
                # print("A")
                for (p in 1:length(filteredStreets[[i]][[j]]@lines[[1]]@Lines)) {
                    
                    # print(paste0("1. j:", j))
                    streetOfInt <- SpatialLines(list(Lines(filteredStreets[[i]][[j]]@lines[[1]]@Lines[p], "id")))
                    
                    tempBuffer <- gBuffer(streetOfInt, width=bufferWidth, byid=TRUE, capStyle = "flat")
                    
                    tempArr <- point.in.polygon(arrestData$x_coord_cd, arrestData$y_coord_cd,
                                                tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,2])
                    tempOff <- point.in.polygon(offenseData$x_coord_cd, offenseData$y_coord_cd,
                                                tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,2])
                    
                    numArrBIG <- sum(tempArr > 0)
                    numOffBIG <- sum(tempOff > 0)
                    
                    ##SMALL##
                    tempBuffer <- gBuffer(streetOfInt, width=50, byid=TRUE, capStyle = "flat")
                    tempArr <- point.in.polygon(arrestData$x_coord_cd, arrestData$y_coord_cd,
                                                tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,2])
                    tempOff <- point.in.polygon(offenseData$x_coord_cd, offenseData$y_coord_cd,
                                                tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,2])
                    numArrSMALL <- sum(tempArr > 0)
                    numOffSMALL <- sum(tempOff > 0)
                    
                    uniqID = paste0(filteredStreets[[i]][[j]]@data$unique_ID, "_", newNum)
                    l = list("precinct" = precNum,
                            "indexOfOrigStreet" = j,
                            "shorterStreet" = streetOfInt,
                            "numArrestBig" = numArrBIG,
                            "numArrestSmall" = numArrSMALL,
                            "numOffBig" = numOffBIG,
                            "numOffSmall" = numOffSMALL,
                            "uniqueID" = uniqID)
                    returnList[[i]][[j]][[newNum]] = l
                    newNum = newNum+1
                    
                    #Make sure there are more than two coordinates to break up
                    #Going through each section of the street
                    # CAN SOMETIMES TAKE FOREVER (WE CAN DO THIS LATER IF NEED BE)
                    
                    if(nrow(filteredStreets[[i]][[j]]@lines[[1]]@Lines[[p]]@coords) > 2) {
                    
                        #Start removing from LEFT to RIGHT
                        for (k in 2:(nrow(filteredStreets[[i]][[j]]@lines[[1]]@Lines[[p]]@coords) - 1)) {
                            
                            #TEST: SpatialLines(list(Lines(streetsBuffer400[[1]][[4]]@lines[[1]]@Lines[2:3], "id")))
                            #length(streetsBuffer400[[1]][[4]]@lines[[1]]@Lines)
                            # print(paste0("Single 1. k:", k))
                            endIndex <- nrow(filteredStreets[[i]][[j]]@lines[[1]]@Lines[[p]]@coords)
                            
                            coordData <- filteredStreets[[i]][[j]]@lines[[1]]@Lines[[p]]@coords
                            streetOfInt <- SpatialLines(list(Lines(list(Line(coordData[k:endIndex,])), "id")))
                            
                            tempBuffer <- gBuffer(streetOfInt, width=bufferWidth, byid=TRUE, capStyle = "flat")
                            
                            tempArr <- point.in.polygon(arrestData$x_coord_cd, arrestData$y_coord_cd,
                                                        tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                        tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,2])
                            tempOff <- point.in.polygon(offenseData$x_coord_cd, offenseData$y_coord_cd,
                                                        tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                        tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,2])
                            
                            numArrBIG <- sum(tempArr > 0)
                            numOffBIG <- sum(tempOff > 0)
                            
                            ##SMALL##
                            tempBuffer <- gBuffer(streetOfInt, width=50, byid=TRUE, capStyle = "flat")
                            tempArr <- point.in.polygon(arrestData$x_coord_cd, arrestData$y_coord_cd,
                                                        tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                        tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,2])
                            tempOff <- point.in.polygon(offenseData$x_coord_cd, offenseData$y_coord_cd,
                                                        tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                        tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,2])
                            numArrSMALL <- sum(tempArr > 0)
                            numOffSMALL <- sum(tempOff > 0)
                            
                            uniqID = paste0(filteredStreets[[i]][[j]]@data$unique_ID, "_", newNum)
                            l = list("precinct" = precNum,
                                    "indexOfOrigStreet" = j,
                                    "shorterStreet" = streetOfInt,
                                    "numArrestBig" = numArrBIG,
                                    "numArrestSmall" = numArrSMALL,
                                    "numOffBig" = numOffBIG,
                                    "numOffSmall" = numOffSMALL,
                                    "uniqueID" = uniqID)
                            returnList[[i]][[j]][[newNum]] = l
                            newNum = newNum+1
                        }
                        
                        #Start removing from RIGHT to LEFT
                        for (k in 1:(nrow(filteredStreets[[i]][[j]]@lines[[1]]@Lines[[p]]@coords) - 2)) {
                            
                            #TEST: SpatialLines(list(Lines(streetsBuffer400[[1]][[4]]@lines[[1]]@Lines[2:3], "id")))
                            #length(streetsBuffer400[[1]][[4]]@lines[[1]]@Lines)
                            # print(paste0("Single 2. k:", k))
                            endIndex <- nrow(filteredStreets[[i]][[j]]@lines[[1]]@Lines[[p]]@coords) - k
                            
                            coordData <- filteredStreets[[i]][[j]]@lines[[1]]@Lines[[p]]@coords
                            streetOfInt <- SpatialLines(list(Lines(list(Line(coordData[1:endIndex,])), "id")))
                            
                            tempBuffer <- gBuffer(streetOfInt, width=bufferWidth, byid=TRUE, capStyle = "flat")
                            
                            tempArr <- point.in.polygon(arrestData$x_coord_cd, arrestData$y_coord_cd,
                                                        tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                        tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,2])
                            tempOff <- point.in.polygon(offenseData$x_coord_cd, offenseData$y_coord_cd,
                                                        tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                        tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,2])
                            
                            numArrBIG <- sum(tempArr > 0)
                            numOffBIG <- sum(tempOff > 0)
                            
                            ##SMALL##
                            tempBuffer <- gBuffer(streetOfInt, width=50, byid=TRUE, capStyle = "flat")
                            tempArr <- point.in.polygon(arrestData$x_coord_cd, arrestData$y_coord_cd,
                                                        tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                        tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,2])
                            tempOff <- point.in.polygon(offenseData$x_coord_cd, offenseData$y_coord_cd,
                                                        tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                        tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,2])
                            numArrSMALL <- sum(tempArr > 0)
                            numOffSMALL <- sum(tempOff > 0)
                            
                            uniqID = paste0(filteredStreets[[i]][[j]]@data$unique_ID, "_", newNum)
                            l = list("precinct" = precNum,
                                    "indexOfOrigStreet" = j,
                                    "shorterStreet" = streetOfInt,
                                    "numArrestBig" = numArrBIG,
                                    "numArrestSmall" = numArrSMALL,
                                    "numOffBig" = numOffBIG,
                                    "numOffSmall" = numOffSMALL,
                                    "uniqueID" = uniqID)
                            returnList[[i]][[j]][[newNum]] = l
                            newNum = newNum+1
                            
                        }
                    }
                    
                }
                
                # (B) Start removing from LEFT to RIGHT
                # print("B")
                for (p in 2:(length(filteredStreets[[i]][[j]]@lines[[1]]@Lines) - 1)) {
                    
                    #TEST: SpatialLines(list(Lines(streetsBuffer400[[1]][[4]]@lines[[1]]@Lines[2:3], "id")))
                    #length(streetsBuffer400[[1]][[4]]@lines[[1]]@Lines)
                    # print(paste0("2. j:", j))
                    endIndex <- length(filteredStreets[[i]][[j]]@lines[[1]]@Lines)
                    
                    streetOfInt <- SpatialLines(list(Lines(filteredStreets[[i]][[j]]@lines[[1]]@Lines[p:endIndex], "id")))
                    
                    tempBuffer <- gBuffer(streetOfInt, width=bufferWidth, byid=TRUE, capStyle = "flat")
                    
                    tempArr <- point.in.polygon(arrestData$x_coord_cd, arrestData$y_coord_cd,
                                                tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,2])
                    tempOff <- point.in.polygon(offenseData$x_coord_cd, offenseData$y_coord_cd,
                                                tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,2])
                    
                    numArrBIG <- sum(tempArr > 0)
                    numOffBIG <- sum(tempOff > 0)
                    
                    ##SMALL##
                    tempBuffer <- gBuffer(streetOfInt, width=50, byid=TRUE, capStyle = "flat")
                    tempArr <- point.in.polygon(arrestData$x_coord_cd, arrestData$y_coord_cd,
                                                tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,2])
                    tempOff <- point.in.polygon(offenseData$x_coord_cd, offenseData$y_coord_cd,
                                                tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,2])
                    numArrSMALL <- sum(tempArr > 0)
                    numOffSMALL <- sum(tempOff > 0)
                    
                    uniqID = paste0(filteredStreets[[i]][[j]]@data$unique_ID, "_", newNum)
                    l = list("precinct" = precNum,
                            "indexOfOrigStreet" = j,
                            "shorterStreet" = streetOfInt,
                            "numArrestBig" = numArrBIG,
                            "numArrestSmall" = numArrSMALL,
                            "numOffBig" = numOffBIG,
                            "numOffSmall" = numOffSMALL,
                            "uniqueID" = uniqID)
                    returnList[[i]][[j]][[newNum]] = l
                    newNum = newNum + 1
                }
                
                # (C) Start removing from RIGHT to LEFT
                # print("C")
                for (p in 1:(length(filteredStreets[[i]][[j]]@lines[[1]]@Lines) - 2)) {
                    
                    #TEST: SpatialLines(list(Lines(streetsBuffer400[[1]][[4]]@lines[[1]]@Lines[2:3], "id")))
                    #length(streetsBuffer400[[1]][[4]]@lines[[1]]@Lines)
                    # print(paste0("3. j:", j))
                    endIndex <- length(filteredStreets[[i]][[j]]@lines[[1]]@Lines) - p
                    
                    streetOfInt <- SpatialLines(list(Lines(filteredStreets[[i]][[j]]@lines[[1]]@Lines[1:endIndex], "id")))
                    
                    tempBuffer <- gBuffer(streetOfInt, width=bufferWidth, byid=TRUE, capStyle = "flat")
                    
                    tempArr <- point.in.polygon(arrestData$x_coord_cd, arrestData$y_coord_cd,
                                                tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,2])
                    tempOff <- point.in.polygon(offenseData$x_coord_cd, offenseData$y_coord_cd,
                                                tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,2])
                    
                    numArrBIG <- sum(tempArr > 0)
                    numOffBIG <- sum(tempOff > 0)
                    
                    ##SMALL##
                    tempBuffer <- gBuffer(streetOfInt, width=50, byid=TRUE, capStyle = "flat")
                    tempArr <- point.in.polygon(arrestData$x_coord_cd, arrestData$y_coord_cd,
                                                tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,2])
                    tempOff <- point.in.polygon(offenseData$x_coord_cd, offenseData$y_coord_cd,
                                                tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,2])
                    numArrSMALL <- sum(tempArr > 0)
                    numOffSMALL <- sum(tempOff > 0)
                    
                    uniqID = paste0(filteredStreets[[i]][[j]]@data$unique_ID, "_", newNum)
                    l = list("precinct" = precNum,
                            "indexOfOrigStreet" = j,
                            "shorterStreet" = streetOfInt,
                            "numArrestBig" = numArrBIG,
                            "numArrestSmall" = numArrSMALL,
                            "numOffBig" = numOffBIG,
                            "numOffSmall" = numOffSMALL,
                            "uniqueID" = uniqID)
                    returnList[[i]][[j]][[newNum]] = l
                    newNum = newNum + 1
                }
            
            }
            
            #single line broken up
            else {
            
                #Make sure there are more than two coordinates to break up
                if(nrow(filteredStreets[[i]][[j]]@lines[[1]]@Lines[[1]]@coords) > 2) {
                    
                    # (D) Start removing from LEFT to RIGHT
                    # print("D")
                    for (p in 2:(nrow(filteredStreets[[i]][[j]]@lines[[1]]@Lines[[1]]@coords) - 1)) {
                    
                        #TEST: SpatialLines(list(Lines(streetsBuffer400[[1]][[4]]@lines[[1]]@Lines[2:3], "id")))
                        #length(streetsBuffer400[[1]][[4]]@lines[[1]]@Lines)
                        # print(paste0("Single 1. j:", j))
                        endIndex <- nrow(filteredStreets[[i]][[j]]@lines[[1]]@Lines[[1]]@coords)
                        
                        coordData <- filteredStreets[[i]][[j]]@lines[[1]]@Lines[[1]]@coords
                        streetOfInt <- SpatialLines(list(Lines(list(Line(coordData[p:endIndex,])), "id")))
                        
                        tempBuffer <- gBuffer(streetOfInt, width=bufferWidth, byid=TRUE, capStyle = "flat")
                        
                        tempArr <- point.in.polygon(arrestData$x_coord_cd, arrestData$y_coord_cd,
                                                    tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                    tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,2])
                        tempOff <- point.in.polygon(offenseData$x_coord_cd, offenseData$y_coord_cd,
                                                    tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                    tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,2])
                        
                        numArrBIG <- sum(tempArr > 0)
                        numOffBIG <- sum(tempOff > 0)
                        
                        ##SMALL##
                        tempBuffer <- gBuffer(streetOfInt, width=50, byid=TRUE, capStyle = "flat")
                        tempArr <- point.in.polygon(arrestData$x_coord_cd, arrestData$y_coord_cd,
                                                    tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                    tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,2])
                        tempOff <- point.in.polygon(offenseData$x_coord_cd, offenseData$y_coord_cd,
                                                    tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                    tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,2])
                        numArrSMALL <- sum(tempArr > 0)
                        numOffSMALL <- sum(tempOff > 0)
                        
                        uniqID = paste0(filteredStreets[[i]][[j]]@data$unique_ID, "_", newNum)
                        l = list("precinct" = precNum,
                                    "indexOfOrigStreet" = j,
                                    "shorterStreet" = streetOfInt,
                                    "numArrestBig" = numArrBIG,
                                    "numArrestSmall" = numArrSMALL,
                                    "numOffBig" = numOffBIG,
                                    "numOffSmall" = numOffSMALL,
                                    "uniqueID" = uniqID)
                        returnList[[i]][[j]][[newNum]] = l
                        newNum = newNum + 1
                        
                    }
                    
                    # (E) Start removing from RIGHT to LEFT
                    # print("E")
                    for (p in 1:(nrow(filteredStreets[[i]][[j]]@lines[[1]]@Lines[[1]]@coords) - 2)) {
                    
                        #TEST: SpatialLines(list(Lines(streetsBuffer400[[1]][[4]]@lines[[1]]@Lines[2:3], "id")))
                        #length(streetsBuffer400[[1]][[4]]@lines[[1]]@Lines)
                        # print(paste0("Single 2. j:", j))
                        endIndex <- nrow(filteredStreets[[i]][[j]]@lines[[1]]@Lines[[1]]@coords) - p
                        
                        coordData <- filteredStreets[[i]][[j]]@lines[[1]]@Lines[[1]]@coords
                        streetOfInt <- SpatialLines(list(Lines(list(Line(coordData[1:endIndex,])), "id")))
                        
                        tempBuffer <- gBuffer(streetOfInt, width=bufferWidth, byid=TRUE, capStyle = "flat")
                        
                        tempArr <- point.in.polygon(arrestData$x_coord_cd, arrestData$y_coord_cd,
                                                    tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                    tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,2])
                        tempOff <- point.in.polygon(offenseData$x_coord_cd, offenseData$y_coord_cd,
                                                    tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                    tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,2])
                        
                        numArrBIG <- sum(tempArr > 0)
                        numOffBIG <- sum(tempOff > 0)
                        
                        ##SMALL##
                        tempBuffer <- gBuffer(streetOfInt, width=50, byid=TRUE, capStyle = "flat")
                        tempArr <- point.in.polygon(arrestData$x_coord_cd, arrestData$y_coord_cd,
                                                    tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                    tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,2])
                        tempOff <- point.in.polygon(offenseData$x_coord_cd, offenseData$y_coord_cd,
                                                    tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                    tempBuffer@polygons[[1]]@Polygons[[1]]@coords[,2])
                        numArrSMALL <- sum(tempArr > 0)
                        numOffSMALL <- sum(tempOff > 0)
                        
                        uniqID = paste0(filteredStreets[[i]][[j]]@data$unique_ID, "_", newNum)
                        l = list("precinct" = precNum,
                                    "indexOfOrigStreet" = j,
                                    "shorterStreet" = streetOfInt,
                                    "numArrestBig" = numArrBIG,
                                    "numArrestSmall" = numArrSMALL,
                                    "numOffBig" = numOffBIG,
                                    "numOffSmall" = numOffSMALL,
                                    "uniqueID" = uniqID)
                        returnList[[i]][[j]][[newNum]] = l
                        newNum = newNum + 1
                    
                    }
                }
            
            } #else
        } #for(j)
    } #for(i)

    return(returnList)
}
  
LONG_STR_BY_WIDTH_PREC_BROKEN <- vector(mode = "list", length = 200)
for (i in seq(130, 10, -10)) {
    b = i/10
    print(paste0("Buffer:", b))
    load(paste0("test", b, ".dat"))
    LONG_STR_BY_WIDTH_PREC_BROKEN[[i]] = longStrBroke
}
