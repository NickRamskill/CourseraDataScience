rankall <- function(outcome, num = "best") {
        
        # Read outcome data
        setwd <- "/Users/nicholasramskill/Desktop/Courses/Data Science Specialization/Course2/Wk4/rprog%2Fdata%2FProgAssignment3-data"
        list.files()
        hospData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        # Check state and outcome are valid i.e. are they in the list
        validOutcomes <- c("heart attack", "heart failure", "pneumonia")
        if(!state %in% hospData$State) {
                stop("invalid state")
        } else if(!outcome %in% validOutcomes){
                stop("invalid outcome")
        }
        
        if(outcome == "heart attack"){
                col <- 11
        } else if(outcome == "heart failure"){
                col <- 17
        } else {
                col <- 23
        }
        
        # Summarise and rank data to only state/outcome of interest
        sts <- unique(hospData[,7])
        stsSum <- data.frame()
        
        for (i in 1:length(sts)){
                stateHospData <- hospData[hospData[,7]==sts[i],]
                sumData <- stateHospData[c(2,7,col)]
                sumData[,3] <- suppressWarnings(as.numeric(sumData[,3]))
                rankData <- sumData[order(sumData[,3],sumData[,1], na.last = NA),]
                
                # Select specified ranked hospital
                if(num == "best"){
                        extrData <- rankData[1,1:2]
                } else if(num == "worst"){
                        extrData <- rankData[nrow(rankData),1:2]
                } else {
                        if(num <= nrow(rankData)){
                                extrData <- rankData[num,1:2]
                        } else {
                                extrData <- c("NA",rankData[1,2])
                        }
                }
        
                stsSum <- rbind(stsSum,extrData)
                
        }
        
        colnames(stsSum)[1] <- "hospital"
        colnames(stsSum)[2] <- "state"
        return(stsSum)

}