rankhospital <- function(state, outcome, num = "best") {
        
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
        stateHospData <- hospData[hospData[,7]==state,]
        sumData <- stateHospData[c(2,col)]
        sumData[,2] <- suppressWarnings(as.numeric(sumData[,2]))
        colnames(sumData)[2] <- "Rate"
        rankData <- sumData[order(sumData[,2],sumData[,1], na.last = NA),]
        
        # Select specified ranked hospital
        if(num == "best"){
                hospName <- rankData[1,1]
        } else if(num == "worst"){
                hospName <- rankData[nrow(rankData),1]
        } else {
                if(num <= nrow(rankData)){
                        hospName <- rankData[num,1]
                } else {
                        return("NA")
                }
        }
        
        return(hospName)
}