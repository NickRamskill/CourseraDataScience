best <- function(state, outcome){
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
        
        # Return hospital w. best 30 day death rate
        
        stateHospData <- hospData[hospData[,7]==state,]
        
        if(outcome == "heart attack"){
                heartAttack <- suppressWarnings(as.numeric(stateHospData[,11]))
                n <- which(heartAttack == min(heartAttack, na.rm = TRUE))
                hospName <- stateHospData$Hospital.Name[n]
                
        } else if(outcome == "heart failure"){
                heartFailure <- suppressWarnings(as.numeric(stateHospData[,17]))
                n <- which(heartFailure == min(heartFailure, na.rm = TRUE))
                hospName <- stateHospData$Hospital.Name[n]
                
        } else {
                pneumonia <- suppressWarnings(as.numeric(stateHospData[,23]))
                n <- which(pneumonia == min(pneumonia, na.rm = TRUE))
                hospName <- stateHospData$Hospital.Name[n]
        }
 
        return(hospName)
                       
}