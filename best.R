##Programming assiment 3

## Function best()
## input: state, outcome // output: hospital name
## This function returns the hospital name of the "best" 30-days mortality rate
## hostpital. This ranking is given for the input state.
## 



best<- function (state,outcome) {
        data<- read.csv("outcome-of-care-measures.csv")
        data[,11]<- as.numeric(data[,11])
        data[,17]<- as.numeric(data[,17])
        data[,23]<- as.numeric(data[,23])
        
        
        outcomeList=c("heart attack", "heart failure", "pneumonia") #valid outcome list
        if (!(state %in% data$State)) {
                
                stop("invalid state")
        }
        
        if (!any(outcome==outcomeList)){
                stop("invalid outcome")
        }
        
        dataByState= subset(data,State==state)
        outcomeIndex <- c(11,17,23) #column index for the 30 day mortality rate
        
        index<-outcomeIndex[outcome==outcomeList]
        

        minFound<-match(min(dataByState[,index],na.rm=TRUE),dataByState[,index])
        
        print(as.character(dataByState$Hospital.Name[minFound]))
        
        
        
}

