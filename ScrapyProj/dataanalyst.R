
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plyr)



#### Industry and Language #### 
a = read.csv('/Users/Sishe/dataAnalyst.csv')
b = a[, c(5,12:20)]
IndusLang = b%>%gather(key = Industry, value = value)
IndusLang = IndusLang[IndusLang$value!="False",]
IndusLang[IndusLang==""] <- NA# fill all with NA
IndusLang <- na.omit(IndusLang) #remove all NA
colnames(IndusLang)[2] <- "ProgLang"
IndusLang <-IndusLang[-grep("\\$",IndusLang$Industry),] # remove all the $containing val in Industry col
IndusLang <-IndusLang%>%filter(Industry!='Unknown / Non-Applicable', Industry!='Unknown', Industry!='Unknown / Non-Applicable per year', Industry!='Company - Public', Industry != 'Company - Private')
IndusLang['Industry'] = revalue(IndusLang$Industry, c("Nonprofit Organization" = 'Non-Profit')) # change Non to next
IndusLang%>%group_by(Industry, ProgLang)%>%ggplot(aes(Industry, fill = ProgLang )) + geom_bar(position = 'stack') +theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#### END ####### 

### GRAPH 2: 
Location = a[, c(11,22,23)]
Location = Location[-116, ]
Location[Location==""] <- NA
Location <- na.omit(Location)
Location = Location %>% group_by(State) %>% summarise_each (funs(mean, mean), Low.Salary, High.Salary)
colnames(Location)[2] <- "Low.Mean.Salary"
colnames(Location)[3] <- "High.Mean.Salary"
Location$Avg <- (Location$Low.Mean.Salary + Location$High.Mean.Salary)/2
Location%>%group_by(State)%>%ggplot(aes(State,Avg, fill = State)) + geom_col(show.legend = FALSE) + labs(x = 'State', y = 'Mean Est.Salary')

### END##

### INDUSTRY VS SALARY
IndusJobs = a[,c(5,9)]
IndusJobs[IndusJobs==""] <- NA# fill all with NA
IndusJobs <- na.omit(IndusJobs) #remove all NA
IndusJobs <-IndusJobs[-grep("\\$",IndusJobs$Industry),]
IndusJobs['Industry'] = revalue(IndusJobs$Industry, c("Nonprofit Organization" = 'Non-Profit')) # change Non to next
IndusJobs <-IndusJobs%>%filter(Industry!='Unknown / Non-Applicable', Industry!='Unknown', Industry!='Unknown / Non-Applicable per year', Industry!='Company - Public', Industry != 'Company - Private')
IndusJobs%>%select(Industry)%>%group_by(Industry)%>%count()%>%ggplot(aes(Industry, freq, fill = Industry))+geom_col(show.legend = FALSE)+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


###END##

###### Num of jobs by Location ######
StateJobs = a[,c(9,11)]
StateJobs = StateJobs[-116, ]
StateJobs[StateJobs==""] <- NA
StateJobs <- na.omit(StateJobs)

StateJobs%>%select(State)%>%group_by(State)%>%count()%>%ggplot(aes(State, freq, fill = State))+geom_col(show.legend = FALSE)

####END### 




