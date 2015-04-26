plot1 <- function() {
        
        ### Configure environment
        library(reshape2)
        library(dplyr)
        library(lubridate)
        fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
        
        ### Verify data in working directory; download if missing
        if(!("exdata_data_NEI_data.zip" %in% list.files())) {
                download.file(fileUrl, "exdata_data_NEI_data.zip", mode="wb")
                unzip("exdata_data_NEI_data.zip")
        }
        
        ### Read-in data 
        nei <- readRDS("summarySCC_PM25.rds")
        
        names(nei)<-gsub("\\.","",names(nei))
        names(nei)<-tolower(names(nei))
        
        neiMelt <- melt(nei, measure.vars=c("emissions"))
        totalEmissions <- dcast(neiMelt, year ~ variable, sum)
        
        ### Open PNG Graphics device
        png(filename="plot1.png",
            width=480, height=480,units="px",pointsize=11,bg="white")
        
        ### Plot the data
        barplot(totalEmissions$emissions, 
                names.arg=totalEmissions$year,
                main="Total U.S. Emissions (1999 - 2008)", 
                xlab="year", ylab="emissions")
        options(scipen=10)
        
        ### Close connection
        dev.off()
}

plot1()