plot2 <- function() {
        
        ### Configure environment
        library(reshape2)
        library(dplyr)
        fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
        
        ### Verify data in working directory; download if missing
        if(!("exdata_data_NEI_data.zip" %in% list.files())) {
                download.file(fileUrl, "exdata_data_NEI_data.zip", mode="wb")
                unzip("exdata_data_NEI_data.zip")
        }
        
        ### Read in and process data 
        nei <- readRDS("summarySCC_PM25.rds")
        
        names(nei)<-gsub("\\.","",names(nei))
        names(nei)<-tolower(names(nei))
        
        baltimore<-nei[(nei$fips=="24510"),]
        baltimoreMelt <- melt(baltimore, measure.vars=c("emissions"))
        baltimoreTotal <- dcast(baltimoreMelt, year ~ variable, sum)
       
        
        ### Open PNG Graphics device
        png(filename="plot2.png",
            width=480, height=480,units="px",pointsize=11,bg="white")
        
        ### Plot the data
        barplot(baltimoreTotal$emissions, names.arg=baltimoreTotal$year,
                main="Total Baltimore City Emissions (1999 - 2008)", 
                xlab="year", ylab="emissions")
        
        ### Close connection
        dev.off()
}

plot2()