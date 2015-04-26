plot3 <- function() {
        
        ### Configure environment
        library(reshape2)
        library(dplyr)
        library(ggplot2)
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
        
        baltimore<-nei[(nei$fips=="24510"),]
        baltimoreMelt <- melt(baltimore, measure.vars=c("emissions"))
        baltimoreBySource<-dcast(baltimoreMelt, year + type ~ variable, sum)
        baltimoreBySource$year <- as.factor(baltimoreBySource$year)
        
        ### Plot the data
        ggplot(baltimoreBySource, aes(year, emissions)) + geom_bar(stat="identity") + facet_grid(type ~ .) + ggtitle("Total Baltimore City Emissions by Source Type (1999 - 2008)")
        ### Save plot as PNG
        ggsave(filename="plot3.png", dpi=150)
}

plot3()