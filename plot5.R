plot5 <- function() {
        
        ### Configure environment
        library(reshape2)
        library(dplyr)
        library(ggplot2)
        fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
        
        ### Verify data in working directory; download if missing
        if(!("exdata_data_NEI_data.zip" %in% list.files())) {
                download.file(url, "exdata_data_NEI_data.zip", mode="wb")
                unzip("exdata_data_NEI_data.zip")
        }
        
        ### Read-in data 
        nei <- readRDS("summarySCC_PM25.rds")
        scc <- readRDS("Source_Classification_Code.rds")
        
        merged<-merge(nei,scc,by="SCC")
        names(merged)<-gsub("\\.","",names(merged))
        names(merged)<-tolower(names(merged))
        
        baltimore<-merged[(merged$fips=="24510"),]
        baltimoreMelt <- melt(baltimore, measure.vars=c("emissions"))
        
        vehicles<-unique(baltimoreMelt[grep("Mobile", baltimoreMelt$eisector),]$scclevelthree)[1:13]
        vehicleMelt<-baltimoreMelt[(baltimoreMelt$scclevelthree %in% vehicles),]
        vehicleTotal <- dcast(vehicleMelt, year ~ variable, sum)
        vehicleTotal$year <- as.factor(vehicleTotal$year)
        
        ### Plot the data
        ggplot(vehicleTotal, aes(year, emissions)) + geom_bar(stat="identity") + ggtitle("Total Baltimore City Motor Vehicle Emissions (1999 - 2008)")
        
        ### Save plot as PNG
        ggsave(filename="plot5.png", dpi=150)
}

plot5()