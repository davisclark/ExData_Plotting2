plot6 <- function() {
        
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
        
        baltimoreVehicles<-unique(baltimoreMelt[grep("Mobile", baltimoreMelt$eisector),]$scclevelthree)[1:13]
        baltimoreVehicleMelt<-baltimoreMelt[(baltimoreMelt$scclevelthree %in% baltimoreVehicles),]
        baltimoreVehicleTotal <- dcast(baltimoreVehicleMelt, year ~ variable, sum)
        baltimoreVehicleTotal$year <- as.factor(baltimoreVehicleTotal$year)
        baltimoreVehicleTotal <- mutate(baltimoreVehicleTotal, location = "Baltimore City")
        
        la<-merged[(merged$fips=="06037"),]
        laMelt <- melt(la, measure.vars=c("emissions"))
        
        laVehicles<-unique(laMelt[grep("Mobile", laMelt$eisector),]$scclevelthree)[1:13]
        laVehicleMelt<-laMelt[(laMelt$scclevelthree %in% laVehicles),]
        laVehicleTotal <- dcast(laVehicleMelt, year ~ variable, sum)
        laVehicleTotal$year <- as.factor(laVehicleTotal$year)
        laVehicleTotal <- mutate(laVehicleTotal, location = "Los Angeles County")
        
        vehicleTotal <- rbind(baltimoreVehicleTotal, laVehicleTotal)
        
        ### Plot the data
        ggplot(vehicleTotal, aes(year, emissions)) + geom_bar(stat="identity") + facet_grid(. ~ location) + ggtitle("Motor Vehicle Emissions (1999 - 2008)")
        
        ### Save plot as PNG
        ggsave(filename="plot6.png", width=6, dpi=150)
       
}

plot6()