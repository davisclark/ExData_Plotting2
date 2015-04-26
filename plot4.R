plot4 <- function() {
        
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
        scc <- readRDS("Source_Classification_Code.rds")
        
        merged<-merge(nei,scc,by="SCC")
        names(merged)<-gsub("\\.","",names(merged))
        names(merged)<-tolower(names(merged))
        coal<-merged[grep("Coal", merged$shortname), ]
        coalMelt <- melt(coal, measure.vars="emissions")
        coalTotal <- dcast(coalMelt, year ~ variable, sum)
        coalTotal$year <- as.factor(coalTotal$year)
        
        ### Plot the data
        ggplot(coalTotal, aes(year, emissions)) + geom_bar(stat="identity") + ggtitle("Total Coal Combustion-related Emissions (1999 - 2008)")
        
        ### Save plot as PNG
        ggsave(filename="plot4.png", dpi=150)
}

plot4()