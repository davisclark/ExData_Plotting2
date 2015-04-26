run_analysis <- function() {
        
        library(reshape2)
        library(dplyr)
        
        url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
        
        if(!("UCI HAR Dataset" %in% list.files())) {
                download.file(url, "directory.zip", mode="wb")
                unzip("directory.zip")
        }
        
        The zip file contains two files:
                
                PM2.5 Emissions Data (summarySCC_PM25.rds): This file contains a data frame with all of the PM2.5 emissions data for 1999, 2002, 2005, and 2008. For each year, the table contains number of tons of PM2.5 emitted from a specific type of source for the entire year. Here are the first few rows.
        ##     fips      SCC Pollutant Emissions  type year
        ## 4  09001 10100401  PM25-PRI    15.714 POINT 1999
        ## 8  09001 10100404  PM25-PRI   234.178 POINT 1999
        ## 12 09001 10100501  PM25-PRI     0.128 POINT 1999
        ## 16 09001 10200401  PM25-PRI     2.036 POINT 1999
        ## 20 09001 10200504  PM25-PRI     0.388 POINT 1999
        ## 24 09001 10200602  PM25-PRI     1.490 POINT 1999
        
        
        .fips: A five-digit number (represented as a string) indicating the U.S. county 
        
        
        .SCC: The name of the source as indicated by a digit string (see source code classification table)
        
        
        .Pollutant: A string indicating the pollutant
        
        
        .Emissions: Amount of PM2.5 emitted, in tons
        
        
        .type: The type of source (point, non-point, on-road, or non-road)
        
        
        .year: The year of emissions recorded
        
        
        Source Classification Code Table (Source_Classification_Code.rds): This table provides a mapping from the SCC digit strings in the Emissions table to the actual name of the PM2.5 source. The sources are categorized in a few different ways from more general to more specific and you may choose to explore whatever categories you think are most useful. For example, source "10100101" is known as "Ext Comb /Electric Gen /Anthracite Coal /Pulverized Coal".
        
        You can read each of the two files using the readRDS() function in R. For example, reading in each file can be done with the following code:
                ## This first line will likely take a few seconds. Be patient!
                NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        
        
        as long as each of those files is in your current working directory (check by calling dir() and see if those files are in the listing).
        
        
        The overall goal of this assignment is to explore the National Emissions Inventory database and see what it say about fine particulate matter pollution in the United states over the 10-year period 1999-2008. You may use any R package you want to support your analysis.
        
        Questions
        
        You must address the following questions and tasks in your exploratory analysis. For each question/task you will need to make a single plot. Unless specified, you can use any plotting system in R to make your plot.
        
        1.Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
        NEIMelt <- melt(NEI, measure.vars=c("Emissions"))
        emissionsBySource <- dcast(NEIMelt, year ~ variable, sum)
        barplot(emissionsBySource$Emissions, names.arg=emissionsBySource$year)
        2.Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.
        baltimore<-NEI[(NEI$fips=="24510"),]
        baltimoreMelt <- melt(baltimore, measure.vars=c("Emissions"))
        baltimoreTotal <- dcast(baltimoreMelt, year ~ variable)
        barplot(baltimoreTotal$Emissions, names.arg=baltimoreTotal$year)
        3.Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make a plot answer this question.
        library(ggplot2)
        baltimoreBySource<-dcast(baltimoreMelt, year + type ~ variable, sum)
        baltimoreBySource$year <- as.factor(baltimoreBySource$year)
        ggplot(baltimoreBySource, aes(year, Emissions)) + geom_bar(stat="identity") + facet_grid(type ~ .)
        4.Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?
        coal<-grep("Coal", mergedMelt$SCCLevelThree)
        coalMelt <- mergedMelt[coal, ]
        coalTotal <- dcast(coalMelt, year ~ variable, sum)
        ggplot(coalTotal, aes(year, Emissions)) + geom_bar(stat="identity")
        5.How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City? 
        
        
        6.Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?
        
        