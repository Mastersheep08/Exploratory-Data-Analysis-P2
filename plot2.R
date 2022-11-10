library(magrittr)
library(tidyverse)


if(!base::file.exists("data")) {
    base::dir.create("data")
}


if(!base::file.exists("./data/FNEI_data.zip")){
    utils::download.file(url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                         destfile = "./data/FNEI_data.zip")
}

if(!base::file.exists("./data/unzipped/Source_Classification_Code.rds") | !base::file.exists("./data/unzipped/summarySCC_PM25.rds")){
    utils::unzip(zipfile = "./data/FNEI_data.zip",
                 exdir = "./data/unzipped/",
                 list = FALSE,
                 overwrite = TRUE)
}


NEI <- base::readRDS("./data/unzipped/summarySCC_PM25.rds")
SCC <- base::readRDS("./data/unzipped/Source_Classification_Code.rds")


NEI_q2 <- base::subset(x = NEI, NEI$fips == "24510")

plot_2_data <- NEI_q2 %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(total = base::sum(Emissions))


base::with(data = plot_2_data, {
    
    
    grDevices::png(filename = "plot2.png", height = 480, width = 800)  
    
        
        par(oma = c(1,1,1,1))
        
        
        p <- graphics::barplot(height = total, name = year,
                                
                               
                               main = base::expression('Total PM'[2.5] ~ ' in Baltimore City'),
                                
                               ylab = base::expression('PM'[2.5] ~ 'Emissions (tons)'),
                                                               
                               xlab = "Year")
        
        graphics::text(x = p,
                       y = total - 100,
                       label = base::format(total,
                                            nsmall = 1,   # Rounding the number.
                                            digits = 1))
    
    grDevices::dev.off()      
})
