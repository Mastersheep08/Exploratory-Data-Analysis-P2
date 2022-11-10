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


#plot_1_data <- base::with(data = NEI,
#                          base::tapply(X = Emissions, # The tapply will create
#                                       INDEX = year,  # a total emissions of
#                                       FUN = sum))    # each year.


plot_1_data <- NEI %>%
     dplyr::group_by(year) %>%
     dplyr::summarise(total = base::sum(Emissions))


base::with(data = plot_1_data, {
    
        grDevices::png(filename = "plot1.png", height = 480, width = 800)  
    
        
        par(oma = c(1,1,1,1))
        
        
        p <- graphics::barplot(height = total/1000000, name = year, # Re-scaling to million.
                                
                               # Adding title.
                               main = base::expression('Total PM'[2.5] ~ ' in the United States'),
                     
                               # Adding y-axis label.
                               ylab = base::expression('PM'[2.5] ~ 'Emissions (10' ^6 ~ 'tons)'),
                                
                               # Adding x-axis label.
                               xlab = "Year");
        
        
        text(x = p,
             y = total/1000000 - 0.5,            # Re-scaling to million.
             label = format(total/1000000, # Re-scaling to million.
                                            nsmall = 1,          # Rounding the number.
                                            digits = 1))
    
    
    grDevices::dev.off()      
})
