library(ggplot2)
library(magrittr)
library(tidyverse)
library(cowplot)


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

NEI_q6 <- base::subset(x = NEI,
                       NEI$fips %in% c("24510","06037") & NEI$type == "ON-ROAD")

city_data <- base::data.frame(fips = c("24510","06037"),                    
                              city = c("Baltimore, MD","Los Angeles, CA"))  

NEI_q6_v2 <- base::merge(NEI_q6, city_data)

plot_6_data <- NEI_q6_v2 %>%
    dplyr::group_by(year, city) %>%
    dplyr::summarise(Total = base::sum(Emissions))


grDevices::png(filename = "plot6.png", height = 480, width = 800)

   
    p61 <- ggplot2::ggplot(data = plot_6_data %>% dplyr::filter(city == "Baltimore, MD"), # Filtering only data from Baltimore.
                           ggplot2::aes(x = year, y = Total)) + 
        
        
        ggplot2::geom_line(lwd = 1, color = "deepskyblue") + 
                
        ggplot2::labs(title = base::expression('Vehicle Emissions PM'[2.5] ~ ' in Baltimore (MD)')) +
                
        ggplot2::scale_x_discrete(limits = c(1999, 2002, 2005, 2008)) + 
                
        ggplot2::xlab("Year") + 
                
        ggplot2::ylab(base::expression('Total PM'[2.5] ~ 'Emissions (tons)'))

    
    p62 <- ggplot2::ggplot(data = plot_6_data %>% dplyr::filter(city == "Los Angeles, CA"), # Filtering only data from Los Angeles County.
                           ggplot2::aes(x = year, y = Total)) +
                
        ggplot2::geom_line(lwd = 1, color = "coral") +
                
        ggplot2::labs(title = base::expression('Vehicle Emissions PM'[2.5] ~ ' in Los Angeles County (CA)')) +
                
        ggplot2::scale_x_discrete(limits = c(1999, 2002, 2005, 2008)) + 
                
        ggplot2::xlab("Year") + 
                
        ggplot2::ylab(base::expression('Total PM'[2.5] ~ 'Emissions (tons)'))
    
    p612 <- cowplot::plot_grid(p61, p62, labels = "")
    
    base::print(p612)

grDevices::dev.off()
