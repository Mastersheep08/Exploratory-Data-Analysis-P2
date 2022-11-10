library(ggplot2)
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


SCC_list <- SCC %>% 
    dplyr::filter(base::grepl(x = EI.Sector,
                              pattern = "Coal|coal")) %>%
    dplyr::select(SCC, EI.Sector)

NEI_q4 <- base::subset(x = NEI, SCC %in% SCC_list$SCC)

NEI_q4_v2 <- base::merge(NEI_q4, SCC_list)

NEI_q4_v3 <- NEI_q4_v2 %>%
    dplyr::group_by(year, EI.Sector) %>%                                   
    dplyr::summarise(Total = base::sum(Emissions)) %>%                      
    dplyr::mutate(EI.Sector = base::gsub(pattern = "Fuel Comb - | - Coal",  
                                         replacement =  "",                
                                         x = EI.Sector))

NEI_q4_v3_total <- NEI_q4_v3 %>%
    dplyr::summarise(Total = base::sum(Total)/1000)

grDevices::png(filename = "plot4.png", height = 480, width = 800)

    ggplot2::ggplot(data = NEI_q4_v3,
                    ggplot2::aes(x = year,
                                 y = Total/1000,      
                                 fill = EI.Sector)) +
        
        ggplot2::geom_bar(position = "stack", stat = "identity") + 
        
        ggplot2::geom_text(data = NEI_q4_v3_total,
                           ggplot2::aes(x = year,
                                        label = base::format(x = Total,
                                                             nsmall = 1, digits = 1), # Rounding the number.
                                        y = Total,
                                        fill = NULL),
                           nudge_y = 10) + # Distance to the point.
        
        ggplot2::scale_x_discrete(limits = c(1999, 2002, 2005, 2008)) +
        
        ggplot2::labs(title = base::expression('Coal Combustion PM'[2.5] ~ ' in the United States')) + 
        
        ggplot2::xlab("Year") +
        
        ggplot2::ylab(base::expression('PM'[2.5] ~ 'Emissions (10' ^3 ~ 'tons)')) +
        
        ggplot2::theme(legend.position = "bottom",
                       legend.title.align = 0.5,
                       plot.title = ggplot2::element_text(hjust = 0.5)) +
        
        ggplot2::guides(fill = ggplot2::guide_legend(title = ""))

grDevices::dev.off()
