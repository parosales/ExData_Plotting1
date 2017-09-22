plot2 <- function(working_directory, data_file_name) {
     # -------------------------------------------- Setup Environment -------------------------------------------- #
     library(dplyr)
     setwd(working_directory)
     
     # -------------------------------------------- Prepare Source Data -------------------------------------------- #
     
     # Upload source data
     household_power_consumption_df <- read.csv2(data_file_name, na.strings=c('?'))
     
     colnames(household_power_consumption_df)[1] <- 'date_as_str'
     colnames(household_power_consumption_df)[2] <- 'time_as_str'
     
     # convert strings to date and time 
     household_power_consumption_df <- mutate(household_power_consumption_df, date_as_date <- as.Date(date_as_str, "%d/%m/%Y"))
     colnames(household_power_consumption_df)[10] <- 'date'
     
     time_df <- strptime(paste(household_power_consumption_df$date_as_str, household_power_consumption_df$time_as_str), '%d/%m/%Y %H:%M:%S')
     household_power_consumption_df <- cbind(household_power_consumption_df, time_df)
     colnames(household_power_consumption_df)[11] <- 'time'
     
     # extract data subset to be analyzed
     analyzed_households_df <- subset(household_power_consumption_df, as.Date("2007-02-01", '%Y-%m-%d') == date | date == as.Date("2007-02-02", '%Y-%m-%d') )
     
     # cast data types as necessary
     analyzed_households_df <- mutate(analyzed_households_df, Global_active_power = as.numeric(as.character(Global_active_power)))
     
     # -------------------------------------------- Plot Chart -------------------------------------------- #
     
     with(analyzed_households_df, plot(time, Global_active_power, xlab = '', ylab = 'Global Active Power (kilowatts)', type = 'l'))
     png(width = 480, height = 480)
     
     dev.copy(png, file = 'Plot 2.png')
     dev.off()
}     