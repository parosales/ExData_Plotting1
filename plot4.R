plot4 <- function(working_directory, data_file_name) {
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
     analyzed_households_df <- mutate(
          analyzed_households_df, 
          Global_active_power = as.numeric(as.character(Global_active_power)),
          Global_reactive_power = as.numeric(as.character(Global_reactive_power)),
          Sub_metering_1 = as.numeric(as.character(Sub_metering_1)),
          Sub_metering_2 = as.numeric(as.character(Sub_metering_2)),
          Sub_metering_3 = as.numeric(as.character(Sub_metering_3)),
          Voltage = as.numeric(as.character(Voltage))
     )
     
     analyzed_households_df
     
     # -------------------------------------------- Plot Chart -------------------------------------------- #
     # set panel
     par(mfrow = c(2,2))
     
     # Plot 1
     with(analyzed_households_df, plot(time, Global_active_power, xlab = '', ylab = 'Global Active Power', type = 'l', col = 'black'))
     
     # Plot 2
     with(analyzed_households_df, plot(time, Voltage, xlab = 'datetime', ylab = 'Voltage', type = 'l', col = 'black'))
     
     # Plot 3
     with(analyzed_households_df, plot(time, Sub_metering_1, xlab = '', ylab = 'Energy sub metering', type = 'l', col = 'black'))
     with(analyzed_households_df, lines(time, Sub_metering_2, col = 'red'))
     with(analyzed_households_df, lines(time, Sub_metering_3, col = 'blue'))
     legend (x = 'topright', legend = c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'), col = c('black', 'red', 'blue'), lty = 1)
     
     # Plot 4
     with(analyzed_households_df, plot(time, Global_reactive_power, xlab = 'datetime', type = 'l', col = 'black'))
     
     # save to png file
     #png(width = 480, height = 480)
     dev.copy(png, file = 'Plot 4.png')
     dev.off()
}     