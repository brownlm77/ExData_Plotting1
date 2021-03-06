#  plot4.R
#
#  Author:  Lawrence Brown
#  Date:    6 Nov 2020
#
#  Scripts for generating plots
#
#  Source data:
#     Electric power consumption [20Mb]
#     https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
#


#  processFile
#
#  Reads power consumption text file and returns data frame
#
#  Arguments: 
#  
#    filepath     name of file to read
#
#  Return:  data frame of energy values
#
#  Details: 
#  
#  Only keeps data for the dates 1/2/2007 and 2/2/2007
#  A new column, datetime, is added that binds the Date and Time as
#  a POSIXlt date


processFile <- function(filepath) {
     con = file(filepath, "rt")
     on.exit(close(con))
     
     # Read Header line 
     line = readLines(con, n = 1)
     df = read.table(text = line, header=TRUE, sep=';', na.strings = '?' )
     header = colnames(df)
     
     line = readLines(con, n = 1)
     while ( !(length(line) == 0)) {
          # Only add data if dates are 1/2/2007 and 2/2/2007
          if (grepl('1/2/2007',line) || grepl('2/2/2007',line)) {
               dfnew = read.table(text = line, header=FALSE, sep=';', na.strings = '?' ) 
               df <- rbind(df, dfnew) 
          }
          line = readLines(con, n = 1)

          # Kick out if past 2/2/2007
          if (grepl('3/2/2007',line)) {
               break
          }
     }
     colnames(df) <- header
     
     # Add new datetime column with Date/Time converted from text
     df$datetime <- strptime(paste(df$Date, df$Time), "%d/%m/%Y %H:%M:%S")
     
     return(df)
}

#  plot4
#
#  Multi-graph for plotting for plots of energy data
#
#  Arguments: 
#  
#    df        data frame of energy values
#    
#    tofile    boolean flag to indicate if print to file (default is TRUE)
#
#  Details: 
#
#    Plot file is plot4.png


plot4 <- function(df, tofile=TRUE) {
     # Name the plot
     if (tofile) {
        png(file = "plot4.png")
     }
     
     par(mfrow=c(2,2))
     
     # Upper left, plot Global Active Power
     with(df, plot(x=datetime,
                   y=Global_active_power,
                   ylab='Global Active Power', 
                   xlab='',
                   type = "l"
     ))      
     
     # Upper Right, plot Voltage
     with(df, plot(x=datetime,
                   y=Voltage,
                   ylab='Voltage', 
                   xlab='datetime',
                   type = "l"
     ))     
     
     # Lower left, Global Active Power 
     with(df, { plot(x=datetime,
                     y=Sub_metering_1,
                     ylab='Energy sub metering', 
                     xlab='',
                     type = "l")
               lines(x=datetime,
                     y=Sub_metering_2,
                     col='red',
                     type="l")
               lines(x=datetime,
                     y=Sub_metering_3,
                     col='blue')
              legend("topright",
                     legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
                     col=c("black", "red","blue"),
                     ncol=1,
                     bty="n",
                     lty=1)
             })
     
     # Lower Right, plot Global_reactive_power
     with(df, plot(x=datetime,
                   y=Global_reactive_power,
                   ylab='Global_reactive_power', 
                   xlab='datetime',
                   type = "l"
          ))       
     
     
     # Save the plot
     if (tofile) {
         dev.off()
     }
}

# unitTest
#
# Simple unit test of scripts to generate plot. 

unitTest <- function() {
     filename <- 'data/household_power_consumption.txt'
     df = processFile(filename)
     plot4(df)
}