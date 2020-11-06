#
#

readData <-function(fname, header=FALSE, nrow=-1) {
     con <- file(fname, open="rt")
     on.exit(close(con))
     df = read.table(con, header=header, nrow=nrow, sep=';', na.strings = '?' ) 
     return(df)
}


processFile <- function(filepath, nrow = 69520) {
     con = file(filepath, "rt")
     on.exit(close(con))
     
     # Read Header line 
     line = readLines(con, n = 1)
     df = read.table(text = line, header=TRUE, sep=';', na.strings = '?' )
     header = colnames(df)
     
     count <- 1
     line = readLines(con, n = 1)
     #while ( !((length(line) == 0) || (count >= nrow) )) {
     while ( !(length(line) == 0)) {
          # Only add data if dates are 1/2/2007 and 2/2/2007
          if (grepl('1/2/2007',line) || grepl('2/2/2007',line)) {
               dfnew = read.table(text = line, header=FALSE, sep=';', na.strings = '?' ) 
               df <- rbind(df, dfnew) 
          }
          line = readLines(con, n = 1)
          count = count + 1 
          # Kick out if past 2/2/2007
          if (grepl('3/2/2007',line)) {
               break
          }
     }
     colnames(df) <- header
     return(df)
}

plot3 <- function(df) {
     # Name the plot
     #png(file = "plot3.png")
     
     df$x <- strptime(paste(df$Date, df$Time), "%d/%m/%Y %H:%M:%S")
     print(head(df))
     with(df, {plot(x=df$x,
                   y=df$Sub_metering_1,
                   ylab='Energy sub metering', 
                   xlab='',
                   type = "l")

               lines(x=df$x,
                     y=df$Sub_metering_2,
                     col='red',
                     type="l")
               lines(x=df$x,
                     y=df$Sub_metering_3,
                     col='blue')
               #add a legend in top left corner of chart at (x, y) coordinates = (1, 19)
               legend("topright",
                      legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
                      col=c("black", "red","blue"),
                      ncol=1,
                      lty=1)
     
          })     
     
     
     # Save the file.
     #dev.off()
}


unitTest <- function() {
     
     filename <- 'data/household_power_consumption.txt'
     df = processFile(filename)
     print(nrow(df))
     #plot2(df)
     return(df)
}