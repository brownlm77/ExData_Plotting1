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

plot1 <- function(df) {
     # Name the plot
     png(file = "plot1.png")

     with(df, hist(Global_active_power, 
                   main='Global Active Power', 
                   col='red', 
                   xlab='Global Active Power (kilowatts)', 
                   ylab='Frequency'
                   #xlim = c(0, 6),
                   #ylim = c(0, 1200)
                   ))

     # Save the file.
     dev.off()
}


unitTest <- function() {

     filename <- 'data/household_power_consumption.txt'
     df = processFile(filename)
     print(nrow(df))
     plot1(df)
     
}