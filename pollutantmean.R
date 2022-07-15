# Returns the mean of the pollutant across all the specified ids

pollutantmean <- function(directory, pollutant, id = 1:332) {
    # Run library(dplyr) for this code to work

    # All the filenames in id argument
    filenames <- sprintf("%03d.csv", id)
    # Directory to all the files in filenames
    filenames <- paste(directory, filenames, sep="/")
    # Merge the rows of all the files to form a big dataframe
    # %>% takes the output of left-hand statement and feeds as
    # input to right-hand statement
    ldf <- lapply(filenames, read.csv) %>% bind_rows()
    # Calculate the mean of the pollutant ignoring NA values
    pmean <- mean(ldf[, pollutant], na.rm = TRUE)
    pmean
    }