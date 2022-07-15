# Returns a list of correlation values for all the ids that have
# number of complete cases above the threshold

corr <- function(directory, threshold = 0){
	# Create an empty numeric vector to store correlations
	corrlist <- numeric()
	filenames <- sprintf("%03d.csv", 1:332)
	# Calculate correlation for every id where complete cases is above threshold
	for (i in filenames){
		data <- read.csv(paste(directory, i, sep = '/'))
		nob = sum(complete.cases(data))
		# Keep only rows with complete cases
		data = data[complete.cases(data), ]
		if (nob > threshold){
			corrl <- cor(data[ , 2], data[ , 3], use = "complete.obs")
			corrlist <- append(corrlist, corrl)
		}
	}
	corrlist
}