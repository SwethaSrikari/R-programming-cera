# Returns a dataframe with the id and number of complete cases

complete <- function(directory, ids){
	# Create an empty dataframe to store id and number of complete cases
	df <- data.frame("id" = 0, "nobs" = 0)
	filenames <- sprintf("%03d.csv", ids)
	# For every id, calculate number of complete cases and store in a dataframe
	for (i in seq_along(ids)){
		data <- read.csv(paste(directory, filenames[i], sep = '/'))
		nob = sum(complete.cases(data))
		df <- rbind(df, data.frame("id" = i, "nobs" = nob))
		# Delete first row as it is just zeros
		dft <- df[1:length(ids)+1, ]
		# Rename th index values after deleting
		rownames(dft) <- 1:length(ids)
	}
	dft
}