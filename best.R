# Function returns the hospital with the lowest 30-day mortality rate
# for the specified outcome in that state

best <- function(state, outcome){
	# Read outcome data
	data <- read.csv("outcome-of-care-measures.csv",
	colClasses = "character")

	# Check the validity of outcome
	valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
	if (outcome %in% valid_outcomes){print("Valid outcome")}
	else{stop("Invalid outcome")}

	valid_states <- names(table(data$State))
	if (state %in% valid_states){print("Valid state")}
	else{stop("Invalid state")}

	# if outcome is heart attack
	if(outcome == "heart attack"){
		names(data)[13] <- 'heartattack'
		usstate <- data[which(data$State == state), ]
		usstate$heartattack <- as.numeric(usstate$heartattack)
		hospital <- usstate[usstate$heartattack == min(usstate$heartattack, na.rm = TRUE), c(7, 2)]
		best_hospital <- as.list(hospital[order(hospital$Hospital.Name), ][2])[[1]][1]
		print(best_hospital)

	}

	# if outcome is heart failure
	if(outcome == "heart failure"){
		names(data)[19] <- 'heartfailure'
		usstate <- data[which(data$State == state), ]
		usstate$heartfailure <- as.numeric(usstate$heartfailure)
		hospital <- usstate[usstate$heartfailure == min(usstate$heartfailure, na.rm = TRUE), c(7, 2)]
		best_hospital <- as.list(hospital[order(hospital$Hospital.Name), ][2])[[1]][1]
		print(best_hospital)

	}

	# if outcome is pneumonia
	if(outcome == "pneumonia"){
		names(data)[25] <- 'pneumonia'
		usstate <- data[which(data$State == state), ]
		usstate$pneumonia <- as.numeric(usstate$pneumonia)
		hospital <- usstate[usstate$pneumonia == min(usstate$pneumonia, na.rm = TRUE), c(7, 2)]
		best_hospital <- as.list(hospital[order(hospital$Hospital.Name), ][2])[[1]][1]
		print(best_hospital)

	}

}