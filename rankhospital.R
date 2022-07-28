rankhospital <- function(state, outcome, num = 'best'){
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
		names(data)[11] <- 'heartattack'
		usstate <- data[which(data$State == state), c(2, 11)]
		usstate$heartattack <- as.numeric(usstate$heartattack)
		usstate_hosp <- usstate[order(usstate$heartattack, usstate$Hospital.Name), ]
		if (num != 'worst' & num != 'best' & num > nrow(usstate_hosp)){return(NA)}
		usstate_hosp$rank <- 1 : nrow(usstate_hosp)
		if (num == 'best'){num = 1}
		if (num == 'worst'){return(tail(usstate_hosp[which(!is.na(usstate_hosp$heartattack)), 1], n= 1))}
		# print(usstate_hosp)
		print(usstate_hosp[usstate_hosp$rank == num, 1])

	}

	# if outcome is heart failure
	if(outcome == "heart failure"){
		names(data)[17] <- 'heartfailure'
		usstate <- data[which(data$State == state), c(2, 17)]
		usstate$heartfailure <- as.numeric(usstate$heartfailure)
		usstate_hosp <- usstate[order(usstate$heartfailure, usstate$Hospital.Name), ]
		if (num != 'worst' & num != 'best' & num > nrow(usstate_hosp)){return(NA)}
		usstate_hosp$rank <- 1 : nrow(usstate_hosp)
		if (num == 'best'){num = 1}
		if (num == 'worst'){return(tail(usstate_hosp[which(!is.na(usstate_hosp$heartfailure)), 1], n= 1))}
		print(usstate_hosp[usstate_hosp$rank == num, 1])

	}

	# if outcome is pneumonia
	if(outcome == "pneumonia"){
		names(data)[23] <- 'pneumonia'
		usstate <- data[which(data$State == state), c(2, 23)]
		usstate$pneumonia <- as.numeric(usstate$pneumonia)
		usstate_hosp <- usstate[order(usstate$pneumonia, usstate$Hospital.Name), ]
		if (num != 'worst' & num != 'best' & num > nrow(usstate_hosp)){return(NA)}
		usstate_hosp$rank <- 1 : nrow(usstate_hosp)
		if (num == 'best'){num = 1}
		if (num == 'worst'){return(tail(usstate_hosp[which(!is.na(usstate_hosp$pneumonia)), 1], n= 1))}
		print(usstate_hosp[usstate_hosp$rank == num, 1])

	}
}