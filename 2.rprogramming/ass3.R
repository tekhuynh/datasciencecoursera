best <- function(state, outcome) {
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    hospital_name_col <- 2
    outcome_cols <- list(
        "heart attack" = 11,
        "heart failure" = 17,
        "pneumonia" = 23
    )

    if (!is.element(state, data$State)) {
        stop("invalid state")
    }

    if (!is.element(outcome, names(outcome_cols))) {
        stop("invalid outcome")
    }

    cols <- c(hospital_name_col, outcome_cols[[outcome]])
    state_hospitals <- data[data$State == state, cols]
    state_hospitals[, 2] <- as.numeric(state_hospitals[, 2])
    sorted <- order(state_hospitals[, 2], state_hospitals[, 1])

    best_hospital <- state_hospitals[sorted[1], 1]
    as.character(best_hospital)
}

rankhospital <- function(state, outcome, num = "best") {
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    hospital_name_col <- 2
    outcome_cols <- list(
        "heart attack" = 11,
        "heart failure" = 17,
        "pneumonia" = 23
    )

    if (!is.element(state, data$State)) {
        stop("invalid state")
    }

    if (!is.element(outcome, names(outcome_cols))) {
        stop("invalid outcome")
    }

    cols <- c(hospital_name_col, outcome_cols[[outcome]])
    state_hospitals <- data[data$State == state, cols]
    state_hospitals[, 2] <- as.numeric(state_hospitals[, 2])
    state_hospitals <- state_hospitals[!is.na(state_hospitals[, 2]), ]
    sorted <- order(state_hospitals[, 2], state_hospitals[, 1])

    if (is.numeric(num) & num <= length(sorted)) {
        return(state_hospitals[sorted[num], 1])
    } else if (num == "best") {
        return(state_hospitals[sorted[1], 1])
    } else if (num == "worst") {
        return(state_hospitals[sorted[length(sorted)], 1])
    }
    NA
}

rankall <- function(outcome, num = "best") {
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    state_col <- 7
    hospital_name_col <- 2
    outcome_cols <- list(
        "heart attack" = 11,
        "heart failure" = 17,
        "pneumonia" = 23
    )

    if (!is.element(outcome, names(outcome_cols))) {
        stop("invalid outcome")
    }

    cols <- c(state_col, hospital_name_col, outcome_cols[[outcome]])
    hospitals <- data[, cols]
    hospitals[, 3] <- as.numeric(hospitals[, 3])

    state_outcomes <- split(hospitals, hospitals$State)

    hospital <- c()
    state <- c()
    for (state_outcome in state_outcomes) {
        state <- c(state, state_outcome[1, 1])
        state_outcome <- state_outcome[!is.na(state_outcome[, 3]), ]

        sorted <- order(state_outcome[, 3], state_outcome[, 2])

        h <- if (is.numeric(num) & num <= length(sorted)) {
            state_outcome[sorted[num], 2]
        } else if (num == "best") {
            state_outcome[sorted[1], 2]
        } else if (num == "worst") {
            state_outcome[sorted[length(sorted)], 2]
        } else {
            NA
        }
        hospital <- c(hospital, h)
    }
    data.frame(hospital, state)
}