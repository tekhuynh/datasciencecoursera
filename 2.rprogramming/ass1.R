pollutantmean <- function(directory, pollutant, id = 1:332) {
    values <- c()
    for (monitorId in id) {
        data <- get_data(directory, monitorId)
        raw <- data[[pollutant]]
        values <- c(values, raw[!is.na(raw)])
    }
    mean(values)
}

complete <- function(directory, id = 1:332) {
    nobs <- c()
    for (monitorId in id) {
        data <- get_data(directory, monitorId)
        nobs <- c(nobs, sum(complete.cases(data)))
    }
    # cbind("id" = id, "nobs" = nobs)
    data.frame(id = id, nobs = nobs)
}

corr <- function(directory, threshold = 0) {
    id <- 1:332
    corrs <- c()
    for (monitorId in id) {
        data <- get_data(directory, monitorId)
        cases <- data[complete.cases(data), ]
        if (nrow(cases) > threshold) {
            corrs <- c(corrs, cor(cases$sulfate, cases$nitrate))
        }
    }
    corrs
}

get_data <- function(directory, id) {
    filename <- sprintf("%03d.csv", id)
    filepath <- file.path(getwd(), directory, filename)
    read.csv(filepath)
}