## Function for finding the Sunday before a given date
previous_sunday = function(x) {
    ## Ensure we're working with a Date object
    if ( !inherits(x, "Date") ) {
        x = try(as.Date(x), silent = TRUE)
        if ( inherits(x, "try-error") ) {
            stop("previous_sunday() needs a date or an object coercible to one")
        }
    }
    ## If the date is a Sunday, get the Sunday exactly one week before
    if ( lubridate::wday(x) == 1 ) {
        return(x - 7)
    }
    ## If not, use floor_date() to find the previous Sunday
    return(lubridate::floor_date(x, unit = "week", week_start = 7))
}
