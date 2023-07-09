## Function to ensure we're working with a Date object
ensure_date = function(x) {
    if ( !inherits(x, "Date") ) {
        x = try(as.Date(x), silent = TRUE)
        if ( inherits(x, "try-error") ) {
            stop("Supply a date or an object coercible to one", call. = FALSE)
        }
    }
    return(x)
}

## Function for finding the Sunday before a given date
previous_sunday = function(x) {
    ## Ensure we're working with a Date object
    x = ensure_date(x)
    ## If the date is a Sunday, get the Sunday exactly one week before
    if ( lubridate::wday(x) == 1 ) {
        return(x - 7)
    }
    ## If not, use floor_date() to find the previous Sunday
    return(lubridate::floor_date(x, unit = "week", week_start = 7))
}

## Function to find the start of Advent in a (regular) calendar year
get_Advent_start = function(year) {
    return(previous_sunday(as.Date(paste0(year, "-12-25"))) - 21)
}

## Function to find the start of the liturgical year a date is in
get_kalendar_start = function(x) {
    ## Ensure we're working with a Date object
    x = ensure_date(x)
    ## Find the start of Advent in that (regular) calendar year
    Advent_start = get_Advent_start(lubridate::year(x))
    ## If the date x is after Advent_start, then that's what we need
    if ( x >= Advent_start ) {
        return(Advent_start)
    }
    ## Otherwise, we need the previous Advent start
    return(get_Advent_start(lubridate::year(x) - 1))
}
