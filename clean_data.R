require(dplyr)

get_cleaned_bookings <- function() {
  bookings = read.csv("hotel_bookings.csv")
  str(bookings)
  
  # hotel type has two values: "Resort Hotel" and "City Hotel"
  bookings$hotel <- factor(bookings$hotel)
  
  # is_canceled is actually a boolean, signified by 0, 1:
  bookings$is_canceled <- (bookings$is_canceled == 1)
  
  # Checked on lead times, has no NA values and is in range (0, 737)
  
  # Year should be an ordered factor
  bookings$arrival_date_year <- factor(bookings$arrival_date_year, ordered = TRUE, 
                                       levels = c(2015, 2016, 2017))
  
  # Month should be an ordered factor
  months = c("January", "February", "March", "April", "May", "June", "July",
             "August", "September", "October", "November", "December")
  months_abrev = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug",
                   "sep", "oct", "nov", "dec")
  bookings$arrival_date_month <- factor(bookings$arrival_date_month,
                                        ordered = TRUE,
                                        levels = months)
  levels(bookings$arrival_date_month) <- months_abrev
  
  # Week, day of month require no cleaning.
  
  # stays doesn't need to be cleaned but we could have a helpful total nights column
  bookings$stays_total_nights <- bookings$stays_in_weekend_nights + bookings$stays_in_week_nights
  
  # adults, babies columns look fine. Children has 4 na values, which is small
  # enough we can comfortably drop the rows:
  bookings <- filter(bookings, !is.na(children))
  
  # meals to factor. Undefined is none.
  
  return(bookings)
}
