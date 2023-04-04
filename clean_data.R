require(dplyr)

get_cleaned_bookings <- function() {
  # Cleaning columns in order they appear in auto-complete in RStudio. 
  # Referencing the original dataset source for column details:
  # https://www.sciencedirect.com/science/article/pii/S2352340918315191
  
  bookings <- read.csv("hotel_bookings.csv")

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
  
  # Meals to factor.From the dataset source: 
  #   undefined, sc: no meal
  #   BB: bed and breakfast
  #   HB: Half board (breakfast + 1 other meal)
  #   FB: Full board (all three meals)
  bookings <- bookings %>% mutate(meal = replace(meal, meal == "Undefined", "None")) %>%
    mutate(meal = replace(meal, meal == "SC", "None")) %>%
    mutate(meal = factor(meal, levels = c("None", "BB", "HB", "FB"), labels = c("None", "Breakfast", "Half Board", "Full Board")))
  
  # country
  # We will likely not be looking at country, so leave data missing it
  bookings <- bookings %>% mutate(country = replace(country, country == "NULL", NA)) %>%
    mutate(country = factor(country))
  
  # Market segment: offline vs online
  bookings <- bookings %>% mutate(market_segment = factor(market_segment))
  
  # Distribution channel. From the dataset source, TA means Travel Agent and 
  # TO means tour operator. Contains "Undefined", which I was not able to 
  # find in the source if that means no channel or the channel could not be found.
  # Leaving as "Undefined" until we need it, in which case we should look a little
  # deeper.
  bookings <- bookings %>% mutate(distribution_channel = factor(distribution_channel))
  
  # Repeat guest. Should be logical, not int.
  bookings$is_repeated_guest <- bookings$is_repeated_guest == 1
  
  # Previous cancellations does not need cleaning.
  
  # Previous non-canceled bookings does not need cleaning
  
  # Room types are indicated by unhelpful single-letter codes, but I was not
  # immediately able to find what they meant in the original source. Until
  # we need them simply convert to factor.
  bookings <- bookings %>% mutate(reserved_room_type = factor(reserved_room_type))
  
  # Same for assigned room type
  bookings <- bookings %>% mutate(assigned_room_type = factor(assigned_room_type))
  
  # booking changes does not need cleaning
  
  # Deposit type has no missing values and can be simply converted to factor.
  bookings <- bookings %>% mutate(deposit_type = factor(deposit_type))
  
  # Agency
  bookings <- bookings %>% mutate(agent = replace(agent, agent == "NULL", "None")) %>%
    mutate(agent = factor(agent))
  
  # Company
  # the specific company is beyond the level of specificity we will be using, so
  # simply replace it with if the booking was paid by a company or not.
  bookings <- bookings %>% mutate(company = (company != "NULL"))
  
  # Days in waiting list does not need cleaning
  
  # Customer type. Simple factor conversion
  bookings <- bookings %>% mutate(customer_type = factor(customer_type))
  
  # Average Daily Rate. This does not require cleaning. It does, strangely, 
  # include a negative entry. Leaving that one alone, since it is a valid 
  # value based on the way it is calculated
  
  # Required car parking does not need cleaning
  
  # Number of special requests does not need cleaning
  
  # Reservation status: the big one. Does not need anything other than conversion
  # to factor.
  bookings <- bookings %>% mutate(reservation_status = factor(reservation_status))
  
  # Reservation status date needs to be converted to date, of course
  bookings <- bookings %>% mutate(reservation_status_date = as.Date(reservation_status_date))
  
  # Stays total nights requires no cleaning.
  
  return(bookings)
}

