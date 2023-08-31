#1
maximum_function <- function(data){ 
  max <- -Inf
  for(i in data) {
    if (i > max) {
      max <- i
    }
  }
  return(max)
}

maximum_function(c(2,3,199))
#2
final_state_price <- function(price, state_name){ 
  state <- tolower(state.name)
  state_rate <- c(0.040, 0.000, 0.056, 0.065, 7.250,
                  2.900, 6.350, 0.000, 6.000, 4.000,
                  4.000, 0.060, 6.250, 0.070, 6.000,
                  6.500, 0.060, 4.450, 5.500, 0.060,
                  0.062, 6.000, 6.875, 7.000, 0.042,
                  0.000, 0.055, 6.850, 0.000, 0.066,
                  0.050, 0.040, 4.750, 5.000, 0.058,
                  0.045, 0.000, 0.060, 0.070, 6.000,
                  4.500, 7.000, 0.062, 4.850, 6.000,
                  4.300, 0.065, 0.060, 0.050, 4.000)
  
  taxrate<-tibble::tibble(state,state_rate)
  
  if (tolower(state_name) %in% state) {
    finalprice <- taxrate %>%
      filter(state == tolower(state_name)) %>%
      mutate(final = price * (1+state_rate/100))
    return(finalprice$final)
  }
  else {
    print("Misspelling! Please type correct state name!")
  }
}

final_state_price(2, "California")
final_state_price(10, "CaliforNIA")
final_state_price(10, "CaliforN")
