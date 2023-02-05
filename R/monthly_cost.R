#' Calculate monthly cost of a house
#'
#' @param house_value house value
#' @param house_assessed_value house value to calculate the property tax 
#' @param mil_rate annual property tax rate, which is the multiplier of the house_assessed_value to get the annual property tax
#' @param one_time_fee one time fee, e.g., points fee
#' @param monthly_hoa_fee monthly HOA fee
#' @param monthly_home_insurance monthly home insurance fee
#' @param monthly_utility_fee monthly utility fee
#' @param closing_fee closing fee
#' @param year number of years for invest this house 
#' @param saving_rate saving rate, which is potential loss of the down payment
#' @param down_payment_rate down payment
#' @param mortgage_rate annual mortgage rate
#' @param morrgage_year mortgage years
#'
#' @return
#' @export
#'
#' @examples
monthly_cost <- function(
  house_value = 20*1e4, 
  house_assessed_value = 15*1e6,   
  mil_rate = 2.5/100,
  one_time_fee = 0,
  monthly_hoa_fee = 150,          
  monthly_home_insurance = 150,   
  monthly_utility_fee = 300,      
  closing_fee = 0,                 
  year = 5,
  saving_rate = 0.03,  
  down_payment_rate = 0.2,   
  mortgage_rate = 0.0375,
  morrgage_year = 30,
  annual_increase_rate=0/100
){
  
  # the annual cost: HOA + property and other tax fee + home insurance + utility
  annual_fee = monthly_hoa_fee * 12 + 
    house_assessed_value * mil_rate + 
    monthly_home_insurance * 12 + 
    monthly_utility_fee * 12 
  
  # total mortgage amount
  mortgage = house_value * (1 - down_payment_rate)                    
  monthly_mortgage <- repayment(mortgage,               
                                mortgage_rate / 12,     
                                morrgage_year * 12) 
  
  ans <- one_time_fee/year/12 + 
         annual_fee/12 + 
         (house_value*down_payment_rate  + one_time_fee + closing_fee)*saving_rate/12 + 
         monthly_mortgage - house_value*((1+annual_increase_rate)^(year)-1)/year/12
  return(ans)
}
