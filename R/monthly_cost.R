#' Calculate monthly cost of a house
#'
#' @param house_value house value
#' @param property_tax_rate annual property tax rate
#' @param other_tax annual other tax rate
#' @param one_time_fee one time fee, e.g., agent fee, points fee
#' @param annual_other_tax other tex fee, e.g., school/county tax
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
  property_tax_rate = 1.54/100,        
  other_tax = 1/100,
  one_time_fee = 1e3,
  annual_other_tax = 3000,        
  monthly_hoa_fee = 150,          
  monthly_home_insurance = 150,   
  monthly_utility_fee = 300,      
  closing_fee,                 
  year = 5,
  saving_rate = 0.03,  
  down_payment_rate = 0.2,   
  mortgage_rate = 0.0375,
  morrgage_year = 30             
){
  
  # the annual cost: HOA + property and other tax fee + home insurance + utility
  annual_fee = monthly_hoa_fee * 12 + 
    house_value * property_tax_rate + other_tax + 
    monthly_home_insurance * 12 + 
    monthly_utility_fee * 12 
  
  # total mortgage amount
  mortgage = house_value * (1 - down_payment_rate)                    
  monthly_mortgage <- repayment(mortgage,               
                                mortgage_rate / 12,     
                                morrgage_year * 12      
  ) 
  
  return(one_time_fee/year/12 + annual_fee/12 + (house_value*down_payment_rate  + one_time_fee)*saving_rate/12 + monthly_mortgage)
}
