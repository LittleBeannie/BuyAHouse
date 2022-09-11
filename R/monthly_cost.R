############ part 2： 小小函数again，用来算每月成本 ###########
#' Title
#'
#' @param house_value 房屋价格
#' @param property_tax 每年地税率
#' @param other_tax 每年其他税率
#' @param one_time_fee 一次性费用
#' @param annual_other_tax 其他税：school/county tax等等
#' @param monthly_hoa_fee 每月物业费
#' @param monthly_home_insurance 每月房屋保险费
#' @param monthly_utility_fee 每月水电网支出
#' @param closing_fee 一次性过户费用，点数等等
#' @param year 房子投资年数
#' @param saving_rate 定期年利息（首付款的潜在机会成本）
#' @param down_payment 首付总额（万）
#' @param mortgage_rate 贷款年利率
#' @param morrgage_year 贷款年份
#'
#' @return
#' @export
#'
#' @examples
monthly_cost <- function(
  # 房屋价格
  house_value = 20*1e4, 
  # tax支出
  property_tax = 1.54/100,        # 每年地税率
  other_tax = 1/100,
  one_time_fee = 1e3,
  annual_other_tax = 3000,        # 其他税：school/county tax等等
  # 每月花费
  monthly_hoa_fee = 150,          # 每月物业费
  monthly_home_insurance = 150,   # 每月房屋保险
  monthly_utility_fee = 300,      # 每月水电网支出
  # 一次性费用
  closing_fee,                    # 中介费(卖出时), 过户费, 点数，等等
  # 房子投资年数
  year = 5,
  # 定期年利息（首付款的潜在机会成本）
  saving_rate = 0.03,  
  # 首付总额（万）
  down_payment = 0.2,   
  # 贷款年利率
  mortgage_rate = 0.0375,
  morrgage_year = 30             # 贷款年份
){
  # 每年要交的地税
  annual_property_tax = house_value * property_tax   
  
  # 每年需要的费用：物业费, 地税，等等
  annual_fee = monthly_hoa_fee * 12 + 
    property_tax + other_tax + 
    monthly_home_insurance * 12 + 
    monthly_utility_fee * 12 
  
  # 贷款总额
  mortgage = house_value * (1 - down_payment)                    
  monthly_mortgage <- repayment(mortgage,               # 贷款额
                                mortgage_rate / 12,     # 年利率/12
                                morrgage_year * 12      # 贷款年数*12
  ) 
  
  return(one_time_fee/year/12 + annual_fee/12 + (down_payment + one_time_fee)*saving_rate/12 + monthly_mortgage)
}
