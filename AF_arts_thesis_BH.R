
library(decisionSupport)

# function to test incomplete model as we go ----
make_variables<-function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(estimate_read_csv("AF_input_table.csv")) 

# model function ---- 

AF_cost_benefit_risk_simulation <- function () {
  coef_var = 10
  n_seasons = 20 
  discount_rate = 5
  
## other variables ---- 

  farmers_maize <- 
    vv (var_mean = (total_area_harvest - (total_area_harvest * other_maize_loss)), 
  var_CV = coef_var, n = n_seasons)
  
  local_price <- 6200
  # local price is the price at which MASECA buys maize in the AF infested state in MX, it is constant, in mxn/ton
  
  AF_maize_loss_status_quo <- 100
  # in status quo, 100% of the maize is loss by aflatoxins. it is a percentage, constant
  
  import_price <- 7000
  # AHORITA ESTA INVENTADO, NECESITO NUMERO REAL DE IGNACIO
  # import price is the price at which MASECA buys maize in the USA, it is constant, in mxn/ton 
  
  maize_yearly_seasons <- 1
  # DATO REAL DE IGNACIO
  # number of maize seasons in a year in AF infested state
  
## status quo ---- 
  #### status quo costs ---- 
  
  farmers_maize_status_quo <- (farmers_maize * AF_maize_loss_status_quo)

  imported_maize_status_quo <- status_quo_maize_requirement - farmers_maize_status_quo 
  # imported maize is the difference between maize requirement and farmers maize
  # maize requirement is X number they give me times number of seasons. 
  # use vv if it's not a set number 
  # add this variable to my input table 
  # imported = required - farmers
  
  seasonal_maize_expense_status_quo <- 
    vv (var_mean = ((farmers_maize_status_quo * local_price) + (imported_maize_status_quo * import_price)),
                    var_CV = coef_var, n = n_seasons)
  
  seasonal_res_and_dev_AF_cost <- 
    vv (var_mean = (res_and_dev_AF / maize_yearly_seasons), var_CV = coef_var, n = n_seasons)
  
  seasonal_maize_loss_AF_cost_status_quo <- 
    vv ((var_mean = (AF_maize_loss_status_quo * farmers_maize) * earnings_per_ton_maize), 
        var_CV = coef_var, n = n_seasons)
  
  # add status quo costs together pre discount
  # we don't need to say prediscount 
  
  status_quo_costs <- 
    (seasonal_res_and_dev_AF_cost + seasonal_maize_expense_status_quo 
     + seasonal_maize_loss_AF_cost_status_quo)

   #### status quo benefits ----
  # processing costs of the specific 5 tons of maize
  # final sale value - all costs
  
status_quo_benefits <- 
  vv (var_mean = (season_revenue_status_quo - costs_goods),
      var_CV = coef_var, n = n_seasons)
  
  #### status quo risks ----
  # in this case, there are no risks because all the crop is already infected by AF and unusable, all is loss.
  
  #### status quo total pre discount ---- 
  # sum everything first, discount once 
  
  status_quo_total_pre_discount <- 
    (status_quo_benefits - status_quo_costs)
  
  ###### status quo final values ----
  
  final_values_status_quo <-
    discount(x = status_quo_total_pre_discount, discount_rate = discount_rate, calculate_NPV = TRUE)
 
  # AF36 option ----
    #### AF36 option costs ----
  
  farmers_maize_AF36 <- (farmers_maize * af36_maize_loss)
  
  imported_maize_AF36 <- status_quo_maize_requirement - farmers_maize_AF36
  
  seasonal_maize_expense_cost_AF36 <- 
    vv (var_mean = ((farmers_maize_AF36 * total_area_harvest) + (imported_maize_AF36 * import_price)), 
        var_CV = coef_var, n = n_seasons)
  
  purchase_AF36_cost <-
    vv (var_mean = (af36_concentration_need * farmers_maize * af36_purchase_price), 
        var_CV = coef_var, n = n_seasons)
  
  distribute_AF36_cost <-
    vv (var_mean = af36_distribution, var_CV = coef_var, n = n_seasons)
  
  training_AF36_cost <-
    vv (var_mean = af36_training, var_CV = coef_var, n = n_seasons)
  
  seasonal_maize_loss_cost_AF36 <- 
    vv (var_mean = ((af36_maize_loss * farmers_maize) * earnings_per_ton_maize), var_CV = coef_var, n = n_seasons)
  
  # add AF36 alternative costs together 
  
  AF36_costs <- 
    vv (var_mean = (seasonal_maize_expense_cost_AF36 + purchase_AF36_cost + distribute_AF36_cost + 
      training_AF36_cost + seasonal_maize_loss_cost_AF36), var_CV = coef_var, n = n_seasons)
  
  #### AF36 option benefits ----
  # add if or convert negative values to zero because we can't have negative sales
  
  additional_sales_AF36 <- (season_revenue_status_quo - (farmers_maize_AF36 * earnings_per_ton_maize))
  
  season_revenue_AF36 <- season_revenue_status_quo + additional_sales_AF36
  
  # add AF36 alternative benefits together 
  AF36_benefits <- 
    vv (var_mean = (season_revenue_AF36 - costs_goods),
        var_CV = coef_var, n = n_seasons)
  
  #### AF36 option risks ----
  
  AF36_farmers_misuse_risk <- chance_event (risk_farmers_misuse, value_if = 0, n = n_seasons)
  AF36_resistance_risk <- chance_event (risk_resistance, value_if = 1, n = n_seasons)
  AF36_health_hazard_risk <- chance_event (risk_health_hazard, value_if = 1, n = n_seasons)
  AF36_dispersion_risk <- chance_event (dispersion_risk, value_if = 0, n = n_seasons)
  
  # AF36 total risk
  AF36_total_risk <- (AF36_farmers_misuse_risk * AF36_resistance_risk * 
    AF36_health_hazard_risk * AF36_dispersion_risk)
  
  #### AF36 total pre discount ----
  
  AF36_total_pre_discount <- 
    ((AF36_benefits - AF36_costs) * AF36_total_risk)

  
  ###### AF36 final values ----

  final_values_AF36 <-
    discount(x = AF36_total_pre_discount, discount_rate = discount_rate, calculate_NPV = TRUE)
  
  return(list(status_quo = final_values_status_quo))
  # return list indicates any outcome you wish to see from the model
  }
  
   ########### END OF MODEL 

# DA Analysis -----
 # you can define a different distribution shape if some values need to be taken
 # more in consideration - find it in the DA package
AF_results <-
  mcSimulation(estimate = estimate_read_csv("AF_input_table.csv"),
               model_function = AF_cost_benefit_risk_simulation,
               numberOfModelRuns = 100,
               functionSyntax = ("plainNames"))

decisionSupport::plot_distributions(mcSimulation_object = AF_results, 
                                    vars = c("status_quo"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)



##############

AF_results <-
  mcSimulation(estimate = estimate_read_csv("AF_input_table.csv"),
               model_function = AF_cost_benefit_risk_simulation,
               numberOfModelRuns = 100,
               functionSyntax = ("plainNames"))

decisionSupport::plot_distributions(mcSimulation_object = AF_results, 
                                    vars = c("first_maize_expense", "second_maize_expense"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)
