
library(decisionSupport)

# function to test incomplete model as we go ----
make_variables<-function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(estimate_read_csv("AF_input_table.csv")) 

# Model Function ---- 

AF_cost_benefit_risk_simulation <- function () {
  coef_var = 10
  n_seasons = 20 
  discount_rate = 5
  
## other variables ---- 
  
# farmers maize is the initial amount of maize harvested, both infected and non-infected by afs
# it is variable, in ton
# total area harvest is (EXACT/ESTIMATE)
  
  farmers_maize <- 
    vv (var_mean = (total_area_harvest), var_CV = coef_var, n = n_seasons)
  
  local_price <- 6200
  # local price is the price at which MASECA buys maize in the afs infested state in MX, 
  # it is constant, in mxn/ton
  # it is exact
  
  AF_maize_loss_status_quo <- 100
  # in status quo, 100% of the maize is loss by aflatoxins. 
  # it is constant, in percentage
  
  import_price <- 7000
  # AHORITA ESTA INVENTADO, NECESITO NUMERO REAL DE IGNACIO
  # import price is the price at which MASECA buys maize in the USA, 
  # it is constant, in mxn/ton 
  # it is exact
  
  maize_yearly_seasons <- 1
  # number of maize seasons in a year in AF infested state
  # it is constant, no specific unit
  # it is exact
  
## status quo ---- 
  #### status quo costs ---- 
  
  farmers_maize_status_quo <- (farmers_maize * (1 - AF_maize_loss_status_quo))
  # farmers maize status quo is the total amount of maize harvested times the inverse percent 
  # of maize lost by afs. In this case, all the maize is infected with afs.

  status_quo_maize_requirement_variable <- vv (var_mean = status_quo_maize_requirement, 
                                               var_CV = coef_var, n = n_seasons)
  # status quo maize requirement is how much maize Gruma needs to meet the maize 
  # production requirements. 
  # it is variable, in tons
  # it is EXACT/ESTIMATE
  
  imported_maize_status_quo <- status_quo_maize_requirement_variable - farmers_maize_status_quo 
  # imported maize is how much maize Gruma will have to purchase internationally to meet
  # the maize requirements for production. it is calculated by subtracting the amount of 
  # maize after removing maize loss by afs from the total maize requirement for production. 
  
  seasonal_maize_expense_status_quo <- 
    vv (var_mean = ((farmers_maize_status_quo * local_price) + 
                      (imported_maize_status_quo * import_price)),
                    var_CV = coef_var, n = n_seasons)
  # seasonal maize expense status quo is how much Gruma spends in acquiring the maize needed
  # for production. it is the sum of the imported maize plus the local maize purchases.
  
  ###### REVISAR SI ESTO HACE SENTIDO DE LOSS BY AFS LA DE ABAJO 
  
  seasonal_maize_loss_AF_cost_status_quo <- 
    vv ((var_mean = (AF_maize_loss_status_quo * farmers_maize) * earnings_per_ton_maize), 
        var_CV = coef_var, n = n_seasons)
  # seasonal maize loss AF cost status quo is how much money Gruma lost because of local maize
  # lost to afs contamination. 
  
  status_quo_costs <- (seasonal_maize_expense_status_quo 
     + seasonal_maize_loss_AF_cost_status_quo)
  # here we add the costs of scenario together to get the total costs for status quo.

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
  
  farmers_maize_AF36 <- ((farmers_maize * (1 - af36_maize_loss_field)) * (1 - af36_maize_loss_storage))
  
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
  
  farmers_maize_loss_AF36 <- farmers_maize_AF36 * af36_maize_loss_field * af36_maize_loss_storage
  
  seasonal_maize_loss_cost_AF36 <- 
    vv (var_mean = (farmers_maize_loss_AF36 * earnings_per_ton_maize), var_CV = coef_var, n = n_seasons)
  
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
  
  AF36_farmers_misuse_risk <- chance_event (risk_af36_farmers_misuse, value_if = 0, n = n_seasons)
  AF36_resistance_risk <- chance_event (risk_af36_resistance, value_if = 1, n = n_seasons)
  AF36_health_hazard_risk <- chance_event (risk_af36_health_hazard, value_if = 1, n = n_seasons)
  AF36_dispersion_risk <- chance_event (risk_af36_dispersion, value_if = 0, n = n_seasons)
  
  # AF36 total risk
  AF36_total_risk <- (AF36_farmers_misuse_risk * AF36_resistance_risk * 
    AF36_health_hazard_risk * AF36_dispersion_risk)
  
  #### AF36 total pre discount ----
  
  AF36_total_pre_discount <- 
    ((AF36_benefits - AF36_costs) * AF36_total_risk)
  
  ###### AF36 final values ----

  final_values_AF36 <-
    discount(x = AF36_total_pre_discount, discount_rate = discount_rate, calculate_NPV = TRUE)
  
  # Self-storage option ----
  
  ### Self-storage costs ----
  # we split farmers maize losses from field and storage
  
  farmers_maize_selfst <- ((farmers_maize * (1 - selfst_maize_loss_field)) * (1 - selfst_maize_loss_storage))
  
  imported_maize_selfst <- status_quo_maize_requirement - farmers_maize_selfst
  
  seasonal_maize_expense_selfst <- 
    vv (var_mean = ((farmers_maize_selfst * local_price) + (imported_maize_selfst * import_price)),
        var_CV = coef_var, n = n_seasons)
  
  # we take status quo cost values and add self-storage costs as vv
  # we keep the same r&d costs because the research currently done is for the field stage
  
  seasonal_maize_loss_AF_cost_selfst <- 
    vv ((var_mean = (selfst_maize_loss_field * farmers_maize) * earnings_per_ton_maize), 
        var_CV = coef_var, n = n_seasons)
  
  selfst_costs1 <- 
    vv (var_mean = (seasonal_maize_expense_selfst + 
        selfst_equipment_costs + selfst_infrastructure_costs + 
        selfst_maintenance_costs + selfst_maizetransport_costs + 
        selfst_personnel_costs),
        var_CV = coef_var, n = n_seasons)
  
  selfst_costs2 <- seasonal_maize_loss_AF_cost_selfst
  
  selfst_cost <- selfst_costs1 + selfst_costs2
  
  ### Self-storage benefits ----
  
 additional_sales_selfst <- (season_revenue_status_quo - (farmers_maize_selfst * earnings_per_ton_maize))
  
   season_revenue_selfst <- season_revenue_status_quo + additional_sales_selfst
  
  selfst_benefits <- 
    vv (var_mean = (season_revenue_selfst - costs_goods),
        var_CV = coef_var, n = n_seasons)
  
  ### Self-storage risks ----
  
  selfst_outbreak_risk <- chance_event (risk_selfst_outbreak, value_if = 0, n = n_seasons)
 
  selfst_risks <- selfst_outbreak_risk

  ### Self-storage total pre discount ----
  
  selfst_total_pre_discount <- 
    ((selfst_benefits - selfst_costs) * selfst_risks)
  
  
  ###### Self-storage final values ----
  
  final_values_selfst <-
    discount(x = selfst_total_pre_discount, discount_rate = discount_rate, calculate_NPV = TRUE)
  
  
  return(list(status_quo = final_values_status_quo, final_values_AF36, final_values_selfst))
  # return list indicates any outcome you wish to see from the model
  # No-cash incentive option ----
  ### No-cash incentive costs ----
  
  farmers_maize_incentive <- ((farmers_maize * (1 - incentive_maize_loss_field)) * (1 - incentive_maize_loss_storage))
  
  imported_maize_incentive <- status_quo_maize_requirement - farmers_maize_incentive
  
  seasonal_maize_expense_cost_incentive <- 
    vv (var_mean = ((farmers_maize_incentive * total_area_harvest) + (imported_maize_incentive * import_price)), 
        var_CV = coef_var, n = n_seasons)
  
  incentive_mgt_practices_cost <-
    vv (var_mean = (incentive_training_cost * incentive_number_farmers), 
        var_CV = coef_var, n = n_seasons)
  
  no_cash_incentive_cost <-
    vv (var_mean = (incentive_no_cash * incentive_number_farmers), var_CV = coef_var, n = n_seasons)
  
  farmers_maize_loss_incentive <- farmers_maize_incentive * incentive_maize_loss_field * incentive_maize_loss_storage
  
  seasonal_maize_loss_cost_incentive <- 
    vv (var_mean = (farmers_maize_loss_incentive * earnings_per_ton_maize), var_CV = coef_var, n = n_seasons)
  
  # add incentive alternative costs together 
  
  incentive_costs <- 
    vv (var_mean = (seasonal_maize_expense_cost_incentive + incentive_mgt_practices_cost + 
                      no_cash_incentive_cost + seasonal_maize_loss_cost_incentive), var_CV = coef_var, n = n_seasons)
  
  ### No-cash incentive benefits ----
  
  additional_sales_incentive <-  vv (var_mean = (incentive_additional_maize * incentive_number_farmers * earnings_per_ton_maize), 
  var_CV = coef_var, n = n_seasons)

  season_revenue_incentive <- season_revenue_status_quo + additional_sales_incentive
  
  # add AF36 alternative benefits together 
  AF36_benefits <- 
    vv (var_mean = (season_revenue_AF36 - costs_goods),
        var_CV = coef_var, n = n_seasons)
  
  
  ### No-cash incentive risks ----
  
  incentive_fail_risk <- chance_event (risk_incentive_fail, value_if = 0, n = n_seasons)
 
  # No-cash incentive total risk
  
  incentive_fail_total_risk <- incentive_fail_risk
  
  }
  
   ########### END OF MODEL 

# Decision Analysis -----
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
