
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
  
  status_quo_costs <- seasonal_maize_expense_status_quo
  
  # here we add the costs of scenario together to get the total costs for status quo.

   #### status quo benefits ----
  # processing costs of the specific tons of maize
  # final sale value - all costs
  
status_quo_benefits <- 
  vv (var_mean = (season_revenue_status_quo),
      var_CV = coef_var, n = n_seasons)
  
  #### status quo risks ----
  # in this case, there are no risks because all the crop is already infected by AF and unusable, all is loss.
  
  #### status quo total ---- 
  # sum everything first, discount once at the end
  # here we don't have risks because 100% of maize is lost
  
  status_quo_total <- (status_quo_benefits - status_quo_costs)
  
  # AF36 option ----
    #### AF36 option costs ----
  
  farmers_maize_AF36 <- ((farmers_maize * (1 - af36_maize_loss_field)) * (1 - af36_maize_loss_storage))
  # farmers maize AF36 is the amount of maize farmers will harvest after removing the percentage of maize lost 
  # by afs under AF36 treatment
  
  imported_maize_AF36 <- status_quo_maize_requirement - farmers_maize_AF36
  # imported maize AF36 is the amount of maize that needs to be imported to meet maize requirement under AF36 treatment
  
  seasonal_maize_expense_cost_AF36 <- 
    vv (var_mean = ((farmers_maize_AF36 * total_area_harvest) + (imported_maize_AF36 * import_price)), 
        var_CV = coef_var, n = n_seasons)
  # seasonal maize expense cost AF36 is how much Gruma spends on maize, imported and local, under AF36 treatment
  
  purchase_AF36_cost <-
    vv (var_mean = (af36_concentration_need * farmers_maize * af36_purchase_price), 
        var_CV = coef_var, n = n_seasons)
  # purchase AF36 cost is how much Gruma spends on AF36 treatment
  
  distribute_AF36_cost <-
    vv (var_mean = af36_distribution, var_CV = coef_var, n = n_seasons)
  # distribute AF36 cost is how much Gruma spends on distributing the AF36 treatment throughout the desired regions
  
  training_AF36_cost <-
    vv (var_mean = af36_training, var_CV = coef_var, n = n_seasons)
  # training AF36 cost is how much Gruma spends on training the farmers to use the AF36 treatment
  
  farmers_maize_loss_AF36 <- farmers_maize_AF36 * af36_maize_loss_field * af36_maize_loss_storage
  # farmers maize loss AF36 is the total amount of maize lost by afs contamination under AF36 treatment
  
  seasonal_maize_loss_cost_AF36 <- 
    vv (var_mean = (farmers_maize_loss_AF36 * earnings_per_ton_maize), var_CV = coef_var, n = n_seasons)
  # seasonal maize loss cost AF36 is how much Gruma loses on maize that is contaminated by afs under AF36 treatment
  
  # add AF36 alternative costs together 
  
  AF36_costs <- 
    vv (var_mean = (seasonal_maize_expense_cost_AF36 + purchase_AF36_cost + distribute_AF36_cost + 
      training_AF36_cost + seasonal_maize_loss_cost_AF36), var_CV = coef_var, n = n_seasons)
  # AF 36 costs it the total costs incurred under AF36 treatment
  
  #### AF36 option benefits ----
  # add if or convert negative values to zero because we can't have negative sales
  
  additional_sales_AF36 <- (season_revenue_status_quo - (farmers_maize_AF36 * earnings_per_ton_maize))
  # additional sales AF36 is how much Gruma makes additional to the status quo revenue under AF36 treatment
  
  season_revenue_AF36 <- season_revenue_status_quo + additional_sales_AF36
  # season revenue AF36 is the total amount that Gruma makes from maize under AF36 treatment
  
  # add AF36 alternative benefits together 
  AF36_benefits <- 
    vv (var_mean = (season_revenue_AF36 - costs_goods),
        var_CV = coef_var, n = n_seasons)
  
  #### AF36 option risks ----
  
  AF36_farmers_misuse_risk <- chance_event (risk_af36_farmers_misuse, value_if = 0, n = n_seasons)
  # AF36 farmers misuse risk is the risk that AF36 fails because of farmers misuse
  
  AF36_resistance_risk <- chance_event (risk_af36_resistance, value_if = 1, n = n_seasons)
  #AF36 resistance risk is the risk that the AF36 fails because afs are stronger and overpower AF36
  
  AF36_health_hazard_risk <- chance_event (risk_af36_health_hazard, value_if = 1, n = n_seasons)
  #AF36 health hazard risk is the risk that AF36 presents a health hazard to human consumption
  
  AF36_dispersion_risk <- chance_event (risk_af36_dispersion, value_if = 0, n = n_seasons)
  #AF36 dispersion risk is the risk that afs disperse through the whole field even under AF36 treatment
  
  # AF36 total risk
  AF36_total_risk <- (AF36_farmers_misuse_risk * AF36_resistance_risk * 
    AF36_health_hazard_risk * AF36_dispersion_risk)
  # AF36 total risk is the sum of risks under AF36 treatment
  
  #### AF36 total  ----
  
  AF36_total <- 
    ((AF36_benefits - AF36_costs) * AF36_total_risk)
  
  # Self-storage option ----
  
  ### Self-storage costs ----
  # we split farmers maize losses from field and storage
  
  farmers_maize_selfst <- ((farmers_maize * (1 - selfst_maize_loss_field)) * (1 - selfst_maize_loss_storage))
  # farmers maize selfst is the amount of maize farmers that is harvested and not contaminated and can be used for
  # production under the self-storage option
  
  imported_maize_selfst <- status_quo_maize_requirement - farmers_maize_selfst
  # imported maize selfst is the amount of maize that has to be imported to meet production requirements under 
  # self-storage option
  
  seasonal_maize_expense_selfst <- 
    vv (var_mean = ((farmers_maize_selfst * local_price) + (imported_maize_selfst * import_price)),
        var_CV = coef_var, n = n_seasons)
  # seasonal maize expense selfst is the cost for Gruma from purchasing both imported and local maize with self-storage option
  
  seasonal_maize_loss_AF_cost_selfst <- 
    vv ((var_mean = (selfst_maize_loss_field * farmers_maize) * earnings_per_ton_maize), 
        var_CV = coef_var, n = n_seasons)
  # seasonal maize loss AF cost selfst is the total cost for maize for Gruma under the self-storage option
  
  selfst_costs1 <- 
    vv (var_mean = (seasonal_maize_expense_selfst + 
        selfst_equipment_costs + selfst_infrastructure_costs + 
        selfst_maintenance_costs + selfst_maizetransport_costs + 
        selfst_personnel_costs),
        var_CV = coef_var, n = n_seasons)
  # selfst costs 1 is the sum of the self-storage costs option that are variable
  
  selfst_costs2 <- seasonal_maize_loss_AF_cost_selfst
  # selfst costs 2 is the sum of self-storage costs option that are fixed
  
  selfst_cost <- selfst_costs1 + selfst_costs2
  # selfst cost is the sum of selfst costs1 and selfst costs2
  
  ### Self-storage benefits ----
  
 additional_sales_selfst <- (season_revenue_status_quo - (farmers_maize_selfst * earnings_per_ton_maize))
  # additional sales selfst is the additional sales other than the status quo using the self-storage option
  
   season_revenue_selfst <- season_revenue_status_quo + additional_sales_selfst
   # season revenue selfst is the total revenue of self-storage option by maize
  
  selfst_benefits <- 
    vv (var_mean = (season_revenue_selfst),
        var_CV = coef_var, n = n_seasons)
  # selfst benefits is the sum of revenue for the option of self-storage
  
  ### Self-storage risks ----
  
  selfst_outbreak_risk <- chance_event (risk_selfst_outbreak, value_if = 0, n = n_seasons)
  # selfst outbreak risk is the risk of having an outbreak of afs 
 
  selfst_risks <- selfst_outbreak_risk
  # total risks for self-storage outbreak

  ### Self-storage total ----
  
  selfst_total <- ((selfst_benefits - selfst_costs) * selfst_risks)
  # total amount earned from self-storage option 
  
  # No-cash incentive option ----
  ### No-cash incentive costs ----
  
  farmers_maize_incentive <- ((farmers_maize * (1 - incentive_maize_loss_field)) * (1 - incentive_maize_loss_storage))
  # farmers maize incentive is the amount of maize harvested and usable for production with the maize incentive option
  
  imported_maize_incentive <- status_quo_maize_requirement - farmers_maize_incentive
  # imported maize incentive is the amount of maize that has to be imported 
  # to comply with the maize requirement for production with the no-cash incentive option
  
  seasonal_maize_expense_cost_incentive <- 
    vv (var_mean = ((farmers_maize_incentive * total_area_harvest) + (imported_maize_incentive * import_price)), 
        var_CV = coef_var, n = n_seasons)
  # seasonal maize expense cost incentive is how much Gruma spent in maize, imported and local, with the 
  # no-cash option
  
  incentive_mgt_practices_cost <-
    vv (var_mean = (incentive_training_cost * incentive_number_farmers), 
        var_CV = coef_var, n = n_seasons)
  # incentive mgt practices cost is the cost associated with agro mgt. practices training for the no-cash incentive option 
  
  no_cash_incentive_cost <-
    vv (var_mean = (incentive_no_cash * incentive_number_farmers), var_CV = coef_var, n = n_seasons)
  # no cash incentive cost is the direct cost associated with the no-cash incentive 
  
  farmers_maize_loss_incentive <- farmers_maize_incentive * incentive_maize_loss_field * incentive_maize_loss_storage
  # farmers maize loss incentive is how much maize was lost with the no-cash incentive option 
  
  seasonal_maize_loss_cost_incentive <- 
    vv (var_mean = (farmers_maize_loss_incentive * earnings_per_ton_maize), var_CV = coef_var, n = n_seasons)
  # seasonal maize loss cost incentive is how much is lost on the no-cash incentive option from maize contaminated
  # add incentive alternative costs together 
  
  incentive_costs <- 
    vv (var_mean = (seasonal_maize_expense_cost_incentive + incentive_mgt_practices_cost + 
                      no_cash_incentive_cost + seasonal_maize_loss_cost_incentive), var_CV = coef_var, n = n_seasons)
  
  ### No-cash incentive benefits ----
  
  additional_sales_incentive <-  vv (var_mean = (incentive_additional_maize * incentive_number_farmers * earnings_per_ton_maize), 
  var_CV = coef_var, n = n_seasons)

  season_revenue_incentive <- season_revenue_status_quo + additional_sales_incentive
  
  incentive_benefits <- additional_sales_incentive + season_revenue_incentive
  
  ### No-cash incentive risks ----
  
  incentive_fail_risk <- chance_event (risk_incentive_fail, value_if = 0, n = n_seasons)
 
  # No-cash incentive total risk
  
  incentive_fail_total_risk <- incentive_fail_risk
  
  ### No cash incentive total ----
  
  incentive_total <- 
    ((incentive_benefits - incentive_costs) * AF36_total_risk)
  
  # Dipstick assay pre-storage option ----
  
  ### Dipstick assay costs ----
  # we split farmers maize losses from field and storage
  
  farmers_maize_dipstick <- ((farmers_maize * (1 - dipstick_maize_loss_field)) * (1 - dipstick_maize_loss_storage))
  # farmers maize dipstick is the amount of maize from farmers that is harvested and not contaminated and can be used for
  # production under the dipstick assay option
  
  imported_maize_dipstick <- status_quo_maize_requirement - farmers_maize_dipstick
  # imported maize dipstick is the amount of maize that needs to be imported to meet maize requirement 
  # with dipstick assay testing
  
  seasonal_maize_expense_cost_dipstick <- 
    vv (var_mean = ((farmers_maize_dipstick * total_area_harvest) + (imported_maize_dipstick * import_price)), 
        var_CV = coef_var, n = n_seasons)
  # seasonal maize expense cost dipstick is how much Gruma spends on maize, imported and local, with dipstick assay tests
  
  purchase_dipstick_cost <-
    vv (var_mean = (dipstick_materials_need * farmers_maize), 
        var_CV = coef_var, n = n_seasons)
  # purchase dipstick  cost is how much Gruma spends on dipstick assay tests and agents needed 
  
  distribute_dipstick_cost <-
    vv (var_mean = dipstick_distribution, var_CV = coef_var, n = n_seasons)
  # distribute dipstick cost is how much Gruma spends on distributing the dipstick assay
  # tests throughout the desired regions
  
  training_dipstick_cost <-
    vv (var_mean = dipstick_training, var_CV = coef_var, n = n_seasons)
  # training dipstick cost is how much Gruma spends on training the farmers to use the dipstick treatment
  
  farmers_maize_loss_dipstick <- farmers_maize_dipstick * dipstick_maize_loss_field * dipstick_maize_loss_storage
  # farmers maize loss dipstick is the total amount of maize lost by afs contamination with dipstick testing
  
  seasonal_maize_loss_cost_dipstick <- 
    vv (var_mean = (farmers_maize_loss_dipstick * earnings_per_ton_maize), var_CV = coef_var, n = n_seasons)
  # seasonal maize loss cost dipstick is how much Gruma loses on maize that is contaminated by afs with dipstick tests
  
  # add dipsticks alternative costs together 
  
  dipstick_costs <- 
    vv (var_mean = (seasonal_maize_expense_cost_dipstick + purchase_dipstick_cost + distribute_dipstick_cost + 
                      training_dipstick_cost + seasonal_maize_loss_cost_dipstick), var_CV = coef_var, n = n_seasons)
  # dipstick costs it the total costs incurred with dipstick assay testing
  
  ### Dipstick option benefits ----
  # add if or convert negative values to zero because we can't have negative sales
  
  additional_sales_dipstick <- (season_revenue_status_quo - (farmers_maize_dipstick * earnings_per_ton_maize))
  # additional sales dipstick is how much Gruma makes additional to the status quo revenue with dipstick testing
  
  season_revenue_dipstick <- season_revenue_status_quo + additional_sales_dipstick
  # season revenue dipstick is the total amount that Gruma makes from maize with dipstick testing
  
  # add dipstick alternative benefits together 
  dipstick_benefits <- 
    vv (var_mean = (season_revenue_dipstick - costs_goods),
        var_CV = coef_var, n = n_seasons)
  
  ### Dipstick option risks ----
  
  dipstick_farmers_misuse_risk <- chance_event (risk_dipstick_farmers_misuse, value_if = 0, n = n_seasons)
  # dipstick farmers misuse risk is the risk that dipstick testing fails because of farmers misuse
  
  dipstick_dispersion_risk <- chance_event (risk_dipstick_dispersion, value_if = 0, n = n_seasons)
  #dipstick dispersion risk is the risk that afs disperse through the storage even with dipstick tests 
  
  #### Dipstick assay total  ----
  
  dipstick_total <- 
    ((dipstick_benefits - dipstick_costs) * dipstick_total_risk)
  
  # Discount rate  ----
  
  final_values_total <-
    discount(x = status_quo_total, AF36_total, selfst_total, incentive_total, dipstick_total,
             discount_rate = discount_rate, calculate_NPV = TRUE)
  
  
  return(list(status_quo = final_values_status_quo, final_values_AF36, final_values_selfst))
  # return list indicates any outcome you wish to see from the model
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
