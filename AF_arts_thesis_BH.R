
library(decisionSupport)
library(ggplot2)
library(colorspace)

# function to test incomplete model as we go ----
make_variables<-function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

#make_variables <- function(est,n=1)

make_variables(estimate_read_csv("AF_input.csv")) 

# Model Function ---- 
# The model is split into 4 options: status quo, AF36 (biocontrol), self-storage
# (optimal storage conditions), no-cash incentive (optimal pre-harvest 
# conditions), and dipstick assay (storing only tested and clean maize). 
# Each option goes through costs, benefits, and risks. At the end of each option 
# we get the total value for NPV per option. At the end of the 
# model we include the risks that affect all options, plus the discount. 

AF_cost_benefit_risk_simulation <- function () {
 
  ## status quo ---- 
  
  farmers_maize <- 
  vv (var_mean = (total_area_harvest), var_CV = coef_var, n = n_years)
  # amount of maize harvested by farmers in the area.
  # total_area_harvest is the total amount of maize harvested by the farmers 
  # in the area (ton)

  #### status quo costs ---- 
  
  farmers_maize_status_quo <- vv(var_mean = (farmers_maize * 
                            (1 - AF_maize_loss_status_quo)),
                            var_CV = coef_var, n=n_years)

  # farmers maize status quo is the total amount of maize harvested times 
  # the inverse percent of maize lost by afs. In this case, all the maize is 
  # infected with afs. AF_maize_loss_status_quo is the percentage of maize 
  # lost by AFs in this scenario

  status_quo_maize_requirement <- vv (var_mean = (mexico_yield * field_area), 
                                            var_CV = coef_var, n = n_years)
  
  # status quo maize requirement is how much maize Gruma needs to meet the maize 
  # production requirements. 
  # it is variable, in tons
  # mexico_yield is the average yield for Mexico for maize in ton/ha
  # field_area is the total area of harvest in Tamaulipas, in ha 
  
  imported_maize_status_quo <- vv (var_mean = (status_quo_maize_requirement
                                               - farmers_maize_status_quo),
                                          var_CV = coef_var, n = n_years)
  
  # imported maize is how much maize Gruma will have to purchase internationally 
  # to meet the maize requirements for production. it is calculated by 
  # subtracting the amount of maize after removing maize loss by afs from the 
  # total maize requirement for production. 
  
  seasonal_maize_expense_status_quo <- 
    vv (var_mean = ((farmers_maize_status_quo * local_price) + 
                  (imported_maize_status_quo * import_price)),
                             var_CV = coef_var, n = n_years)
  
  # seasonal maize expense status quo is how much Gruma spends in acquiring the
  # maize needed for production. it is the sum of the imported maize plus the 
  # local maize purchases.
  
  status_quo_costs <- seasonal_maize_expense_status_quo
  
  # here we add the costs of scenario together to get the total costs for 
  # status quo.

   #### status quo benefits ----
  # processing costs of the specific tons of maize
  # final sale value - all costs
  
    
  status_quo_benefits <-
  vv (var_mean = (season_revenue_status_quo),
            var_CV = coef_var, n = n_years)
  
  #### status quo risks ----
  # in this case, there are no risks because all the crop is already infected 
  # by AF and unusable, all is loss. season_revenue_status_quo is a calculation 
  # of the total brute sales divided by the number of productive hectares in 
  # Mexico, multiplied by the number of productive hectares in Tamaulipas to 
  # tie the earnings value to the hectares in our area of decision
  
  #### status quo total ---- 
  # sum everything first, discount once at the end
  # here we don't have risks because 100% of maize is lost
  
  status_quo_total <- (status_quo_benefits - status_quo_costs) 
  
  # AF36 option ----
    #### AF36 option costs ----
  
  farmers_maize_AF36 <- ((farmers_maize * (1 - af36_maize_loss_field)) * 
                                          (1 - af36_maize_loss_storage))
  
  # farmers maize AF36 is the amount of maize farmers will harvest after 
  # removing the percentage of maize lost by afs under AF36 treatment
  
  imported_maize_AF36 <- status_quo_maize_requirement - farmers_maize_AF36
  
  # imported maize AF36 is the amount of maize that needs to be imported to 
  # meet maize requirement under AF36 treatment
  
  seasonal_maize_expense_cost_AF36 <- 
    vv (var_mean = ((farmers_maize_AF36 * local_price) + 
                   (imported_maize_AF36 * import_price)), 
                        var_CV = coef_var, n = n_years)
  
  # seasonal maize expense cost AF36 is how much Gruma spends on maize, 
  # imported and local, under AF36 treatment
  
  purchase_AF36_cost <-
    vv (var_mean = (af36_concentration_need * farmers_maize_AF36 * 
                                        af36_purchase_price), 
                            var_CV = coef_var, n = n_years)
  
  # purchase AF36 cost is how much Gruma spends on AF36 treatment
  
  distribute_AF36_cost <-
    vv (var_mean = af36_distribution, var_CV = coef_var, n = n_years)
  
  # distribute AF36 cost is how much Gruma spends on distributing the AF36 
  # treatment throughout the desired regions
  
  training_AF36_cost <-
    vv (var_mean = af36_training, var_CV = coef_var, n = n_years)
  
  # training AF36 cost is how much Gruma spends on training the farmers to 
  # use the AF36 treatment
  
  farmers_maize_loss_AF36 <- farmers_maize_AF36 
  
  # farmers maize loss AF36 is the total amount of maize lost by afs 
  # contamination under AF36 treatment
  
  seasonal_maize_loss_cost_AF36 <- 
    vv (var_mean = (farmers_maize_loss_AF36 * earnings_per_ton_maize), 
                                     var_CV = coef_var, n = n_years)
  
  # seasonal maize loss cost AF36 is how much Gruma loses on maize that is 
  # contaminated by afs under AF36 treatment add AF36 alternative costs together 
  
  AF36_costs <- 
    vv (var_mean = (seasonal_maize_expense_cost_AF36 + purchase_AF36_cost + 
                                distribute_AF36_cost + training_AF36_cost + 
                                           seasonal_maize_loss_cost_AF36), 
                                          var_CV = coef_var, n = n_years)
  
  # AF 36 costs is the total costs incurred under AF36 treatment
  
  #### AF36 option benefits ----
  
  # add if or convert negative values to zero because we can't have 
  # negative sales
  
  additional_sales_AF36 <- vv (var_mean = (farmers_maize_AF36 * 
                                       earnings_per_ton_maize),
                               var_CV = coef_var, n = n_years)
  
  # additional sales AF36 is how much Gruma makes additional to the status 
  # quo revenue under AF36 treatment
  
  season_revenue_AF36 <- additional_sales_AF36 + status_quo_benefits
  
  # season revenue AF36 is the total amount that Gruma makes from maize 
  # under AF36 treatment
  
  # add AF36 alternative benefits together 
  
  AF36_benefits <- 
    vv (var_mean = season_revenue_AF36,
        var_CV = coef_var, n = n_years)
  
  #### AF36 option risks ----
  
  AF36_farmers_misuse_risk <- chance_event (risk_af36_farmers_misuse, 
                      value_if = 0, value_if_not = 1, n = n_years)
  
  # AF36 farmers misuse risk is the risk that AF36 fails because of farmers 
  # misuse over a period of 10 years

  # AF36 total risk
  
  AF36_total_risk <- AF36_farmers_misuse_risk
                   
  
  # AF36 total risk is the sum of risks under AF36 treatment
  
  #### AF36 total  ----
  
  AF36_total_cashflow <- AF36_benefits - AF36_costs
  
  AF36_total <- 
    ((AF36_benefits * AF36_total_risk) -  AF36_costs) - status_quo_total
  
  # Self-storage option ----
  
  ### Self-storage costs ----
  # we split farmers maize losses from field and storage
  
  farmers_maize_selfst <- ((farmers_maize * (1 - selfst_maize_loss_field)) * 
                                            (1 - selfst_maize_loss_storage))
  
  # farmers maize selfst is the amount of maize farmers that is harvested and 
  # not contaminated and can be used for production under the 
  # self-storage option
  
  imported_maize_selfst <- status_quo_maize_requirement - farmers_maize_selfst
  
  # imported maize selfst is the amount of maize that has to be imported to 
  # meet production requirements under self-storage option
  
  seasonal_maize_expense_selfst <- 
    vv (var_mean = ((farmers_maize_selfst * local_price) + 
                  (imported_maize_selfst * import_price)),
                         var_CV = coef_var, n = n_years)
  
  # seasonal maize expense selfst is the cost for Gruma from purchasing both 
  # imported and local maize with self-storage option
  
  seasonal_maize_loss_AF_cost_selfst <- 
    vv (var_mean = (selfst_maize_loss_field * 
     earnings_per_ton_maize), var_CV = coef_var, n = n_years)
  
  # seasonal maize loss AF cost selfst is the total cost for maize for Gruma 
  # under the self-storage option
  
  selfst_costs1 <- 
    vv (var_mean = (seasonal_maize_expense_selfst + 
                         selfst_maintenance_costs + 
                      selfst_maizetransport_costs + 
                           selfst_personnel_costs), 
                  var_CV = coef_var, n = n_years)
  
  # selfst costs 1 is the sum of the self-storage costs option that are variable
  
  selfst_costs2 <- seasonal_maize_loss_AF_cost_selfst
  
  # selfst costs 2 is the sum of self-storage costs option that are fixed
  
  selfst_costs3 <- selfst_equipment_costs + selfst_infrastructure_costs 
  
  # sefst costs 3 is the sum of one-time costs
  
  selfst_cost <- selfst_costs1 + selfst_costs2 + selfst_costs3
  
  # selfst cost is the sum of selfst costs1 and selfst costs2
  
  ### Self-storage benefits ----
  
 additional_sales_selfst <- vv (var_mean = (farmers_maize_selfst * 
        earnings_per_ton_maize), var_CV = coef_var, n = n_years)
  
  # additional sales selfst is the additional sales other than the status 
  # quo using the self-storage option
  
   season_revenue_selfst <- season_revenue_status_quo + additional_sales_selfst
   # season revenue selfst is the total revenue of self-storage option by maize
  
  selfst_benefits <- 
    vv (var_mean = (season_revenue_selfst),
         var_CV = coef_var, n = n_years)
  
  # selfst benefits is the sum of revenue for the option of self-storage
  
  ### Self-storage risks ----
  
  selfst_outbreak_risk <- chance_event (risk_selfst_outbreak, 
              value_if = 0, value_if_not = 1,  n = n_years)
  
  # selfst outbreak risk is the risk of having an outbreak of afs 
 
  selfst_risks <- selfst_outbreak_risk 
  #* climatic_total_loss
  
  # total risks for self-storage outbreak

  ### Self-storage total ----
  
  selfst_total_cashflow <- selfst_benefits - selfst_cost
  
  selfst_total <- 
    ((selfst_benefits * selfst_risks) - selfst_cost) - status_quo_total

  
  # total amount earned from self-storage option 
  
  # No-cash incentive option ----
  
  ### No-cash incentive costs ----
  
  farmers_maize_incentive <- ((farmers_maize * (1 - incentive_maize_loss_field))
                                           * (1 - incentive_maize_loss_storage))
  
  # farmers maize incentive is the amount of maize harvested and usable for 
  # production with the maize incentive option
  
  imported_maize_incentive <- status_quo_maize_requirement - 
                                     farmers_maize_incentive
  
  # imported maize incentive is the amount of maize that has to be imported 
  # to comply with the maize requirement for production with the no-cash 
  # incentive option
  
  seasonal_maize_expense_cost_incentive <- 
    vv (var_mean = ((farmers_maize_incentive * local_price) + 
                  (imported_maize_incentive * import_price)), 
                            var_CV = coef_var, n = n_years)
  
  # seasonal maize expense cost incentive is how much Gruma spent in maize, 
  # imported and local, with the no-cash option
  
  incentive_mgt_practices_cost <-
    vv (var_mean = (incentive_training_cost * incentive_number_farmers), 
                                       var_CV = coef_var, n = n_years)
  
  # incentive mgt practices cost is the cost associated with agro mgt. 
  # practices training for the no-cash incentive option 
  
  no_cash_incentive_cost <-
    vv (var_mean = (incentive_no_cash * incentive_number_farmers), 
                                 var_CV = coef_var, n = n_years)
  
  # no cash incentive cost is the direct cost associated with the 
  # no-cash incentive 
  
  farmers_maize_loss_incentive <- vv (var_mean = (farmers_maize_incentive * 
                incentive_maize_loss_field * incentive_maize_loss_storage), 
                                          var_CV = coef_var, n = n_years)
  
  # farmers maize loss incentive is how much maize was lost with the no-cash 
  # incentive option 
  
  seasonal_maize_loss_cost_incentive <- 
    vv (var_mean = (farmers_maize_loss_incentive * earnings_per_ton_maize), 
                                          var_CV = coef_var, n = n_years)
  
  # seasonal maize loss cost incentive is how much is lost on the no-cash 
  # incentive option from maize contaminated add incentive alternative 
  # costs together 
  
  incentive_costs <- 
    vv (var_mean = (seasonal_maize_expense_cost_incentive + 
    incentive_mgt_practices_cost + no_cash_incentive_cost + 
                       seasonal_maize_loss_cost_incentive), 
                          var_CV = coef_var, n = n_years)

  ### No-cash incentive benefits ----
  
  additional_sales_incentive <-  vv (var_mean = (incentive_additional_maize * 
                          incentive_number_farmers * earnings_per_ton_maize), 
                                            var_CV = coef_var, n = n_years)

  season_revenue_incentive <- additional_sales_incentive + status_quo_benefits
  
  incentive_benefits <- additional_sales_incentive 
  
  ### No-cash incentive risks ----
  
  incentive_fail_risk <- chance_event (risk_incentive_fail, value_if = 0, 
                                        value_if_not = 1, n = n_years)
 
  # No-cash incentive total risk
  
  incentive_fail_total_risk <- incentive_fail_risk 
  
  ### No cash incentive total ----
  
  incentive_total_cashflow <- incentive_benefits - incentive_costs
  
  incentive_total <- 
((incentive_benefits * incentive_fail_total_risk) - incentive_costs) 
                                                  - status_quo_total
  
  # Dipstick assay pre-storage option ----
  
  ### Dipstick assay costs ----
  # we split farmers maize losses from field and storage
    
  farmers_maize_dipstick <- ((farmers_maize * (1 - dipstick_maize_loss_field)) * 
                                              (1 - dipstick_maize_loss_storage))
  
  # farmers maize dipstick is the amount of maize from farmers that is harvested 
  # and not contaminated and can be used for production under the dipstick 
  # assay option
  
  imported_maize_dipstick <- status_quo_maize_requirement - 
                                     farmers_maize_dipstick
  
  # imported maize dipstick is the amount of maize that needs to be imported to 
  # meet maize requirement with dipstick assay testing
  
  seasonal_maize_expense_cost_dipstick <- 
    vv (var_mean = ((farmers_maize_dipstick * local_price) + 
                  (imported_maize_dipstick * import_price)), 
                           var_CV = coef_var, n = n_years)
  
  # seasonal maize expense cost dipstick is how much Gruma spends on maize, 
  # imported and local, with dipstick assay tests
  
  purchase_dipstick_cost <-
    vv (var_mean = (dipstick_materials_need * farmers_maize_dipstick), 
                            var_CV = coef_var, n = n_years)
  
  # purchase dipstick  cost is how much Gruma spends on dipstick assay tests 
  # and agents needed 
  
  distribute_dipstick_cost <-
    vv (var_mean = dipstick_distribution, var_CV = coef_var, n = n_years)
  
  # distribute dipstick cost is how much Gruma spends on distributing the 
  # dipstick assay tests throughout the desired regions
  
  training_dipstick_cost <-
    vv (var_mean = dipstick_training, var_CV = coef_var, n = n_years)
  
  # training dipstick cost is how much Gruma spends on training the farmers to 
  # use the dipstick treatment
  
  farmers_maize_loss_dipstick <- farmers_maize_dipstick * 
                              dipstick_maize_loss_field *
                              dipstick_maize_loss_storage
  
  # farmers maize loss dipstick is the total amount of maize lost by afs 
  # contamination with dipstick testing
  
  seasonal_maize_loss_cost_dipstick <- 
    vv (var_mean = (farmers_maize_loss_dipstick * 
                         earnings_per_ton_maize), 
                 var_CV = coef_var, n = n_years)
  
  # seasonal maize loss cost dipstick is how much Gruma loses on maize that is 
  # contaminated by afs with dipstick tests
  
  # add dipsticks alternative costs together 
  
  dipstick_costs <- 
    vv (var_mean = (seasonal_maize_expense_cost_dipstick + 
                                  purchase_dipstick_cost + 
                                distribute_dipstick_cost + 
                                  training_dipstick_cost + 
                       seasonal_maize_loss_cost_dipstick), 
                         var_CV = coef_var, n = n_years)
  
  # dipstick costs it the total costs incurred with dipstick assay testing
  
  ### Dipstick option benefits ----
  # add if or convert negative values to zero because we can't have negative 
  # sales
  
  additional_sales_dipstick <-  farmers_maize_dipstick * earnings_per_ton_maize
  
  # additional sales dipstick is how much Gruma makes additional to the status 
  # quo revenue with dipstick testing
   
  season_revenue_dipstick <- additional_sales_dipstick + status_quo_benefits
  
  # season revenue dipstick is the total amount that Gruma makes from 
  # maize with dipstick testing
  
  # add dipstick alternative benefits together 
  
  dipstick_benefits <- 
    vv (var_mean = (season_revenue_dipstick),
        var_CV = coef_var, n = n_years)
  
### Dipstick option risks ----
  
  dipstick_farmers_misuse_risk <- chance_event (risk_dipstick_farmers_misuse, 
                               value_if = 0, value_if_not = 1, n = n_years)
  
  # dipstick farmers misuse risk is the risk that dipstick testing fails 
  # because of farmers misuse
  
  dipstick_total_risk <- dipstick_farmers_misuse_risk 
  
  #### Dipstick assay total  ----
  
  dipstick_total_cashflow <- dipstick_benefits - dipstick_costs
  
  dipstick_total <- 
    ((dipstick_benefits * dipstick_total_risk) - dipstick_costs) 
                                            - status_quo_total
                                              
  
  # NPV calculation ----
  
  NPV_status_quo_total <-
    discount(x = status_quo_total, 
             discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_AF36_total <- 
    discount(x = AF36_total, 
             discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_selfst_total <- 
    discount(x = selfst_total, 
             discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_incentive_total <- 
    discount(x = incentive_total, 
             discount_rate = discount_rate, calculate_NPV = TRUE)
  
  NPV_dipstick_total <- 
    discount(x = dipstick_total, 
             discount_rate = discount_rate, calculate_NPV = TRUE)

  # Return values ----

  return(list(NPV_dipstick_total = NPV_dipstick_total, 
              NPV_selfst_total = NPV_selfst_total, 
              NPV_AF36_total = NPV_AF36_total, 
              NPV_incentive_total = NPV_incentive_total,
              NPV_status_quo_total = NPV_status_quo_total,
              AF36_total_cashflow = AF36_total_cashflow,
              selfst_total_cashflow = selfst_total_cashflow,
              incentive_total_cashflow = incentive_total_cashflow,
              dipstick_total_cashflow = dipstick_total_cashflow))


} 
 
   ########### END OF MODEL 

# Decision Analysis -----
 # you can define a different distribution shape if some values need to be taken
 # more in consideration - find it in the DA package

AF_results <-
  mcSimulation(estimate = estimate_read_csv("AF_input.csv"),
               model_function = AF_cost_benefit_risk_simulation,
               numberOfModelRuns = 10000,
               functionSyntax = ("plainNames"))

# here we are doing a mini montecarlo of 100 runs to figure things out


# Aflatoxin reduction decision options 


decisionSupport::plot_distributions(mcSimulation_object = AF_results, 
                                    vars = c("NPV_incentive_total", 
                                              "NPV_AF36_total",
                                             "NPV_selfst_total",
                                             "NPV_dipstick_total"),
                                    method = 'smooth_simple_overlay',
                                    old_names = c("NPV_incentive_total", 
                                                  "NPV_AF36_total",
                                                  "NPV_selfst_total",
                                                  "NPV_dipstick_total"),
                                    new_names = c("NPV no-cash incentive",
                                                  "NPV AF36",
                                                  "NPV self-storage",
                                                  "NPV dipstick assay"),
                                    colors = c('limegreen', 'cyan',
                                               'gold', 'purple'),
                                    x_axis_name = "Net Present Value ($ MXN)",
                                    y_axis_name = " ",
                                     base_size = 15)

#Projection to Latent Structures (PLS) analysis
pls_result <- plsr.mcSimulation(object = AF_results,
                                resultName = names(AF_results$y)[4], ncomp = 1,
                                base_size = 15)


input_table <- read.csv("AF_input.csv")

plot_pls(pls_result, input_table = input_table, threshold = 0)


#multiEVPI 
mcSimulation_table <- data.frame(AF_results$x, AF_results$y[1:5])


evpi <- multi_EVPI(mc = mcSimulation_table, 
                   first_out_var = "NPV_dipstick_total")

plot_evpi(evpi, decision_vars = "NPV_incentive_total")

plot_evpi(evpi, decision_vars = "NPV_AF36_total")

plot_evpi(evpi, decision_vars = "NPV_selfst_total",
          new_names = "NPV self-storage intervention",
          base_size = 15)

plot_evpi(evpi, decision_vars = "NPV_dipstick_total",
          new_names = "NPV Dipstick assay intervention",
          base_size = 15)

# boxplots 
#NPV total - SQ
decisionSupport::plot_distributions(mcSimulation_object = AF_results, 
                                    vars = c("NPV_selfst_total",
                                             "NPV_incentive_total", 
                                                      "NPV_AF36_total",
                                                    "NPV_dipstick_total"),
                                    method = 'boxplot_density',
                                    old_names = c("NPV_selfst_total", 
                                                  "NPV_incentive_total",
                                                  "NPV_AF36_total",
                                                  "NPV_dipstick_total"),
                                    new_names = c("NPV self-storage",
                                                  "NPV No-cash incentive",
                                                  "NPV AF36",
                                                  "NPV dipstick assay"),
                                    colors = c('limegreen', 'cyan',
                                               'gold', 'purple', 'blue'),)

# boxplot maize lost in field by aflatoxins on each option
# maize loss by AFs at the field
decisionSupport::plot_distributions(mcSimulation_object = AF_results, 
                                    vars = c("af36_maize_loss_field", 
                                             "dipstick_maize_loss_field",
                                             "incentive_maize_loss_field",
                                             "selfst_maize_loss_field"),
                                    method = 'boxplot',
                                    old_names = c("af36_maize_loss_field", 
                                             "dipstick_maize_loss_field",
                                             "incentive_maize_loss_field",
                                             "selfst_maize_loss_field"),
                                    new_names= c("AF36 biocontrol",
                                                 "Dipstick assay",
                                                 "No-cash incentive",
                                                 "Self-storage"),
                                    colors = c('limegreen', 'cyan',
                                               'gold', 'purple'),
                                    x_axis_name = "Interventions",
                                    y_axis_name = "Percentage of maize loss
                                    at field",
                                    base_size = 15)
#maize loss by AFs at storage
decisionSupport::plot_distributions(mcSimulation_object = AF_results, 
                                    vars = c("af36_maize_loss_storage", 
                                             "dipstick_maize_loss_storage",
                                             "incentive_maize_loss_storage",
                                             "selfst_maize_loss_storage"),
                                    method = 'boxplot',
                                    old_names = c("af36_maize_loss_storage", 
                                                  "dipstick_maize_loss_storage",
                                                  "incentive_maize_loss_storage",
                                                  "selfst_maize_loss_storage"),
                                    new_names= c("AF36 biocontrol",
                                                 "Dipstick assay",
                                                 "No-cash incentive",
                                                 "Self-storage"),
                                    x_axis_name = "Interventions",
                                    y_axis_name = "Percentage of maize loss 
                                    in storage",
                                    colors = c('springgreen', 'cyan',
                                               'gold', 'purple'),
                                    base_size = 15)

                                    
#Losses storage or field per intervention

#AF36 losses per storage or field
AF36_loss_pie <- c(af36_maize_loss_field, af36_maize_loss_storage)
colors_AF36_pie <- c("palegreen", "seagreen3")
AF36_pie_labels <- round (AF36_loss_pie/ sum(AF36_loss_pie) * 100,1)
AF36_pie_labels <- paste (AF36_pie_labels, "%", sep= " ")
pie(AF36_loss_pie, main = "AF36 intervention losses at field or storage", 
    col = colors_AF36_pie, labels = AF36_pie_labels, cex=.8 )
legend (1.5, 0.5, c("field", "storage"), cex = .8, fill = colors_AF36_pie )

#self-storage losses per storage or field
selfst_loss_pie <- c(selfst_maize_loss_field, selfst_maize_loss_storage)
colors_selfst_pie <- c("plum3", "mediumorchid")
selfst_pie_labels <- round(selfst_loss_pie/ sum(selfst_loss_pie) * 100,1)
selfst_pie_labels <- paste (selfst_pie_labels, "%", sep = " ")
pie(selfst_loss_pie, main = "Self-storage intervention losses at field or 
    storage", col = colors_selfst_pie, labels = selfst_pie_labels, cex = .8)
legend (1.5, 0.5, c("field", "storage"), cex = .8, fill = colors_selfst_pie)

#no-cash incentive losses per storage or field
incentive_loss_pie <- c(incentive_maize_loss_field, 
                        incentive_maize_loss_storage)
colors_incentive_pie <- c("goldenrod2", "orange")
incentive_pie_labels <- round(incentive_loss_pie/ sum(selfst_loss_pie) * 100,1)
incentive_pie_labels <- paste(selfst_pie_labels, "%", sep = " ")
pie(incentive_loss_pie, main = "No-cash incentive intervention losses at field
    or storage", col = colors_incentive_pie, labels = incentive_pie_labels,
    cex =.8)
legend (1.5, 0.5, c("field", "storage"), cex = .8, fill = colors_incentive_pie)

#dipstick assay losses per storage or field
dipstick_loss_pie <- c(dipstick_maize_loss_field, dipstick_maize_loss_storage)
colors_dipstick_pie <- c("darkturquoise", "deepskyblue")
dipstick_pie_labels <- round(dipstick_loss_pie/ sum(dipstick_loss_pie) *100,1)
dipstick_pie_labels <- paste (dipstick_pie_labels, "%", sep = " ")
pie(dipstick_loss_pie, main = "Dipstick assay intervention losses at field
    or storage", col = colors_dipstick_pie, labels = dipstick_pie_labels,
    cex =.8)
legend (1.5, 0.5, c("field", "storage"), cex = .8, fill = colors_dipstick_pie )



#cashflow


plot_cashflow(mcSimulation_object = AF_results, 
              cashflow_var_name = c("AF36_total_cashflow", 
                                    "selfst_total_cashflow",
                                    "incentive_total_cashflow",
                                    "dipstick_total_cashflow"),
              x_axis_name = "Years with intervention",
              y_axis_name = "Annual cashflow in $ MXN pesos",
              color_25_75 = "slateblue3", color_5_95 = "turquoise2",
              color_median = "red", 
              facet_labels = c("AF36 intervention", "Self-storage intervention",
                               "No-cash incentive intervention",
                               "Dipstick assay intervention"),
              base_size = 15)

