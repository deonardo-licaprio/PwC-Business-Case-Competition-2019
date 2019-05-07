#TODO: comments

get_uniqe_counts <- function(x) { 
    return(length(unique(x))) 
}

any_over_90_days <- function(x) {
    if(any(x > 90)) { return(1) } else { return(0) }
}

create_features <- function(df_train) {
    df_train %>%
        mutate(
            credit_to_income = Credit_amount / Monthly_Income,
            monthly_savings = Monthly_Income - Monthly_Spendings,
            one_instalment = Credit_amount / Number_of_installments,
            rate_to_savings = one_instalment / monthly_savings,
            nr_of_parents = ifelse(Marital_status == "Maried", 2, 1),
            size_of_family = nr_of_parents + Household_children,
            income_per_person = Monthly_Income / size_of_family,
            chldr_ratio = Household_children / size_of_family,
            value_mean = apply(df_train[,value_var_names], 1, mean),
            value_min = apply(df_train[,value_var_names], 1, min),
            value_max = apply(df_train[,value_var_names], 1, max),
            value_sd = apply(df_train[,value_var_names], 1, sd),
            dpd_mean = apply(df_train[,dpd_var_names], 1, mean),
            dpd_min = apply(df_train[,dpd_var_names], 1, min),
            dpd_max = apply(df_train[,dpd_var_names], 1, max),
            dpd_sd = apply(df_train[,dpd_var_names], 1, sd),
            overdue_mean = apply(df_train[,overdue_var_names], 1, mean),
            overdue_min = apply(df_train[,overdue_var_names], 1, min),
            overdue_max = apply(df_train[,overdue_var_names], 1, max),
            overdue_sd = apply(df_train[,overdue_var_names], 1, sd),
            any_del_in_last_3_month = ifelse(DPD_t0 + DPD_lag1 + DPD_lag2 == 0, 0, 1),
            any_del_in_last_6_month = ifelse(DPD_t0 + DPD_lag1 + DPD_lag2 + DPD_lag3 + DPD_lag4 + DPD_lag5 == 0, 0, 1),
            unemployment_rate = Unemployed_total / Working_age_population,
            unemployment_rate_to_all = Unemployed_total / Total_population,
            unempl_vocational_ratio = Unemployed_vocational_number / Unemployed_total,
            unempl_highschool_ratio = Unemployed_highschool_and_lower_number / Unemployed_total,
            total_spending = apply(df_train[,spending_var_names], 1, sum),
            total_spending_per_person = total_spending / Total_population,
            spending_ratio = Monthly_Spendings / total_spending_per_person,
            income_ratio = Monthly_Income / Average_income,
            debt_to_credit =  NotionalOverdue_t0 / Credit_amount,
            expo_to_credit = NotionalValue_t0 / Credit_amount,
            debt_to_income = NotionalOverdue_t0 / Monthly_Income,
            expo_to_income = NotionalValue_t0 / Monthly_Income,
            any_def_in_the_past = apply(df_train[,dpd_var_names], 1, any_over_90_days),
            monthly_left = monthly_savings - one_instalment,
            credit_per_person = Credit_amount / size_of_family,
            savings_per_operson = monthly_savings / size_of_family,
            Personal_car_number / Total_population,
            Truck_number / Total_population,
            Tractor_number / Total_population,
            Agricultural_tractor_number / Total_population,
            Building_permit_number / Total_population,
            Building_permit_individual_number / Total_population,
            Building_project_subbmission_number / Total_population,
            Apartment_project_subbmission_number / Total_population,
            Apartment_project_subbmission_area / Total_population,
            Total_population_age_15_29_years / Total_population,
            Total_population_age_30_44_years / Total_population,
            Total_population_age_45_59_years / Total_population,
            Total_population_age_60_years_or_older / Total_population,
            Employed_number_men / Employed_number_total,
            Employed_number_women / Employed_number_total, 
            Employed_agricultural_number / Employed_number_total,
            Employed_industry_number / Employed_number_total,
            Emplyed_trade_transport_number / Employed_number_total,
            Employed_finance_number / Employed_number_total
        )
}
