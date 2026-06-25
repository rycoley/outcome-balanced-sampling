### RED Suicide XGBoost Model Fitting
### Binds and formats simulation results held in a folder for all K simulations 
### to produce summary results and shell tables. Output is a list containing
### a master results object that lists metrics for each specific simulation,
### a summarized results object that computes average values and quantile-based 
### CI across all simulations, a results table that extracts and summarizes
### key evaluation metrics listed in the statistical analysis plan (SAP), and a 
### table describing average computational time metrics listed in the SAP.
### Arguments:
###   max_iter          Number of simulations (sim_replications)
###   rate              Number of non-events relative to cases (1:1 = 1; 1:2 = 2; 1:5=5)
###   model             String descriptor of the modeling approach (XGBoost, RF, etc)
###   digits            Number of significant figures results tables should be rounded to
###   folder_path     String describing the path and first part of the file name of prediction results

format_simulations <- function(max_iter, rate, model, digits, folder_path){
  ## Read all simulation results from the results folder and append them 
  ## together into a list with each element is a simulation-specific results object
  sim.res <- lapply(1:max_iter, function(x) {
    filename <- paste0(folder_path, x, ".rds")
    return(readRDS(filename))})
  
  
  ## Create a master results list which reformats the individual results from each
  ## simulation and creates a 15-element list containing simulation results indexed
  ## by their type (e.g. test point estimates across simulations, coverage 
  ## indicators across simulations). Offers simulation-specific results.
  new_list <- list()
  for(list_element in 1:14){
    for(list in 1:max_iter){
      if(list_element %in% c(10:13)){
        if(list == 1){
          list_element_name <- names(sim.res[[1]])[list_element]
          temp <- data.frame(t(sapply(sim.res[[list]][list_element][1],c))) %>% 
            mutate(simulation = list, .before=1)
          new_list[[list_element_name]] <- temp 
        }
        else{
          temp <- data.frame(t(sapply(sim.res[[list]][list_element][1],c))) %>% 
            mutate(simulation = list, .before=1)
          new_list[[list_element_name]] <- bind_rows(new_list[[list_element_name]] %>% as.data.frame(), temp)
        }}
      
      else if(list_element %in% c(6:8)){
        if(list == 1){
          list_element_name <- names(sim.res[[1]])[list_element]
          temp <- sim.res[[list]][list_element] %>% as.data.frame() %>% 
            rownames_to_column("term") %>% mutate(simulation = list, .before=1)
          colnames(temp) <- gsub("^.*\\.","", colnames(temp))
          new_list[[list_element_name]] <- temp 
        }
        else{
          temp <- sim.res[[list]][list_element] %>% as.data.frame() %>% 
            rownames_to_column("term") %>% mutate(simulation = list, .before=1)
          colnames(temp) <- gsub("^.*\\.","", colnames(temp))
          new_list[[list_element_name]] <- bind_rows(new_list[[list_element_name]] %>% as.data.frame(), temp)
        }}
      
      else{
        if(list == 1){
          list_element_name <- names(sim.res[[1]])[list_element]
          temp <- sim.res[[list]][list_element] %>% as.data.frame() %>% 
            mutate(simulation = list, .before=1)
          colnames(temp) <- gsub("^.*\\.","", colnames(temp))
          new_list[[list_element_name]] <- temp 
        }
        else{
          temp <- sim.res[[list]][list_element] %>% as.data.frame() %>% 
            mutate(simulation = list, .before=1)
          colnames(temp) <- gsub("^.*\\.","", colnames(temp))
          new_list[[list_element_name]] <- bind_rows(new_list[[list_element_name]] %>% as.data.frame(), temp)
        }}
    }}
  
  ## Reformat master list s.t. each element averages across simulations within 
  ## each element and appends quantile based CI. Offers overview of results across 
  ## simulations
  averages_list <- list()
  for(list_element in 1:14){
    if(list_element %in% c(1, 2, 9)){
      averages_list[[list_element]] <- new_list[[list_element]] %>%
        group_by(Pctile, strata) %>%
        summarize(across(where(is.numeric)&!simulation, 
                         ~(paste0(round(mean(.x, na.rm=TRUE),digits), " (",
                                  round(quantile(.x,.025, na.rm = T),digits),", ", 
                                  round(quantile(.x, .975, na.rm = T),digits), ")"))))
    }
    else if(list_element %in% 6:8){
      averages_list[[list_element]] <- new_list[[list_element]] %>%
        group_by(term) %>%
        summarize(across(where(is.numeric)&!simulation, 
                         ~(paste0(round(mean(.x, na.rm=TRUE),digits), " (",
                                  round(quantile(.x,.025, na.rm = T),digits),", ", 
                                  round(quantile(.x, .975, na.rm = T),digits), ")")))) 
    }
    else if(list_element %in% 10:13){
      averages_list[[list_element]] <- new_list[[list_element]] %>%
        select(-c(user.child, sys.child))%>%
        summarize(across(where(is.numeric)&!simulation, 
                         ~(paste0(round(mean(.x, na.rm=TRUE),digits), " (",
                                  round(quantile(.x,.025, na.rm = T),digits),", ", 
                                  round(quantile(.x, .975, na.rm = T),digits), ")"))))
    }  
    else if(list_element %in% 3:4){
      averages_list[[list_element]] <- new_list[[list_element]] %>%
        summarize(across(where(is.numeric)&!simulation, 
                         ~(paste0(round(mean(.x, na.rm=TRUE),digits), " (",
                                  round(quantile(.x,.025, na.rm = T),digits),", ", 
                                  round(quantile(.x, .975, na.rm = T),digits), ")")))) 
    }
    else if(list_element == 5){
      averages_list[[list_element]] <- new_list[[list_element]] %>%
        group_by(Estimate, Pctile, strata) %>%
        summarize(across(where(is.numeric)&!simulation, 
                         ~(paste0(round(mean(.x, na.rm=TRUE),digits), " (",
                                  round(quantile(.x,.025, na.rm = T),digits),", ", 
                                  round(quantile(.x, .975, na.rm = T),digits), ")")))) 
    }  
    else{
      averages_list[[list_element]] <- new_list[[list_element]] %>%
        group_by(Measure, Pctile, strata) %>%
        summarize(across(where(is.numeric)&!simulation, 
                         ~(paste0(round(mean(.x, na.rm=TRUE),digits), " (",
                                  round(quantile(.x,.025, na.rm = T),digits),", ", 
                                  round(quantile(.x, .975, na.rm = T),digits), ")")))) 
    }}
  
  names(averages_list) <- names(new_list)
  
  ## Computes shell table of results from the master list
  results_table <- list(new_list$point.estimates %>% 
                          select(-c(RiskCutpoint, Calibration))%>%
                          pivot_longer(cols = !c(simulation, Pctile, strata), 
                                       names_to = "Metric") %>%
                          group_by(Pctile, strata, Metric) %>%
                          summarize(test_mean = round(mean(value,na.rm=TRUE), digits),
                                    test_se = round(sd(value,na.rm=TRUE)/sqrt(n()), digits),
                                    ci_width = round((quantile(value, .975, na.rm=T)-quantile(value, .025, na.rm=T)), digits)) %>%
                          ungroup() %>%
                          mutate(Metric = factor(Metric, levels=c("AUC","AUCPR", "Accuracy", "Sens", "Spec", "PPV", "NPV", "Fscore", "Brier")),
                                 strata = relevel(as.factor( str_remove(strata, "race")), ref="overall")) %>%
                          arrange(Metric, Pctile, strata)  %>%
                          select(Metric, strata, Pctile, everything()),
                        
                        new_list$validation.point.estimates%>% 
                          select(-c(RiskCutpoint, Calibration))%>%
                          pivot_longer(cols = !c(simulation, Pctile, strata), names_to = "Metric") %>%
                          group_by(Pctile, strata, Metric) %>%
                          summarize(valid_mean = round(mean(value,na.rm=TRUE), digits)) %>%
                          ungroup() %>%
                          mutate(Metric = factor(Metric, levels=c("AUC","AUCPR", "Accuracy", "Sens", "Spec", "PPV", "NPV", "Fscore", "Brier")),
                                 strata = relevel(as.factor( str_remove(strata, "race")), ref="overall")) %>%
                          arrange(Metric, Pctile, strata)  %>%
                          select(Metric, strata, Pctile, everything()),
                        
                        new_list$bias%>% 
                          select(-c(RiskCutpoint, Calibration))%>%
                          pivot_longer(cols = !c(simulation, Pctile, strata), names_to = "Metric") %>%
                          group_by(Pctile, strata, Metric) %>%
                          summarize(bias_mean = round(mean(value,na.rm=TRUE), digits),
                                    bias_se = round(sd(value,na.rm=TRUE)/sqrt(n()), digits)) %>%
                          ungroup() %>%
                          mutate(Metric = factor(Metric, levels=c("AUC","AUCPR", "Accuracy", "Sens", "Spec", "PPV", "NPV", "Fscore", "Brier")),
                                 strata = relevel(as.factor( str_remove(strata, "race")), ref="overall")) %>%
                          arrange(Metric, Pctile, strata)  %>%
                          select(Metric, strata, Pctile, everything()),
                        
                        new_list$coverage%>%
                          mutate(Metric = factor(Measure, levels=c("AUC","AUCPR", "Accuracy", "Sens", "Spec", "PPV", "NPV", "Fscore", "Brier")),
                                 strata = relevel(as.factor( str_remove(strata, "race")), ref="overall")) %>%
                          group_by(Metric, strata, Pctile) %>%
                          summarize(ci_coverage = round(sum(Accuracy_cover95)/n(), (digits-2))) %>%
                          arrange(Metric, Pctile, strata, ci_coverage)  %>%
                          select(Metric, strata, Pctile, ci_coverage)) %>% reduce(full_join, by=c("Metric", "strata", "Pctile")) %>%
    select(Metric, strata, Pctile, test_mean, test_se, valid_mean, bias_mean, bias_se, ci_width, ci_coverage) %>%
    janitor::clean_names()

  ## Computes shell table of computational times from the master list
  compute_time_table <- tibble(
    "Sampling" = case_when(is.na(rate) ~ "Full", TRUE ~ paste0("1:", rate)),
    "Model" =model,
  #  "total_time" = total.runtime[["elapsed"]],
    "mean_data_gen" = mean(new_list$datgen.runtime[["elapsed"]], na.rm = TRUE),
    "mean_bootstrap" = mean(new_list$boot.runtime[["elapsed"]], na.rm = TRUE),
    "mean_validation" = mean(new_list$valdata.runtime[["elapsed"]], na.rm = TRUE),
    "mean_model_fit" = mean(new_list$modelfit.runtime[["elapsed"]], na.rm = TRUE))
  
  return(list(
    "master_list" = new_list,
    "averaged_list" = averages_list,
    "results_table" = results_table,
    "time_table" = compute_time_table))
  
  rm(sim.res, new_list, averages_list, results_table, compute_time_table)}
