# outcome-balanced-sampling
Code for simulation studies in "Valid and practical class imbalance corrections for risk prediction models: methods for maintaining performance, accurate validation, and computational efficiency" 

Because clinical records data cannot be shared publicly, contents of this repository cannot be used to reproduce paper simulations. The code provided is specific to the self-harm outcomes dataset and must be modified for another application. It is provided so that interested readers can see how balanced sampling methods were implemented and adapt the code to their specific needs. This repository also contains code for a balanced sampling approach using xgboost, which was not included in msnuscript.

Simulation study "wrapper" scripts for each method (RF, ANN, and XGB) can be found in simulation_function_rf.R, simulation_function_nn.R, and simulation_function_xgb.R, respectively. The wrapper function does the following for each simulation iteration:
1. generate_sim_data.R samples data for training, testing, and validation sets from the exisitng dataset 
2. fit_<model>_on_genrate_sim_data estimate the prediction <model< (RF, ANN, or XGB) using the specified sampling scheme. *These scripts should be references for code to implement outcome balanced sampling.*
3. calibrates predictions in testing and validation sets (using calibration model estimated in the testing set) using functions defined in calibration_functions.R
4. evaluates prediction model performance in testing and validation sets using functions defined in performance_functions.R
Additional "helper" functions called in the simulation code are also uploaded to the repository, including format_simulations.R,  sboot_edits.R (to perform stratified bootstrap sampling), and tibble_to_XGB.R

Simulation results are summarized in perf_eval_<model>.R for RF, ANN, and XGB.

Questions can be directed to rebecca.y.coley@kp.org
