# reservation-prices
This is a code repository to accompany the manuscript with the working title "Using Survival Prediction Techniques to Learn Consumer-Specific Reservation Price Distributions". All experiments were conducted in `Experiments.R` which calls a number of functions from `helper.R` for calculating the profit. 

All the results from the 10-times repeated 10-fold cross validation are saved in the `RDSFiles` folder and can be reas into R using the `readRDS()` function. These RDS files can be replicated by runnning `Experiments.R`. All Tables and Figures were created in `MakeTables_Figures.R`.  

Eventually, all the data files will be uploaded into a `Data` folder.
