# tip_wage

Notes for using the R-code and the data to reproduce a paper entitled:
"Minimum Wage for Tipped Employees: Data, Theory, and Calibrations, "by Oz Shy

Available at: 

1) Place the R-code and the data file tip-wage-data-190119.csv on the same directory as the R-code.
2) In the R-code, change directory to where your put the data file and this R-cocde. If you are using R Studio, go to Session: Set Working Directory: To Source File Location.
3) Make sure that you have the relevant R Packages listed at the beginning of the script. Then, load these pacakges using the library() command. The location of these packages on your computer may be different from the current specification. If this is the case, make changes accordingly. 
4) This R-code contains comments that will direct you to the relevant code for reproducing Section 2 and Section 4. 
5) Variables (Dictionary) is define on Line 30 in the R-code, and also below.
6) Have fun!

### Dictionary of the 9 variables ###
# state_name   : Factor 
# state_code   : Factor (2-letter code)
# mw           : num (minimum wage for non-tipped employees)
# mw_tip_sm    : num (lowest hourly minimum wage for tipped employees, generally applies to small business) # Not used!
# mw_tip_lg    : num (higher hourly minimum wage for tipped employees) used!
# eat_no       : int (Number of eating and drinking places for the state in 2018)
# eat_jobs     : int (Number of employees in eating and drinking places for the state in 2018)
# eat_food_jobs: int (same as above PLUS all other food service employees)
# eat_sales    : num (Estimated sales of eating and drinking places for 2017 in USD billions)
##########
