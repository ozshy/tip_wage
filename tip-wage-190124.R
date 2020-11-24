#tip-wage-190124.R Adding some calculations for Sec. 4.2 (measurement errors)
#tip-wage-190122.R End of file: Adding diag graph for presentation only!
#tip-wage-190121.R corresponds to tip-wage-25.tex (posted online and SSRN)
# 190119 revising using the 2019 data
# R-code for "Tipped Minimum Wage: Data, Theory, and Calibrations"
# By Oz Shy. ozshy@ozshy.com
# URL for downloading this code and the data: https://www.ozshy.com/tip-wage-data-R.zip
### tip-wage-180713.R Trimming code for tip-wage-19.tex
### tip-wage-180710.R Major revision: Calibrations using graphs instead of tables assuming 30 hours/week
### tip-wage-180406.R Trying to improve graphs, line 322, while still under revidew

# Instructions:
# 1) Place the R-code and the data file tip-wage-data-180129.csv on the same directory as this R-code.
# 2) In the R-code, change directory to where your put the data file and this R-cocde. If you are using R Studio, go to Session: Set Working Directory: To Source File Location.
# 3) Make sure that you have the relevant R Packages listed at the beginning. Then, load these pacakges using the library() command. The location of these packages on your computer may be different from the current specification. If this is the case, make changes accordingly. 
# 4) This R-code contains comments that will direct you to the relevant code for Section 1 and Section 3. 
# 5) Variables are defined beginning Line 34 (Dictionary)

# Packages used (location can be different on your computer):
library("ggplot2") #package for making plots
theme_set(theme_bw())# White background for plots 
library(xtable)# Pacakge that converts tables (dataframes) into LaTeX format
library("reshape2")# Needed for Figure 1 to convert wide to long data
library("tidyr")# Similar to reshape2 for converting wide to long in Fig 1
setwd("C:/Users/Oz/Papers/tip-wage")# See Instruction # 2) above because you may be using a different directory to store the code and data.
dir()# Just to make sure that the data file (see next line) is in this directory
tw_raw=read.csv("tip-wage-data-190119.csv")# Read raw data
names(tw_raw)# "tw" stands for "tip-wage." There are many columns that are not needed (will be removed)
dim(tw_raw)#51 states (raws), DC included
tw_raw$state_name

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

tw3=tw_raw
dim(tw3)
names(tw3)
any(is.na(tw3))# Search for missing data (NA)
tw3_na=apply(tw3, 1, function(x) any(is.na(x)))# Search for NA by row (Note: 1 refers to apply by row)
tw3_na# Visually inspect which row as a missing entry (True)
tw3[tw3_na==T,"state_name"]# List the states that have NA (missing values)
tw3[which(tw3_na==T),]# Inspect which column has NA
# 
# Below, multiply eat_sales by 3.3% (estimated sales growth rate from 2017 to 2018)
tw3$eat_sales
tw4 = tw3
(tw4$eat_sales = 1.033 * tw3$eat_sales)# adding 3.3%
summary(tw3$eat_sales)# summary 2017 sales
summary(tw4$eat_sales)# summary assumed 2018 sales (used in the paper)
#
# Now set minimum wage to 7.25 (Federal Level) for states with lower than 7.25 or no min wage at all
tw4$mw# View states' non-tipped minimum wages (those with lower than 7.25 should be fixed)
tw4$mw[which(tw4$mw<7.25)]=7.25# Set min wage to the Federal level for states with lower than $7.25
tw4$mw
tw4$mw_tip_sm # Inspect tipped min wage to make sure it is not lower than $2.13 (already set in the BLS data)
tw4$mw_tip_lg # Inspect tipped min wage to make sure it is not lower than $2.13 (already set in the BLS data)
#
# Computing the gap between minimum wage and (higher) min wage for tipped workers.
(tw4$mw_gap = tw4$mw - tw4$mw_tip_lg)# Adding new column measuring the gap between min wage and min wage for tipped employees

### Section 2 of the paper: Observations ###
#Basic statistics for mw, mw_tip, and mw_gap described in Section 2
#
# Which state pays the highest full mw?
tw4[which(tw4$mw == max(tw4$mw)), ]
# Which state has the highest gap?
tw4[which(tw4$mw_gap == max(tw4$mw_gap)), ]
# WA state
tw4[which(tw4$state_code == "WA"), ]
# which states adhere to $2.13 Federal tipped mw?
tw4[which(tw4$mw_tip_lg == 2.13), "state_name"]
length(tw4[which(tw4$mw_tip_lg == 2.13), "state_name"]) # how many states?
# which states adhere to $7.25 Federal full mw?
tw4[which(tw4$mw== 7.25), "state_name"]
length(tw4[which(tw4$mw == 7.25), "state_name"]) # how many states?
# which states adhere to both $2.13 and $7.25 Federal mw?
tw4[which((tw4$mw_tip_lg== 2.13) & (tw4$mw== 7.25)), "state_name"]
length(tw4[which((tw4$mw_tip_lg== 2.13) & (tw4$mw== 7.25)), "state_name"])
# 
round(summary(tw4$mw_tip_lg),digits = 2)# Summary of tipped minimum wage across 51 states
round(sd(tw4$mw_tip_lg),digits = 2)# Standard deviation of tipped minimum wage
round(summary(tw4$mw),digits=2)# Summary of non-tipped minimum wage across 51 states
round(sd(tw4$mw),digits = 2)# Standard deviation of non-tipped minimum wage
round(summary(tw4$mw_gap),digits = 2)# Summary of gaps between non-tipped and tipped min wages across states
round(sd(tw4$mw_gap),digits = 2)# Standard deviation of gaps between non-tipped and tipped min wages across states
#
### Table 1 in Section 2 (Observations)
names(tw4) # Below remove mw_tip_sm and reorder columns:
table1=subset(tw4, select = 
    c("state_name","state_code","mw_tip_lg","mw","mw_gap",
    "eat_sales","eat_jobs")) # 
names(table1)
str(table1)
#
# Reorder (sort decending) states according to mw gap then tipped mw:
table1 = table1[order(-table1$mw_gap, -table1$mw_tip_lg), ] 
head(table1)
# Assigning more explicit column headings:
colnames(table1)=c("State","Code","Min.W tip", "Min.W",
        "Gap","Sales $b","Industry Jobs")
names(table1)# View revised column headings
head(table1)# View first few lines in Table 1
# Table 1 in LaTeX format (pasted into the paper)
print(xtable(table1),include.rownames = F)

### Figure 1 in Section 2 (Observations)
# 
# Converting from wide data to long data
tw5=subset(tw4, select = c("state_name","state_code","mw","mw_tip_lg","mw_gap"))
names(tw5)
dim(tw5)
# 
# Reordering Fig 1 according to gap
tw50=tw5 
colnames(tw50)
colnames(tw50)[colnames(tw50)=="mw_tip_lg"]="mw_tip"
colnames(tw50)#Shortening column name
str(tw50)
# Below, re-ordering by: gap, mw_tip, and mw
tw51=tw50[order(tw50$mw_gap, tw50$mw_tip,tw50$mw),]
dim(tw51)
str(tw51)
tw51$state_name
row.names(tw51)=1:nrow(tw51)#Reorder row.names afer sorting
state_list=factor(tw51$state_name, as.character(unique(tw51$state_name)))
state_list # This is the factor levels as state_name (instead of alphabetical) 
tw52=tw51
tw52$state_name=factor(tw51$state_name, as.character(unique(tw51$state_name)))
names(tw52)
tw53 = subset(tw52,select = -state_code) #state_code is not used in Figure 1, now deleted
names(tw53) 
dim(tw53)
str(tw53)
# 
#converting to long data, see https://tidyr.tidyverse.org/reference/gather.html
tw53.long=gather(tw53, mw_type, wage, -state_name)
dim(tw53.long)
head(tw53.long)
# 
# Drawing Figure 1 using ggplot2
(fig1 = ggplot(tw53.long, aes(x = wage, y = state_name, color = mw_type, shape = mw_type)))
(fig1 = fig1 + geom_point(size = 3))
(fig1 = fig1+scale_x_continuous(breaks = seq(0, 13.5, 0.5), sec.axis = dup_axis()))
(fig1=fig1+labs(x="Dollars per hour",y="50 U.S. States and DC"))
(fig1=fig1+theme(legend.position = c(0.95,0.6),legend.justification = c(1,0),
                 legend.title = element_blank()))
(fig1 = fig1 + scale_color_discrete(labels = c("Min wage (non-tipped)","Difference (Gap)","Min wage (tipped)")) 
  + scale_shape_discrete(labels = c("Min wage (non-tipped)","Difference (Gap)","Min wage (tipped)")))#change legend lables

### Figure 2 (box plot) in Section 2 (Observations)
# 
boxplot(tw4$mw_tip_lg, tw4$mw_gap, tw4$mw, axes=F,
        col = c("cyan","green","red"),
        names = c("Min wage tipped","Difference","Min wage"))
axis(2, at=0:14,las=2)
axis(1,at=c(1:3),
     labels=c("Min wage (tipped)","Difference (gap)","Min wage (non-tipped)"))
#legend(1,1,c("Min wage","Min wage (tipped)","Difference"))
axis(4, at=0:14,las=2)

### Section 4 of the paper (Simulations) 
#
names(tw4) # back to tw4
# some statstics discussed at the beginning of Section 4
# which states have same mw for tipped and non-tipped?
tw4[which(tw4$mw_gap == 0), "state_name"]
# mw gaps in NY and MA
tw4[tw4$state_code == "NY", "mw_gap"]
tw4[tw4$state_code == "MA", "mw_gap"]
# gap statistics
summary(tw4$mw_gap)
#
cal1 = tw4
str(cal1)
(tw4$eat_sales) # 2017 (already adjusted for 2018) sales revenue by state 
# Below, adding column with 2017 yearly sales revenue per employee in each state and converting from billons to dollars
cal1$rev_emp=10^9*cal1$eat_sales/cal1$eat_jobs 
cal1$rev_emp#
# 
# Below, compute state-level yearly tip income per employee for tipping rates 5%, 10%, 15%, 20%
cal2 = cal1 
(cal2$tip5_emp=0.05*cal1$rev_emp)# yearly tips per worker assuming 5%
(cal2$tip10_emp=0.1*cal1$rev_emp)# yearly tips per worker assuming 10%
(cal2$tip15_emp=0.15*cal1$rev_emp)# yearly tips per worker assuming 15%
(cal2$tip20_emp=0.2*cal1$rev_emp)# yearly tips per worker assuming 20%
# 
n_hours = 30 # Assumed weekly number of hours waiters work
# Recall data
names(cal2) # The tip5, tip10, tip15, and tip20 columns are YEARLY tip income per-worker
cal3 = subset(cal2, select = c(state_name, mw_gap, tip10_emp, tip15_emp, tip20_emp))
names(cal3)
# Convert yearly tips per employee to hourly tip per employee:
cal4 = cal3
cal4$tip10_hr= cal4$tip10_emp/(52*n_hours)
cal4$tip15_hr= cal4$tip15_emp/(52*n_hours)
cal4$tip20_hr= cal4$tip20_emp/(52*n_hours)
head(cal4)
# Sort (reorder) states according to gap
cal5=cal4[order(cal4$mw_gap, cal4$tip15_hr), ]#ordering by: gap and tip15_hr
head(cal5)
tail(cal5)
cal6 = subset(cal5, select = c("state_name", "tip15_hr", "mw_gap", "tip20_hr"))
head(cal6)
tail(cal6)
# 
# Conversion to long (maintaining the order according to gap)
cal7 = cal6
cal7 = cbind(cal6, tag = 1:nrow(cal6)) # Creating a tag according to which melt will sort 
head(cal7)
tail(cal7)
cal8 = melt(cal7, id.vars = c("tag", "state_name"))
dim(cal8)
head(cal8)
tail(cal8)

### Figure 4: gap, tip 15_hr and tip20_hr
names(cal8)
str(cal8)
(fig_cal = ggplot(cal8, aes(x = value, y = reorder(state_name, tag), color = variable, shape = variable)))
(fig_cal = fig_cal + geom_point(size = 3))
(fig_cal = fig_cal+scale_x_continuous(breaks = seq(0, 11.5, 0.5), sec.axis = dup_axis()))
(fig_cal=fig_cal+labs(x="Dollars per hour",y="50 U.S. States and DC"))
(fig_cal=fig_cal+theme(legend.position = c(0.48,0.6),legend.justification = c(1,0),
                 legend.title = element_blank()))
(fig_cal = fig_cal + scale_color_discrete(labels = c("Hourly tip income (15%)","Gap = Min wage - tipped min wage ","Hourly tip income (20%)")) 
  + scale_shape_discrete(labels = c("Hourly tip income (15%)","Gap = Min wage - tipped min wage ","Hourly tip income (20%)")))#change legend labels

## Analyses and discussions in Section 4
names(cal6)
dim(cal6)
tw4[tw4$state_name == "Nebraska", ]
cal6[cal6$state_name == "Nebraska", ]
tw4[tw4$state_name == "Virginia", ]
cal6[cal6$state_name == "Virginia", ]
#
# For which states, gap > tip15_hr ?
(gap_15 = cal6[cal6$mw_gap >= cal6$tip15_hr, ])
sort(gap_15$state_name)
(length(gap_15$state_name))
# For which states, gap > tip20_hr ?
(gap_20 = cal6[cal6$mw_gap >= cal6$tip20_hr, ])
sort(gap_20$state_name)
(length(gap_20$state_name))
# for which states tip20_hr MINUS gap < $0.4 ?
(less40 = cal6[which(cal6$tip20_hr - cal6$mw_gap >0 & cal6$tip20_hr - cal6$mw_gap < 0.4), ] )
(less40$state_name)
(length(less40$state_name))
# tip20_hr MINUS gap in these states
round(less40$tip20_hr - less40$mw_gap, digits = 2)

### Subsection 4.2 (measurement errors)
#Some aggregate statistics on total tips in the US
#
# Total US sales revenue
(sales_us_2017 = sum(tw3$eat_sales)) # 2017 as appears on NRA
# 
# Total tip income in the entire US assuming 15% and 20% tipping rates
(tip_us_05 = 0.05 * sum(tw4$eat_sales))
(tip_us_10 = 0.10 * sum(tw4$eat_sales))
(tip_us_15 = 0.15 * sum(tw4$eat_sales))
(tip_us_20 = 0.20 * sum(tw4$eat_sales))


### tip-wage-190122.R adding diag graph (for presentation only)
# Draw min wage vs. min wage tipped using wide data, tw5
# with regression
# # Regression with intercept
# (diag.lm = lm(mw_tip_lg ~ mw, data = tw5))
# diag.lm$coefficients
# (diag.int = diag.lm$coefficients[1])
# (diag.slope = diag.lm$coefficients[2])
#
# Regression no intercept
(diag.lm = lm(mw_tip_lg ~ 0 +mw, data = tw5))
diag.lm$coefficients
diag.int = 0 # for the abline in graph
(diag.slope = diag.lm$coefficients[1])
#
ggplot(tw5,aes(y=mw_tip_lg,x=mw))+
  geom_point(size=2)+
  scale_x_continuous(breaks = seq(7,13.5,by=0.5), sec.axis = dup_axis())+
  scale_y_continuous(breaks = seq(2,12.5,by=0.5), sec.axis = dup_axis())+
  labs(x="Full minimum wage (dollars per hour)",
       y="Tipped minimum wage (dollars per hour)")+
  # stat_smooth(method = lm)+ #with 95% confidence intervals
  geom_abline(intercept = diag.int,slope = diag.slope,color="blue",size=1.0)+
  geom_abline(intercept = 0,slope = 1,color="red",size=1.0)+
  geom_text(aes(label=state_code),size=3,vjust=0.75,hjust=1.45)+
  annotate("text",label="(Tipped min wage = 0.535 full min wage)",x=11,y=7,color="blue")+
  annotate("text",label="(Tipped min wage = full min wage)",x=12.2,y=10.6,color="red")

### End of R-code corresponding to tip-wage-26.tex ###
