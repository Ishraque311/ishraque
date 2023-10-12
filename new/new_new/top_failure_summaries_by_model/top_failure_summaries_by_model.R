# Libraries
library('data.table')
library('glue')
library('dplyr')
library('tidyr')

# Manual Inputs
#=============

#Top failure type for for each model we want to consider
top_failure_type_per_model <- list('A-10' = c('Obstruction','Locking','Lock Rod - Out of Adjustment'),
                                   'M3' = c('Motor - Not Operating','Obstruction','Locking'),
                                   'M5' = c('Circuit Controller- Out of Adjustment','Obstruction','Motor - Not Operating'))

#Location of the switches
location='Mainline'

#Period of analysis in month
period_month = 30



#User Defined Function
#=====================

# Beautify column names
beautify_column_names <- function (df) {
  for (i in 1:ncol(df)) {
    col_name = colnames(df)[i]
    col_name <- gsub('\\.', '_', tolower(col_name))
    col_name <- gsub('__', '_', col_name)
    col_name <- gsub('_$', '', col_name)
    col_name <- gsub('__', '_', col_name)
    colnames(df)[i] = col_name
  }
  return(df)
}



#Importing Data
#==============

#Importing Combined Data
combined_df = read.csv("datahub/combined-switch-machines.csv")
combined_df = beautify_column_names(combined_df)
colnames(combined_df)[which(names(combined_df) == "system")] <- "equipment"

# Importing Trouble Call - Confirmed Failure Data
fail_df = read.csv("datahub/sg-tblcl-swm-wo-fail.csv")
fail_df = beautify_column_names(fail_df)

#Importing PFAC Definitions
pfac_def = read.csv('datahub/pfca_sg-swmch.csv')
pfac_def = beautify_column_names(pfac_def)

# Definition for each unique  PFAC codes
problem_def <- pfac_def[,c('problem_code','problem_description')] %>% distinct(problem_code, problem_description, .keep_all = TRUE)
failure_def <- pfac_def[,c('failure_code','failure_description')] %>% distinct(failure_code, failure_description, .keep_all = TRUE)
cause_def <- pfac_def[,c('cause_code','cause_description')] %>% distinct(cause_code, cause_description, .keep_all = TRUE)
action_def <- pfac_def[,c('action_code','action_description')] %>% distinct(action_code, action_description, .keep_all = TRUE)


#Merging Combined Data, Trouble Call - Confirmed Failure Data and PFAC code Definition Data
merged_df = merge(x=combined_df, y=fail_df[,c('equipment','problem_code','failure_code','cause_code','action_code')], by.x='equipment', by.y='equipment', all.x = TRUE, all.y=FALSE)
merged_df = merge(x=merged_df, y=problem_def, by.x='problem_code', by.y='problem_code', all.x = TRUE, all.y=FALSE)
merged_df = merge(x=merged_df, y=failure_def, by.x='failure_code', by.y='failure_code', all.x = TRUE, all.y=FALSE)
merged_df = merge(x=merged_df, y=cause_def, by.x='cause_code', by.y='cause_code', all.x = TRUE, all.y=FALSE)
merged_df = merge(x=merged_df, y=action_def, by.x='action_code', by.y='action_code', all.x = TRUE, all.y=FALSE)



#Calculations of Number of Failure, Throws, Throws Per Failure, Month per Failure, and Days per Failure 
failure_stats_df = data.frame()

for (mod in names(top_failure_type_per_model)){
  
  for (fail_type in top_failure_type_per_model[[mod]]){
    
    failure_count = sum(!is.na(merged_df[(((merged_df$failure_description==fail_type) & (merged_df$model==mod)
                                           & (merged_df$location==location)
                                           & (merged_df$usage_switch_throw>0))),'problem_code']))
    
    total_throws = sum(combined_df[((combined_df$model %in% c(mod))
                                    & (combined_df$location %in%c(location))
                                    & (combined_df$usage_switch_throw>0)),'usage_switch_throw'])
    
    throws_per_failure = round(total_throws/failure_count,2)
    month_per_failure = round(period_month/failure_count,2)
    day_per_failure = round(period_month/failure_count*30,2)
    
    output = c(mod, fail_type, failure_count, total_throws,
               throws_per_failure, month_per_failure, day_per_failure)
    print(output)
    failure_stats_df <- rbind(failure_stats_df,output)
    
  }
}

colnames(failure_stats_df)<-c('model','failure_type','failures','thows',
                              'throws_per_failure','month_per_failure','day_per_failure')

write.csv(failure_stats_df, "output\\top_three_failure_type_stast_per_model.csv", row.names=FALSE)