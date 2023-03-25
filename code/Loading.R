# Loading all existing files after each other as a start up process


# Prepare + Process -------------------------------------------------------


source("code/2_prepare/1_CSV load.R")
# source("code/2_prepare/2_Identify data structures.R") ## Skipping as it's not creating or changing tables
source("code/3_process/1_Fix DATE issues.R")

source("code/3_process/2_Corelation.R")

source("code/3_process/3_Wide vs Long.R")

source("code/3_process/4_Reformat_minute_sleep.R")


# Analyze -----------------------------------------------------------------

source("code/4_analyze/2_plots_daily_sleep_day.R")

source("code/4_analyze/1_prep_merge_and_order.R")
source("code/4_analyze/1_prep_remove_duplicates.R")

# Plots - should not be needed for loading
# source("code/4_analyze/2_plots_daily_merged.R")
# source("code/4_analyze/2_plots_hourly_merged.R")
# source("code/4_analyze/2_plots_minute_merged.R")
# source("code/4_analyze/2_plots_weight_log_info.R")
