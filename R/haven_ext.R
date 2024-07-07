
# Use \link[haven]{read_dta} directly

# @title Read data with preliminary error checking
#
# @description ..
# 
# @param file \link[base]{character} scalar, file name (extension not needed).
# 
# @param ... 
#  
# @details ..
# 
# @note 
# `foreign::read.dta` requires Stata version 5 (or newer?), 
# while \link[haven]{read_dta} (returns `'tbl_df'` object) do not have this issue.
# 
# @importFrom haven read_dta
# @export
#read_dta_file <- function(file, ...) {
#  read_dta(file = file)
#}

