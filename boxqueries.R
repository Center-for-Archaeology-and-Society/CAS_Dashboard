# boxcounts

#' @export
boxcounts = function(group_vars = NULL, con){
  source("functions.R")
  library(dplyr)
  if(length(group_vars) > 1) {
    group_vars = group_vars[which(!group_vars %in% "None")]
  }

  results = NULL
  if(is.null(group_vars[1]) || any(group_vars == "None")){
    results = tbl(con,"Records") %>%
      filter(rec_RecTypeID == 115) %>%
      count(name = "count") %>%
      collect()
  } else {
    results_list = purrr::map(group_vars, getData,con)
    results = purrr::reduce(results_list, function(x, y) {
      full_join(x, y, by = "rec_ID", na_matches = "never")
    }) %>%
      distinct_all() %>%
      group_by(across(all_of(group_vars))) %>%
      count(name = "count")
  }
  return(results)
}

getData = function(var,con){
  source("functions.R")
  library(dplyr)
  var_tbl = readr::read_csv("variables.csv", show_col_types = F)
  var_tbl = var_tbl %>%
    filter(variable == var)
  if(nrow(var_tbl) == 0) stop("no matching variable")
  if(nrow(var_tbl) > 1) stop("must have only one matching variable")
  if(!is.na(var_tbl$rec_RecTypeID)){
    results = tbl(con,"Records") %>%
      filter(rec_RecTypeID == var_tbl$parent_rec_RecTypeID) %>%
      select(rec_ID) %>%
      left_join(
        tbl(con,"recDetails") %>%
          filter(dtl_DetailTypeID == var_tbl$dtl_DetailTypeID) %>%
          select(rec_ID = dtl_RecID, tmp = dtl_Value),
        by = "rec_ID"
      ) %>%
      left_join(
        tbl(con,"Records") %>%
          filter(rec_RecTypeID == var_tbl$rec_RecTypeID) %>%
          select(tmp = rec_ID, !!as.name(var_tbl$variable) := rec_Title),
        by = "tmp"
      ) %>%
      select(-tmp) %>%
      arrange(!!as.name(var_tbl$variable)) %>%
      collect()
  } else if(!is.na(var_tbl$trm_ID)){
    results = tbl(con,"Records") %>%
      filter(rec_RecTypeID == var_tbl$parent_rec_RecTypeID) %>%
      select(rec_ID) %>%
      left_join(
        tbl(con,"recDetails") %>%
          filter(dtl_DetailTypeID == var_tbl$dtl_DetailTypeID) %>%
          select(rec_ID = dtl_RecID, trm_ID = dtl_DetailTypeID, tmp = dtl_Value),
        by = "rec_ID"
      ) %>%
      left_join(
        tbl(con,"defTerms") %>%
          filter(trm_ParentTermID == var_tbl$trm_ID) %>%
          select(tmp = trm_ID, !!as.name(var_tbl$variable) := trm_Label),
        by = "tmp"
      ) %>%
      select(-tmp,-trm_ID) %>%
      arrange(!!as.name(var_tbl$variable)) %>%
      collect()
  } else {
    warning("no matching variable type")
    return(NULL)
  }

  if(var_tbl$parent_rec_RecTypeID != 115){
    results = results %>%
      rename(tmp = rec_ID) %>%
      mutate(tmp = as.character(tmp)) %>%
      left_join(
        tbl(con,"recDetails") %>%
          filter(dtl_DetailTypeID == var_tbl$parent_DetailTypeID) %>%
          select(rec_ID = dtl_RecID, tmp = dtl_Value) %>%
          collect() %>%
          mutate(tmp = as.character(tmp)),
        by = "tmp"
      )
  }

  return(results)
}
