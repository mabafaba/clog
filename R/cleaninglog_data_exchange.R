#' add cleaninglog elements as columns to a datset
#' @param data the dataset to add cleaninglog to as columns
#' @param cl cleaninglog
#' @return the original `data` with one additional column per unique value cleaninglog `name`. The `name` becomes the column name. For each row in the data, the new column is `TRUE` if a cleaninglog element with that name was listed for the respective row
#' @export
clog_cleaninglog_to_data<-function(data,cleaninglog){
cl<-cleaninglog
  cl_by_column <- cl %>% split.data.frame(cl$name)

  id_in_data<-function(cl){
    data[[attributes(cl)$data_id_column_name]] %in% cl$ids
  }

  issue_cols<-purrr::map(cl_by_column,id_in_data) %>% (tibble::as_tibble)
  original_attributes<-attributes(data)
  data<-c(data,issue_cols) %>% (tibble::as_tibble)
  attributes(data)$changelog<-original_attributes$changelog
  attributes(data)$class<-original_attributes$class
  data
}

#' add cleaninglog elements as columns to a datset
#' @param data the dataset to select variabes from that should be added to the cleaning log
#' @param cleaninglog the cleaninglog (see `cleaninglog()` )
#' @param ... names of the variables to be added from the dataset to the cleaninglog (without quotes!)
#' @example clog_data_to_cleaninglog(mydata,my_cleaninglog, a_data_variable_name, another_data_variable_name, `a data variable name with spaces`)
#' @return the original `data` with one additional column per unique value cleaninglog `name`. The `name` becomes the column name. For each row in the data, the new column is `TRUE` if a cleaninglog element with that name was listed for the respective row
#' @details returning `NA`` if the id in the cleaning log can not be found in the data
#' @export
clog_data_to_cleaninglog<-function(data,cleaninglog,...){
  vars_to_add <- rlang::enquos(...)

  keep_cols <- data %>% dplyr::select(!!!vars_to_add,attributes(cleaninglog)$data_id_column)
  by_cols = attributes(cleaninglog)$data_id_column
  names(by_cols) = "ids"
  new_cl<-dplyr::left_join(cleaninglog,keep_cols,by=by_cols)
  new_cl
}



