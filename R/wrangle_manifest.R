categorize_cols <- function(plate_df_aux, cols_to_categorize) {
  # cols_to_categorize is list of tuples with structure (col_name,num_cats,na_replacement,categorized_col_name)
  if(length(cols_to_categorize)>0){
    for(col_to_categorize_tuple in cols_to_categorize){

      col_name <- col_to_categorize_tuple[1]
      num_cats <- as.numeric(col_to_categorize_tuple[2])

      cut_interval_has_worked = FALSE
      while((!cut_interval_has_worked) & num_cats > 0){
        tryCatch(
          {
            categorized_col_vec <- as.numeric(cut_interval(as.matrix(plate_df_aux[,col_name]),num_cats))
            cut_interval_has_worked <- TRUE
            #print(paste0("Final num_cats: ", num_cats))

          }, error = function(msg){
          }, warning = function(msg){
          })
        num_cats <- num_cats - 1
      }
      if(!cut_interval_has_worked){
        categorized_col_vec <- plate_df_aux[,col_name]
      }

      if(length(col_to_categorize_tuple)==4){
        na_replacement <- col_to_categorize_tuple[3]
        categorized_col_name <- col_to_categorize_tuple[4]
        replace(categorized_col_vec, is.na(categorized_col_vec), na_replacement)
      }else{
        categorized_col_name <- col_to_categorize_tuple[3]
      }

      plate_df_aux[paste(categorized_col_name)] <- categorized_col_vec
      #print(categorized_col_name)
    }
  }
}
