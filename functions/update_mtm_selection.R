update_mtm_selection <- function(mtm_matrix, 
  from, to, selection){
  temp_mat <- mtm_matrix
  from_ind <- which(colnames(mtm_matrix) == from)
  to_ind <- which(rownames(mtm_matrix) == to)
  
  temp_mat[from_ind, to_ind] <- selection
  return(temp_mat)
  
}