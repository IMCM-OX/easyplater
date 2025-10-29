make_well_distances_matrix <- function(plate_size){

  if (plate_size == 96) {
    plate_n_rows <- 8
  } else {
    stop("plate_size != 96: easyplater is currently only implemented for 96-well plates")
  }

  well_distances_matrix <- matrix(0, nrow=plate_size, ncol=plate_size)
  rownames(well_distances_matrix) <- 0:(plate_size-1)
  colnames(well_distances_matrix) <- 0:(plate_size-1)

  for (i in 0:(plate_size-2)){
    for (j in (i+1):(plate_size-1)){
      irow <- i %% plate_n_rows
      icol <- i %/% plate_n_rows
      jrow <- j %% plate_n_rows
      jcol <- j %/% plate_n_rows

      if((irow==jrow) || (icol==jcol)){
        d <- 0
      }else{
        d <- sqrt((irow-jrow)^2 + (icol-jcol)^2)
      }
      well_distances_matrix[as.character(i), as.character(j)] <- d
      well_distances_matrix[as.character(j), as.character(i)] <- d
    }
  }
  return(well_distances_matrix)
}

make_full_mask <- function(well_distances_matrix, mask_edge_thresh=3){
  if(mask_edge_thresh < 3){
    stop("mask_edge_thresh in make_full_mask cannot be < 3.")
  }

  full_mask <- well_distances_matrix
  full_mask[which(well_distances_matrix<=mask_edge_thresh)] <- 0
  full_mask[which(well_distances_matrix>mask_edge_thresh)] <- 1

  return(full_mask)
}

make_scoring_mask <- function(well_distances_matrix, scoring_mask_edge_thresh=1){

  scoring_mask <- well_distances_matrix
  scoring_mask[which(well_distances_matrix<=scoring_mask_edge_thresh)] <- 0
  scoring_mask[which(well_distances_matrix>scoring_mask_edge_thresh)] <- 1

  return(scoring_mask)
}
