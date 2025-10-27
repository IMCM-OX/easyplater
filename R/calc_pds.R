# calc_pds <- function(plate_df, columns_for_scoring, column_weights, mask,
#                      plate_n_rows, plate_n_cols,
#                      internal_control_well_indices,
#                      pds_local_weight=1, patch_weight=NULL){
#
#
#
#   pds_global <- calc_pds_global(plate_df, columns_for_scoring, column_weights,
#                                 mask, plate_n_rows, plate_n_cols,
#                                 internal_control_well_indices)
#
#   pds_local <- calc_pds_local(plate_df, columns_for_scoring, column_weights, plate_n_rows, plate_n_cols, patch_weight)
#
#   return(pds_global + (pds_local_weight*pds_local))
# }
#
# calc_pds_local <- function(plate_df, columns_for_scoring, column_weights,
#                            plate_n_rows, plate_n_cols,
#                            patch_weight=NULL){
#
#   row_column_score <- calc_row_column_score(plate_df, columns_for_scoring, column_weights, plate_n_rows, plate_n_cols)
#   patch_score <- calc_patch_score(plate_df, columns_for_scoring, column_weights, plate_n_rows, plate_n_cols, patch_weight)
#
#   return(row_column_score + patch_score)
# }
#
# calc_patch_score <- function(plate_df, columns_for_scoring, column_weights, plate_n_rows, plate_n_cols, patch_weight=NULL)
# {
#   # setup *******************************************************************
#   sub_plate_n_cols <- plate_n_cols - 2
#   sub_plate_n_rows <- plate_n_rows - 2
#   sub_plate_size <- sub_plate_n_cols * sub_plate_n_rows
#
#   if(is.null(patch_weight)){
#     patch_weight <- min(c(1, (plate_n_rows + plate_n_cols)/(2*sub_plate_size)))
#     #Multiplying denominator by 2 so that patch penalties are down-weighted to match the importance of row and column penalties separately,
#     #rather than being as important as the sum of them (which would make the penalty patch twice as important as either the row or column penalty - I think!)
#   }
#
#   x <- matrix(1:(plate_n_rows * plate_n_cols), plate_n_rows, plate_n_cols)
#   x[,1] <- 0
#   x[1,] <- 0
#   x[,plate_n_cols] <- 0
#   x[plate_n_rows,] <- 0
#
#   y <- x[which(x>0)]
#
#   n <- plate_n_rows # this assignment just makes the next line of code neater...
#   mask <- c( (-n-1), (-n), (-n+1), -1, 0 , 1, (n-1), (n), (n+1) )
#   # *************************************************************************
#
#   score <- 0
#   cwi <- 1
#   for(cs in columns_for_scoring){
#
#     column_data <- unlist(as.list(select(plate_df, cs)))
#     column_weight <- column_weights[cwi]
#     column_as_plate <- matrix(column_data, nrow=plate_n_rows, ncol=plate_n_cols)
#
#
#     column_as_patches <- matrix(unlist(lapply(y, function(a) lapply(mask, function(b) column_as_plate[b + a]))),9,sub_plate_size)
#     column_penalty <- sum(apply(column_as_patches, 2, function(c) length(unique(c[which(!is.na(c))]))==1 & length(which(!is.na(c)))>(0.7 * 9)  ))
#     column_score <- patch_weight * (column_weight * ( sub_plate_size - column_penalty ))
#
#     score <- score + column_score
#     cwi <- cwi + 1
#   }
#
#
#   return(score)
# }
#
# calc_row_column_score <- function(plate_df, columns_for_scoring, column_weights, plate_n_rows, plate_n_cols)
# {
#
#   score <- 0
#   cwi <- 1
#   for(cs in columns_for_scoring){
#
#     column_data <- unlist(as.list(select(plate_df, cs)))
#     column_weight <- column_weights[cwi]
#     column_as_plate <- matrix(column_data, nrow=plate_n_rows, ncol=plate_n_cols)
#
#
#     column_penalty <- sum(apply(column_as_plate,1,function(r) length(unique(r[which(!is.na(r))]))==1 & length(which(!is.na(r)))>(0.7 * plate_n_cols)  )) +
#       sum(apply(column_as_plate,2,function(c) length(unique(c[which(!is.na(c))]))==1 & length(which(!is.na(c)))>(0.7 * plate_n_rows)  ))
#
#     column_score <- column_weight* ((plate_n_rows + plate_n_cols) - column_penalty)
#
#     score <- score + column_score
#     cwi <- cwi + 1
#   }
#
#   return(score)
# }

#' Calculate global plate design score
#'
#' @description
#' `calc_pds_global()` calculates \eqn{PDS_{v_x}}, the sub-score of \eqn{PDS_v} where \eqn{v} is a clinical metadata variable and \eqn{x} is a unique value of \eqn{v}, accounting for the randomization of value \eqn{x} of \eqn{v} across a plate design.
#'
#' @param plate_df Data frame of samples and associated clinical metadata variables.
#' @param columns_for_scoring Character vector of column names to use for scoring.
#' @param column_weights Numeric vector of weights to use for the variables in `columns_for_scoring`. Must be same length as `columns_for_scoring`.
#' @param internal_control_well_indices Numeric vector containing indices of control wells. Expecting zero index, and numbering going first top to bottom, then left to right.
#'
#' @returns A numeric scalar.
#' @export
#'
#' @examples
#' # An example of a preprocessed plate dataframe
#' example_plate_df
#'
#' cols_for_scoring <- names(example_plate_df)[2:5]
#' col_weights <- c(5, 5, 10, 4)
#' ic_well_idcs <- c(86:95)
#' calc_pds_global(example_plate_df, cols_for_scoring, col_weights, ic_well_idcs)
calc_pds_global <- function(plate_df,
                            columns_for_scoring,
                            column_weights,
                            internal_control_well_indices)
{
  # This bit of code assumes that internal controls are pre-placed in the last column.
  # Later, will add an if statement so that this only runs when internal controls are in a fixed position...
  # Note that this will remove one assumption, but the assumption that fixed internal controls are in right-most column(s)
  # of plate will remain... The upshot is that in situations when this is not the case the score is an approximation and,
  # moreover, we need to bound the score between 0 and 1 for the cases where the approximation is outside of this boundary.
  # One last, albeit less important, assumption is that we will always think of the smaller dimension of a plate as being its rows, and the
  # larger dimension will be its columns... This just aligns with the usual "8x12" implied layout of 96 well plates, which
  # (for the moment) is a primary assumption of this library.

  if (nrow(plate_df) == 96) {
    plate_n_rows <- 8
    plate_n_cols <- 12
  } else {
    stop("nrow(plate_df) != 96: calc_pds_global() is currently only implemented for 96-well plates")
  }

  mask <- nrow(plate_df) |>
    make_well_distances_matrix(plate_n_rows) |>
    make_scoring_mask()

  num_samples <- (plate_n_rows * plate_n_cols) - length(internal_control_well_indices)
  min_dim <- min(c(plate_n_rows, plate_n_cols))
  max_dim_floor <- num_samples %/% min_dim

  score <- 0
  cwi <- 1
  for(cs in columns_for_scoring){
    column_data <- plate_df[[cs]]
    column_values_aux <- stats::na.omit(unique(column_data))
    column_values <- c()
    for(cv in column_values_aux){
      if(length(which(column_data==cv)) > 1){
        column_values <- c(column_values, cv)
      }
    }
    column_weight <- column_weights[cwi]

    sub_sub_score <- lapply(column_values, function(val) {
      max(
        min(
          (
            (
              sum(gdata::lowerTriangle(mask[which(column_data==val), which(column_data==val)])) -
                calc_sas_edge_min(sum(stats::na.omit(column_data==val)), min_dim, max_dim_floor)
             )/
              calc_sas_edge_range(sum(stats::na.omit(column_data==val)), min_dim, max_dim_floor)
          ),
          1),
        0)
    }) |>
      unlist()

    sub_score <- column_weight * stats::median(sub_sub_score)

    score <- sum(c(score, sub_score), na.rm=TRUE)

    cwi <- cwi + 1
  }
  return(score)
}


#' @export
#' @rdname calc_sas_edge_min
calc_sas_edge_range <- function(n, nrow, ncol){
  edge_max <- calc_sas_edge_max(n, nrow, ncol)
  edge_min <- calc_sas_edge_min(n, nrow, ncol)
  return(edge_max - edge_min)
}

#' @export
#' @rdname calc_sas_edge_min
calc_sas_edge_max <- function(n, nrow, ncol){
  edge_max_temp <- (n*(n-1))/2
  row_penalty <- ((n %/% nrow) * (n %% nrow)) + (nrow * (( ((n %/% nrow) - 1) * (n %/% nrow) )/2) )
  col_penalty <- ((n %/% ncol) * (n %% ncol)) + (ncol * (( ((n %/% ncol) - 1) * (n %/% ncol) )/2) )

  edge_max <- edge_max_temp - row_penalty - col_penalty
  return(edge_max)
}

#' Samples that could be assigned to row and column exclusive wells.
#'
#' @description
#' `calc_sas_edge_min()` returns the minimum samples that could be assigned to row and column exclusive wells.
#'
#' `calc_sas_edge_max()` returns the maximum samples that could be assigned to row and column exclusive wells.
#'
#' `calc_sas_edge_range()` returns the range of samples that could be assigned to row and column exclusive wells.
#'
#' @param n Integer. Number of samples from the focal group.
#' @param nrow Numeric. Number of rows available plate.
#' @param ncol Numeric. Number of columns available on plate.
#'
#' @returns A numeric scalar.
#' @export
#'
#' @examples
#' # Minimum samples that could be assigned to row and column exclusive wells
#' calc_sas_edge_min(33L, 10, 8)
#'
#' # Maximum samples that could be assigned to row and column exclusive wells
#' calc_sas_edge_max(33L, 10, 8)
#'
#' # Range of samples that could be assigned to row and column exclusive wells
#' calc_sas_edge_range(33L, 10, 8)
calc_sas_edge_min <- function(n, nrow, ncol){
  max_dim <- max(c(nrow,ncol))

  min_dim <- n %/% max_dim
  edge_min_temp <- ((min_dim * max_dim) * ((min_dim-1)*(max_dim-1)))/2

  remainder <- n %% max_dim
  remainder_edges <- remainder * (min_dim * (max_dim - 1))

  edge_min <- edge_min_temp + remainder_edges
  return(edge_min)
}
