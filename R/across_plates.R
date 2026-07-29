## Note that these are meant to be Oxford IMCM internal use, packaged here for
## convenient access, and have not been vetted with the same scrutiny as public-
## facing within-plate randomization code. These are all intended to be remain
## internal functions, accessed via easyplater::: syntax.

# Wrapper function (i.e. run this)
# Example:
# manifest_df <- input_manifest
# manifest_df$SubjectID <- paste0(manifest_df$SampleID, manifest_df$Cohort)
# manifest_df$Group <- factor(manifest_df$Group)
# manifest_df$VarToControl <- manifest_df$Group
# columns_to_test_df <- tibble::tribble(
# ~ test_column,               ~ column_type,            ~ column_weight,
# "Group",                    "discrete_noperm",         1,
# "Sex",                      "discrete_noperm",         0.5,
# "Age",                      "continuous",              0.5
# )
# assign_plates_out <- assign_plates(manifest_df, columns_to_test_df, getwd())

assign_plates <- function(manifest_df,
              columns_to_test_df,
              output_dir,
              plate_output_filename = NULL,
              score_output_filename = NULL,
              plate_size,
              num_ctrl,
              num_plates,
              wells_to_skip,
              assignment_seeds = NULL,
              testing_seeds = NULL,
              min_cell_expected = 5,
              perms = 100,
              perm_p_weighting = 0.2,
              trials = 1e4,
              seed = 1374,
              print_mode = "progress",
              create_dir = TRUE) {

  ## Create output dir
  if (create_dir) {
    dir.create(file.path(output_dir), recursive = TRUE, showWarnings = FALSE)
  }

  # Force any factors to character, as the running table() in `assign_subjects_to_plates()` on factors results in a non-random order
  if(any(lapply(manifest_df, class) == "factor")) {
    fcts <- names(which(lapply(manifest_df, class) == "factor"))
    warning(paste0("Converting the following columns from factor to character: ", paste(fcts, collapse = ", ")))
    manifest_df <- manifest_df |>
      dplyr::mutate(dplyr::across(dplyr::all_of(fcts), as.character))
  }

  ## Make sure we are using the correct assignment seeds and testing seeds... *************************************

  # stop execution if user has supplied one or other of assignment_seeds or testing_seeds; continue if supplied both or neither.
  if(xor(is.null(assignment_seeds), is.null(testing_seeds))){
    stop("You cannot supply only assignment_seeds, or only testing_seeds. You must either supply both or supply neither.")
  }

  # if no seeds have been supplied, generate them.
  if(is.null(assignment_seeds) && is.null(testing_seeds)){
    # withr::with_seed() prevents the user's seed from changing, so each run with
    # the same seed argument will reproduce the same result, without restarting R
    withr::with_seed(seed, {
      # We want to do this randomly, but we need to set seeds and keep track of them so that we can reproduce the outputs.
      # For each full sample-to-plate assignment we need two seeds: One for the assignment phase, and one for the testing phase.
      assignment_seeds <- c(sample(10^7)[seq(1,trials)])
      testing_seeds <- c(sample(10^7)[seq(1,trials)])
    })
  }

  # Make sure user is informed of what is going on.
  if(!(is.null(assignment_seeds)) && !(is.null(testing_seeds))){
    print("WARNING: You have opted to supply assignment_seeds and testing_seeds, so parameters trial and seed will be ignored because they are only used when randomly generating these vectors.")
    trials <- length(assignment_seeds)
  }

  # Checking validity of inputed seeds
  if(length(assignment_seeds)!=length(testing_seeds)){
    stop("Number of assignment seeds does not match number of testing seeds.")
  }

  #*****************************************************************************

  # Now that we know what the seeds are, we know the number of trials, so we can make output file names
  if (is.null(plate_output_filename)) {
    plate_output_filename <- paste0("random_manifest_assignments-", trials, "trials-", lubridate::today(), ".rds")
  }

  if (is.null(score_output_filename)) {
    score_output_filename <- paste0("random_manifest_tests-", trials, "trials-", lubridate::today(), ".csv")
  }

  # Now do the actual work...

  ## STEP 1: Generate random manifests, save to list, write to rds
  out_manifests <- generate_random_manifests(manifest_df, assignment_seeds, plate_size, num_ctrl, num_plates, wells_to_skip)

  #readr::write_rds(out_manifests, file.path(output_dir, plate_output_filename))

  ## STEP 2: Test random manifests
  # out_manifests <- read_rds(file.path(output_dir, plate_output_filename))
  trial_scores_df <- score_random_manifests(out_manifests, columns_to_test_df,
                                           testing_seeds, assignment_seeds,
                                           plate_size, num_ctrl, num_plates,
                                           wells_to_skip, min_cell_expected,
                                           perms, perm_p_weighting,
                                           print_mode = "progress")

  readr::write_csv(trial_scores_df, file.path(output_dir, score_output_filename))

  return(list(manifests = out_manifests, scores = trial_scores_df))
}


# Calculate wells_to_skip
calc_wells_to_skip <- function(plate_size, num_ctrl, num_samples, num_bridging_samples){
  n_full_plates <- (num_samples + num_bridging_samples) %/% (plate_size - num_ctrl)
  n_overhang <- (num_samples + num_bridging_samples) %% (plate_size - num_ctrl)

  wells_to_skip <- c(rep((num_bridging_samples %/% n_full_plates), n_full_plates))
  wells_to_skip <- wells_to_skip + c(rep(1, (num_bridging_samples %% n_full_plates)), rep(0, n_full_plates-(num_bridging_samples %% n_full_plates)))

  if(n_overhang>0){
    wells_to_skip <- c(wells_to_skip, plate_size - num_ctrl -n_overhang)
  }

  return(wells_to_skip)
}


# Generate random manifests
generate_random_manifests <- function(manifest_df, assignment_seeds, plate_size, num_ctrl,
                                      num_plates, wells_to_skip, max_attempts=500) {

  out_manifests <- list()
  cat("\nGenerating random manifests.\n")
  pb <- utils::txtProgressBar(min = 0, max = length(assignment_seeds), style = 3)
  for (trial in 1:length(assignment_seeds)){
    set.seed(assignment_seeds[trial])

    out_manifests[[as.character(assignment_seeds[trial])]] <-
      generate_plate_assignments(manifest_df, num_plates, plate_size, num_ctrl, wells_to_skip, max_attempts, assignment_seed=assignment_seeds[trial])
    utils::setTxtProgressBar(pb, trial)
  }
  return(out_manifests)
}

# Generate scores for each random manifest
score_random_manifests <- function(out_manifests, columns_to_test_df,
                                testing_seeds, assignment_seeds,
                                plate_size, num_ctrl,
                                num_plates, wells_to_skip,
                                min_cell_expected = 5, perms = 100, perm_p_weighting = 0.2,
                                print_mode = "progress") {

  trial_scores_df <- data.frame(matrix(ncol = (3+nrow(columns_to_test_df)+1), nrow = 0))
  colnames(trial_scores_df) <- c("trial", "assignment_seed", "testing_seed", columns_to_test_df$test_column, "total_weighted_score")

  cat("\nTesting and scoring plated manifest.\n")

  if (print_mode == "progress") {
    pb <- utils::txtProgressBar(min = 0, max = length(assignment_seeds), style = 3)
  }

  for (trial in 1:length(testing_seeds)) {
    out_manifest <- out_manifests[trial][[1]]

    # Test this manifest for equal distribution of samples across plates, and calculate a corresponding score:
    set.seed(testing_seeds[trial])

    weighted_scores <- c()
    for (row in 1:nrow(columns_to_test_df)) {
      col2test <- columns_to_test_df[row,]
      test_column <- col2test$test_column
      column_type  <- col2test$column_type
      column_weight  <- col2test$column_weight

      if (print_mode == "verbose") {
        cat(paste0("Trial = ", trial, ", row = ", row, ", column = ", test_column, ", type = ", column_type, ", weight = ", column_weight, "\n"))
      }

      if(column_type == "discrete"){
        chisq_or_perm_result <-
          chisq_or_perm(out_manifest, test_column, num_plates, min_cell_expected,
                        plate_size, num_ctrl, wells_to_skip, perms, noperm = FALSE)
        weighted_score <- get_score_from_test_results(chisq_or_perm_result, column_weight, perm_p_weighting)
      }else if(column_type == "discrete_noperm"){
        chisq_or_perm_result <-
          chisq_or_perm(out_manifest, test_column, num_plates, min_cell_expected,
                        plate_size, num_ctrl, wells_to_skip, perms, noperm = TRUE)
        weighted_score <- get_score_from_test_results(chisq_or_perm_result, column_weight, perm_p_weighting)
      }else if(column_type == "continuous"){
        formula_str <- paste(test_column, "~ plate") |> stats::as.formula()
        oneway_error <- TRUE
        tryCatch(
          {
            equal_variance <- stats::fligner.test(formula_str, data = out_manifest)$p.value >= 0.1
            oneway_result <- stats::oneway.test(formula_str, data = out_manifest, var.equal = FALSE)
            oneway_error <- FALSE
          }, error = function(msg){
            print("Error caught with stats::oneway.test. Setting corresponding score to 0.")
          }, warning = function(msg){
            print("Warning caught with stats::oneway.test. Setting corresponding score to 0.")
          }
        )
        if(oneway_error){
          weighted_score <- 0
        }else{
          weighted_score <- get_score_from_test_results(list('oneway', oneway_result), column_weight, perm_p_weighting)
        }
      }
      if(weighted_score==-1){
        print(paste("ERROR: column scoring has failed for column", test_column, "has failed. Please check input manifest and try again.", sep=" "))
      }else{
        weighted_scores <- c(weighted_scores, weighted_score)
      }
    }
    total_weighted_score <- sum(weighted_scores)/sum(columns_to_test_df$column_weight)

    trial_scores_row <-
      matrix(c(trial, assignment_seeds[trial], testing_seeds[trial], weighted_scores, total_weighted_score),
             dimnames = list(NULL,c("trial", "assignment_seed", "testing_seed", columns_to_test_df$test_column, "total_weighted_score")),
             nrow = 1) |>
      as.data.frame()
    trial_scores_df <- dplyr::bind_rows(trial_scores_df, trial_scores_row)

    if (print_mode == "progress") {
      utils::setTxtProgressBar(pb, trial)
    }
  }

  return(trial_scores_df)
}

get_num_wells_to_use_per_plate <- function(num_samples_to_plate, wells_free_per_plate, num_plates){
  # pre-condition/ assumptions: num_samples_to_plate > sum(wells_free_per_plate (calling function has checked this)
  wells_free_per_plate_temp <- wells_free_per_plate
  num_wells_to_use_per_plate <- rep(0, num_plates)

  while(num_samples_to_plate > 0){
    max_samples_per_plate <- ceiling(num_samples_to_plate/ num_plates)

    num_wells_to_use_per_plate_sub <- unlist(lapply(wells_free_per_plate_temp, function(x) {min(x,max_samples_per_plate)}))

    num_wells_to_use_per_plate <- num_wells_to_use_per_plate + num_wells_to_use_per_plate_sub
    wells_free_per_plate_temp <- wells_free_per_plate_temp - num_wells_to_use_per_plate_sub
    num_samples_to_plate <- num_samples_to_plate - sum(num_wells_to_use_per_plate_sub)
  }
  return(num_wells_to_use_per_plate)
}

subtract_plate_allocations_from_free_wells <- function(wells_free_per_plate, plate_allocations, num_plates){
  return(wells_free_per_plate - unlist(lapply(seq(1,num_plates), function(p){sum(plate_allocations==p)})))
}

assign_subjects_to_plates <- function(manifest, num_plates, plate_size, num_ctrl, wells_to_skip){
  plate_indices <- seq(1,num_plates)           # Note - we don't actually care what the original plate names or numbers are because they do not matter in this permutation process. Also, using numeric indices is neater to code here.

  if (length(wells_to_skip) != num_plates) {
    stop("`wells_to_skip` must be vector of length num_plates")
  }
  wells_free_per_plate <- rep(plate_size - num_ctrl, num_plates) - wells_to_skip

  subject_plate_permutation_df <- data.frame(SubjectID=c(), permplate=c())
  failed = FALSE

  longitudinal_repeats_df <- data.frame(table(manifest$SubjectID))
  num_repeats <- unique(longitudinal_repeats_df$Freq[longitudinal_repeats_df$Freq>1])

  if(length(num_repeats)>0){
    for (nr in sort(num_repeats, decreasing=TRUE)){
      # Count number of longitudinal repeats per Subject
      longitudinal_repeats_sub_df <- data.frame(table(manifest$SubjectID))
      # Pick the Subjects with n=nr repeats
      nr_subset <- longitudinal_repeats_sub_df$Var1[longitudinal_repeats_sub_df$Freq==nr]
      nr_subset_temp <- nr_subset
      nr_subset_plates <- c()
      # Until all runs of same-Subject samples are allocated
      while(length(nr_subset_temp)>0 & !failed){
        # If there are plates with enough wells to fit the run of repeats at hand
        if(any(wells_free_per_plate>=nr)){
          # Get indices of plates with enough wells to fit the run of repeats at hand
          useful_plates <- plate_indices[wells_free_per_plate>=nr]
          num_useful_plates <- length(useful_plates)
          if(num_useful_plates==1){
            # If only one plate has enough wells, just use that one
            subset_useful_plates <- useful_plates[1]
          }else{
            # Otherwise, assign the samples to the (randomly permuted) plates with enough wells
            subset_useful_plates <- sample(useful_plates)[1:min(num_useful_plates, length(nr_subset_temp))]
          }
          # Add the randomly assigned plate indices to a growing vector that will eventually cover all samples sharing this number of repeats for a subject
          nr_subset_plates <- c(nr_subset_plates, subset_useful_plates)

          ## Update loop variables
          # Subtract the used wells from the remaining wells to loop over
          wells_free_per_plate[subset_useful_plates] <- wells_free_per_plate[subset_useful_plates] - nr
          # Remove the resolved Subjects from the vector to loop over
          nr_subset_temp <- nr_subset_temp[-seq(1,length(subset_useful_plates))]

        }else{
          print("PERMUTATION FAILED")
          failed <- TRUE
        }
      }
      if(!failed){
        subject_plate_permutation_df <- rbind(subject_plate_permutation_df, data.frame(SubjectID=nr_subset, permplate=nr_subset_plates))
      }
    }
  }

  if(failed){
    return(-1)
  }else{
    non_longitudinal_samples <- longitudinal_repeats_df$Var1[longitudinal_repeats_df$Freq==1]

    if(length(non_longitudinal_samples) > sum(wells_free_per_plate)){
      print("ERROR DURING PERMUTATION - NOT ENOUGH WELLS AVAILABLE...")
      return(-1)
    }else{
      manifest_non_longitudinal <- manifest[manifest$SubjectID %in% non_longitudinal_samples,]
      manifest_non_longitudinal_subjects <- manifest_non_longitudinal$SubjectID

      num_samples_to_plate <- length(manifest_non_longitudinal_subjects)

      if(num_samples_to_plate > 0){
        num_wells_to_use_per_plate <- get_num_wells_to_use_per_plate(num_samples_to_plate, wells_free_per_plate, num_plates)

        leftover_plate_spaces <- unlist(lapply(plate_indices, function(p){rep(p,num_wells_to_use_per_plate[p])}))
        if(length(leftover_plate_spaces)==1){
          plate_allocations <- leftover_plate_spaces
        }else{
          plate_allocations <- sample(leftover_plate_spaces)[1:num_samples_to_plate]
        }

        subject_plate_permutation_df <- rbind(subject_plate_permutation_df, data.frame(SubjectID=manifest_non_longitudinal_subjects, permplate=plate_allocations))

        wells_free_per_plate <- subtract_plate_allocations_from_free_wells(wells_free_per_plate, plate_allocations, num_plates)
      }
      return(dplyr::inner_join(manifest,subject_plate_permutation_df, by="SubjectID")$permplate)
    }
  }
}

generate_plate_assignments <- function(manifest, num_plates, plate_size, num_ctrl, wells_to_skip, max_attempts, assignment_seed){
  num_attempts <- 0
  success <- FALSE
  # Run plate permutation
  while(!(success) & (num_attempts < max_attempts)){
    plate_permutation <- assign_subjects_to_plates(manifest, num_plates, plate_size, num_ctrl, wells_to_skip)
    if(plate_permutation[1]!=-1){
      success <- TRUE
      my_manifest <- manifest
      my_manifest$plate <- unlist(lapply(plate_permutation, function(x){paste('plate',x)}))
      return(my_manifest)
    }
    num_attempts <- num_attempts + 1
  }

  if(!(success)){
    stop(paste0("Unable to generate plate assignment using assignment seed = ", assignment_seed, " in ", max_attempts, " attempts."))
  }
}

olink_plate_permutation <- function(manifest, num_plates, plate_size, num_ctrl, wells_to_skip, perms){
  my_manifest <- manifest
  num_successful_perms <- 0
  perm_col_names <- c()
  for (perm in seq(1,perms)){
    plate_permutation <- assign_subjects_to_plates(manifest, num_plates, plate_size, num_ctrl, wells_to_skip)
    if((length(plate_permutation) == 1) & plate_permutation[1] == -1){
      print("Warning: a permutation has failed... If this keeps happening, this testing strategy may not be appropriate")
    }else{
      my_manifest[paste('pp', perm, sep="")] <- plate_permutation
      num_successful_perms <- num_successful_perms + 1
      perm_col_names <- c(perm_col_names, paste('pp', perm, sep=""))
    }
  }
  if( (num_successful_perms/perms) < 0.1){
    print("Permutation strategy has failed. (Less than 10% of permutations worked).")
    print(num_successful_perms)
    print(perms)
    return(-1)
  }else{
    return(list(num_successful_perms, my_manifest, perm_col_names))
  }
}

run_permutation_test <- function(manifest, test_column, num_plates, min_cell_expected, lengths, values, plate_size, num_ctrl, wells_to_skip, perms){
  permutation_output <- olink_plate_permutation(manifest, num_plates, plate_size, num_ctrl, wells_to_skip, perms)
  if(length(permutation_output)== 1 & permutation_output[1]==-1){
    print("Unable to run permutation test because not enough permutations have worked.")
    return(-1)
  }else{
    num_perms <- permutation_output[[1]]
    manifest_with_permutations <- permutation_output[[2]]
    perm_col_names <- permutation_output[[3]]

    reduced_manifest_with_permutations_other <- manifest_with_permutations[manifest_with_permutations[[test_column]] %in% values[lengths < (num_plates*min_cell_expected)],]

    real_full_table <- table(manifest_with_permutations$plate, manifest_with_permutations[[test_column]])
    real_other_table <- real_full_table[,which(dimnames(real_full_table)[[2]] %in% values[lengths < (num_plates*min_cell_expected)])]

    peaks <- as.vector(apply(real_other_table, 2, function(x){max(x)}))
    spreads <- as.vector(apply(real_other_table, 2, function(x){sum(x>0)}))

    all_perm_peaks <- matrix(-1,num_perms,sum(lengths < (num_plates*min_cell_expected)))
    all_perm_spreads <- matrix(-1,num_perms,sum(lengths < (num_plates*min_cell_expected)))

    for (perm in seq(1,num_perms)){
      perm_col_name <- perm_col_names[perm]

      perm_full_table <- table(manifest_with_permutations[[perm_col_name]], manifest_with_permutations[[test_column]])
      perm_other_table <- perm_full_table[,which(dimnames(perm_full_table)[[2]] %in% values[lengths < (num_plates*min_cell_expected)])]

      perm_peaks <- as.vector(apply(perm_other_table, 2, function(x){max(x)}))
      perm_spreads <- as.vector(apply(perm_other_table, 2, function(x){sum(x>0)}))

      all_perm_peaks[perm,] <- perm_peaks
      all_perm_spreads[perm,] <- perm_spreads
    }

    num_tests <- sum(lengths < (num_plates*min_cell_expected))

    perm_ps <- unlist(lapply(seq(1,num_tests), function(x){ (sum( (all_perm_spreads[,x] < spreads[x]) |
                                                                    ((all_perm_spreads[,x] == spreads[x]) & (all_perm_peaks[,x] >= peaks[x]) ) ) + 1)/(num_perms+1) }))

    return(perm_ps)
  }
}

chisq_or_perm <- function(manifest, test_column, num_plates, min_cell_expected, plate_size, num_ctrl, wells_to_skip, perms, noperm){
  lengths <-  rle(sort(manifest[[test_column]]))$lengths
  values <- rle(sort(manifest[[test_column]]))$values

  print(paste0("In chisq_or_perm function for column ", test_column))
  print(paste0("(num_plates*min_cell_expected) is ", (num_plates*min_cell_expected)))
  print(paste0("lengths < (num_plates*min_cell_expected) is ", lengths < (num_plates*min_cell_expected)))
  print(paste0("sum(lengths < (num_plates*min_cell_expected)) is ", sum(lengths < (num_plates*min_cell_expected))))
  print("Here are the values and lengths for this column:")
  print(values)
  print(lengths)

  if(noperm | (sum(lengths < (num_plates*min_cell_expected)) == 0)){
    print("Doing standard Chi-square test")
    chisq_result <- stats::chisq.test(table(manifest$plate, manifest[[test_column]]))
    return(list('normal', chisq_result))

  }else{

    print("Doing permutation instead of Chi-square test")

    full_table <- table(manifest$plate, manifest[[test_column]])
    reduced_table <- full_table[,which(dimnames(full_table)[[2]] %in% values[lengths >= (num_plates*min_cell_expected)])]

    trycatch_error = TRUE
    tryCatch(
      {
        chisq_result <- stats::chisq.test(reduced_table)
        trycatch_error = FALSE
      }, error = function(msg){
      }, warning = function(msg){
      }
    )

    if(trycatch_error){
      chisq_result <- list(0)
      names(chisq_result) <- "p.value"
    }

    perm_ps <- run_permutation_test(manifest, test_column, num_plates, min_cell_expected, lengths, values, plate_size, num_ctrl, wells_to_skip, perms)

    if(length(perm_ps)==1 & perm_ps[1]==-1){
      print("Unable to run chisq_or_perm: could not permute plates.")
      return(-1)
    }else{
      perm_ps_weights <- lengths[lengths < (num_plates*min_cell_expected)]
      return(list('super', chisq_result, perm_ps, perm_ps_weights))
    }
  }
}

perm_ps_weighted_average <- function(perm_ps, perm_ps_weights){
  return(sum(perm_ps * perm_ps_weights)/sum(perm_ps_weights))
}

get_score_from_test_results <- function(wrapped_result, column_weight, perm_p_weighting){
  if(wrapped_result[[1]]=="oneway"){

    if(is.na(wrapped_result[[2]]$p.value)){
      return(0)
    }else{
      return((wrapped_result[[2]]$p.value) * column_weight)
    }

  }else if(wrapped_result[[1]]=="normal"){

    return((wrapped_result[[2]]$p.value) * column_weight)

  }else if(wrapped_result[[1]]=="super"){
    main_p <- wrapped_result[[2]]$p.value
    perm_p <- perm_ps_weighted_average(wrapped_result[[3]], wrapped_result[[4]])
    if((perm_p_weighting < 0) | (perm_p_weighting > 1)){
      print("ERROR: Unable to get score from test results... perm_p_weighting is out of range [0,1].")
      return(-1)
    }else{
      return( ((main_p + (perm_p_weighting*perm_p))/(1+perm_p_weighting)) * column_weight)
    }
  }else{
    print("ERROR: Unable to get score from test results... Test result type not recognised.")
    return(-1)
  }
}
