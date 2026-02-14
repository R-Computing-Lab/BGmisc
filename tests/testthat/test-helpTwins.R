test_that("fuse twins behaves", {
  # Simple pedigree: two parents and two MZ twin children
  ped1 <- potter
  ped1$ID <- ped1$personID
  ped2 <- ped1
  ped2$famID <- 2

  ped2$ID <- ped2$personID + 100
  ped2$momID <- ped2$momID + 100
  ped2$dadID <- ped2$dadID + 100
  ped2$twinID <-  ped2$twinID + 100

  ped <- rbind(ped1, ped2)
  remove(ped2)
  remove(ped1)
  #  returnRows = TRUE,
  # returnIDs = FALSE,
  # returnAsList = TRUE

  returnedRowsList <-  findMZtwins(ped, returnRows = TRUE, returnIDs = FALSE, returnAsList = TRUE)
  returnIDsList <-  findMZtwins(ped, returnRows = FALSE, returnIDs = TRUE, returnAsList = TRUE)
  returnedBothList <-  findMZtwins(ped, returnRows = T, returnIDs = T, returnAsList = TRUE)

  # no error should be thrown when running fuseTwins with any of the above outputs as arguments
  expect_no_error(
    fuseTwins(ped,
      test_df_twins = TRUE,
      mz_id_pairs = NULL,
      mz_row_pairs = NULL))

  expect_no_error(
    fuseTwins(ped,
      test_df_twins = TRUE,
      mz_id_pairs = NULL,
      mz_row_pairs = returnedRowsList)
  )
  expect_no_error(
    fuseTwins(ped,
      test_df_twins = TRUE,
      mz_id_pairs = returnIDsList,
      mz_row_pairs = NULL)
  )
  expect_no_error(
    fuseTwins(ped,
      test_df_twins = TRUE,
      mz_id_pairs = returnedBothList$pair_ids,
      mz_row_pairs = returnedBothList$pair_rows)
  )


  df_null   <- tryCatch(fuseTwins(ped,
    test_df_twins = TRUE,
    mz_id_pairs = NULL,
    mz_row_pairs = NULL),
  error = function(e) e)




  df_returnedRows   <-  tryCatch(fuseTwins(ped,
    test_df_twins = TRUE,
    mz_id_pairs = NULL,
    mz_row_pairs = returnedRowsList),
  error = function(e) e)



  df_returnIDs   <-  tryCatch(fuseTwins(ped,
    test_df_twins = TRUE,
    mz_id_pairs = returnIDsList,
    mz_row_pairs = NULL),
  error = function(e) e)


  df_returnedBoth   <-  tryCatch(fuseTwins(ped,
    test_df_twins = TRUE,
    mz_id_pairs = returnedBothList$pair_ids,
    mz_row_pairs = returnedBothList$pair_rows),
  error = function(e) e)



  expect_equal(df_returnedRows, df_returnIDs)
  expect_equal(df_returnedRows, df_returnedBoth)
  expect_equal(df_returnedRows, df_null)
  expect_equal(nrow(df_returnedRows), 2) # One pair of twins should returned
})
