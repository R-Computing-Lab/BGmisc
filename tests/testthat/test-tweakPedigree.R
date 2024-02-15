

# Test for makeTwins function
test_that("makeTwins - Twins specified by IDs", {
        set.seed(1234)
    ped <- data.frame(fam = c(1, 1, 2, 2),
                                        ID = c(1, 2, 3, 4),
                                        gen = c(1, 1, 2, 2),
                                        dadID = c(NA, NA, 1, 1),
                                        momID = c(NA, NA, 2, 2),
                                        spt = c(NA, NA, NA, NA),
                                        sex = c("M", "F", "M", "F"))
    expected_result <- data.frame(fam = c(1, 1, 2, 2),
                                                                ID = c(1, 2, 3, 4),
                                                                gen = c(1, 1, 2, 2),
                                                                dadID = c(NA, NA, 1, 1),
                                                                momID = c(NA, NA, 2, 2),
                                                                spt = c(NA, NA, NA, NA),
                                                                sex = c("M", "F", "M", "F"),
                                                                MZtwin = c(2, 1, NA, NA))
    result <- makeTwins(ped, ID_twin1 = 1, ID_twin2 = 2)
    expect_equal(result, expected_result)
})

test_that("makeTwins - Twins specified by generation", {
  set.seed(15)
  Ngen <- 4
  kpc <- 4
  sexR <- .50
  marR <- .7
  gen_twin = 2
  ped <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR)
#
    result <- makeTwins(ped, gen_twin = gen_twin)
     expect_equal(names(result), c("fam","ID","gen","dadID","momID","spt","sex","MZtwin"))
     # did it make one pair of twins?
     expect_equal(sum(!is.na(result$MZtwin)), 2)
     # did it make the pair in the correct generation?
     expect_equal(mean(result$gen[!is.na(result$MZtwin)]), gen_twin)
     # are they the same sex?
     expect_equal(length(unique(result$sex[!is.na(result$MZtwin)])), 1)
     # are they from the same family?
     expect_equal(length(unique(result$fam[!is.na(result$MZtwin)])), 1)
     # do they have the same mom?
     expect_equal(length(unique(result$momID[!is.na(result$MZtwin)])), 1)
     # do they have the same dad?
     expect_equal(length(unique(result$dadID[!is.na(result$MZtwin)])), 1)
 })

# Test for makeInbreeding function
test_that("makeInbreeding - Inbred mates specified by IDs", {
    ped <- data.frame(fam = c(1, 1, 2, 2),
                                        ID = c(1, 2, 3, 4),
                                        gen = c(1, 1, 2, 2),
                                        dadID = c(NA, NA, 1, 1),
                                        momID = c(NA, NA, 2, 2),
                                        spt = c(NA, NA, NA, NA),
                                        sex = c("M", "F", "M", "F"))
    expected_result <- data.frame(fam = c(1, 1, 2, 2),
                                                                ID = c(1, 2, 3, 4),
                                                                gen = c(1, 1, 2, 2),
                                                                dadID = c(NA, NA, 1, 1),
                                                                momID = c(NA, NA, 2, 2),
                                                                spt = c(2, 1, NA, NA),
                                                                sex = c("M", "F", "M", "F"))
    result <- makeInbreeding(ped, ID_mate1 = 1, ID_mate2 = 2)
    expect_equal(result, expected_result)
})

# test_that("makeInbreeding - Inbred mates specified by generation", {
#     ped <- data.frame(fam = c(1, 1, 2, 2),
#                                         ID = c(1, 2, 3, 4),
#                                         gen = c(1, 1, 2, 2),
#                                         dadID = c(NA, NA, 1, 1),
#                                         momID = c(NA, NA, 2, 2),
#                                         spt = c(NA, NA, NA, NA),
#                                         sex = c("M", "F", "M", "F"))
#     expected_result <- data.frame(fam = c(1, 1, 2, 2),
#                                                                 ID = c(1, 2, 3, 4),
#                                                                 gen = c(1, 1, 2, 2),
#                                                                 dadID = c(NA, NA, 1, 1),
#                                                                 momID = c(NA, NA, 2, 2),
#                                                                 spt = c(NA, NA, NA, NA),
#                                                                 sex = c("M", "F", "M", "F"),
#                                                                 inbred_mate = c(2, 1, NA, NA))
#     result <- makeInbreeding(ped, gen_inbred = 1)
#     expect_equal(result, expected_result)
# })
