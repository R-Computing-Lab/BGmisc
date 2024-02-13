

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

# #test_that("makeTwins - Twins specified by generation", {
# #    set.seed(1234)
# #    ped <- data.frame(fam = c(1, 1, 2, 2),
# #                                        ID = c(1, 2, 3, 4),
#                                         gen = c(1, 1, 2, 2),
# #                                        dadID = c(NA, NA, 1, 1),
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
#                                                                 MZtwin = c(NA, NA, 4, 3))
#
#    # note: that right now it will make 4 and 2 twins, but it should be 3 and 4.
#
#     result <- makeTwins(ped, gen_twin = 2)
#     expect_equal(result, expected_result)
# })

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
