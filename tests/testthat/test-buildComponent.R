test_that(".assignParentValue works", {

  expect_equal(.assignParentValue("generation"), .5)
  expect_equal(.assignParentValue("additive"), .5)
  expect_equal(.assignParentValue("common nuclear"), 1)
  expect_equal(.assignParentValue("mitochondrial"), 1)
  expect_equal(.assignParentValue("mtdna"), 1)
  expect_equal(.assignParentValue("mitochondria"), 1)

  expect_error(.assignParentValue("unknown component"),
               "Don't know how to set parental value")
})

