# readWikifamilytree


test_that("traceTreePaths works correctly for horizontal tree", {
  # Create a mock tree_horizontal data frame
  # This is a simplified version of the tree_horizontal data frame
  # with Row, Column, Value, and id columns
  # The id column is used to identify the nodes in the tree

  tree_horizontal <- data.frame(
    Row = rep(1, 3),
    Column = 1:3,
    Value = c("A", "+", "B"),
    stringsAsFactors = FALSE
  )
  tree_horizontal$id <- NA
  tree_horizontal$id[tree_horizontal$Value %in% c("A", "B")] <- tree_horizontal$Value[tree_horizontal$Value %in% c("A", "B")]

  result <- traceTreePaths(tree_horizontal)
  # Check the result
  expect_equal(result$path[1], "A")
  expect_equal(result$path[2], "B")
  expect_equal(result$path[3], NA)
})


test_that("traceTreePaths works correctly for vertical tree", {
  # Create a mock tree_vertical data frame
  # This is a simplified version of the tree_vertical data frame
  # with Row, Column, Value, and id columns
  # The id column is used to identify the nodes in the tree
  tree_spouse_child <- data.frame(
    Row = c(1, 1, 1, 2, 3, 4, 5),
    Column = c(1, 2, 3, 2, 2, 2, 2),
    Value = c("A", "+", "B", "|", "y", "|", "C"),
    stringsAsFactors = FALSE
  )
  tree_spouse_child$id <- NA
  tree_spouse_child$id[tree_spouse_child$Value %in% c("A", "B", "C")] <- tree_spouse_child$Value[tree_spouse_child$Value %in% c("A", "B", "C")]

  result <- traceTreePaths(tree_spouse_child)

  expect_equal(result$path[1], "A")
  expect_equal(result$path[2], "B")
  expect_equal(result$path[3], "C")
  expect_equal(result$path[4], "y")
  expect_equal(result$path[5], NA)
})








test_that("readWikifamilytree reads a string correctly", {
  # Create a temporary WikiFamilyTree file for testing
  # Example usage
  family_tree_text <- "{{familytree/start |summary=I have a brother Joe and a little sister: my mom married my dad, and my dad's parents were Grandma and Grandpa; they had another child, Aunt Daisy.}}
{{familytree | | | | GMa |~|y|~| GPa | | GMa=Gladys|GPa=Sydney}}
{{familytree | | | | | | | |)|-|-|-|.| }}
{{familytree | | | MOM |y| DAD | |DAISY| MOM=Mom|DAD=Dad|DAISY=[[Daisy Duke]]}}
{{familytree | |,|-|-|-|+|-|-|-|.| | | }}
{{familytree | JOE | | ME  | | SIS | | | JOE=My brother Joe|ME='''Me!'''|SIS=My little sister}}
{{familytree/end}}"

  temp_file <- tempfile(fileext = ".txt")
  writeLines(family_tree_text, temp_file)


  result <- readWikifamilytree(text = family_tree_text)
  result2 <- readWikifamilytree(file_path = temp_file)

  expect_equal(
    result$summary,
    "I have a brother Joe and a little sister: my mom married my dad, and my dad's parents were Grandma and Grandpa; they had another child, Aunt Daisy."
  )

  expect_equal(
    result2$summary,
    "I have a brother Joe and a little sister: my mom married my dad, and my dad's parents were Grandma and Grandpa; they had another child, Aunt Daisy."
  )
})


# read E:/Dropbox/Lab/Research/Projects/2024/BGMiscJoss/BGmisc_main/data-raw/Targaryen tree Dance.txt

# test_that("readWikifamilytree reads a file correctly", {
# Create a temporary WikiFamilyTree file for testing
# Example usage
#  family_tree_file_path <- "data-raw/Targaryen tree Dance.txt" # system.file("extdata", "Targaryen tree Dance.txt", package = "BGmisc")

#  result <- readWikifamilytree(file_path=family_tree_file_path)
# })
