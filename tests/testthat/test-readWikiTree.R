# readWikifamilytree

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
