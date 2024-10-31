test_that("escape_txt works as expected", {
  expect_equal(escape_txt("."), "\\.")
  expect_equal(escape_txt("ab[c]"), "ab\\[c\\]")
  expect_equal(escape_txt("$30"), "\\$30")
})

test_that("get_path_components works for unix-style path separators", {

  linux_path <- "/path/spec"
  expect_equal( get_path_components(linux_path), c("", "path", "spec"))

  linux_path <- "a/path/spec"
  expect_equal(get_path_components(linux_path), c("a", "path", "spec"))

  linux_path <- "another path/spec"
  expect_equal(get_path_components(linux_path), c("another path", "spec"))

  linux_path <- "/path/to somewhere/else"
  expect_equal(get_path_components(linux_path),
               c("", "path", "to somewhere", "else"))
})

test_that("get_path_components works for windows-style path separators", {

  windows_path <- "C:\\Path\\to somewhere\\else"
  expect_equal(get_path_components(windows_path),
               c("C:", "Path", "to somewhere", "else"))

  windows_path <- "Path\\to somewhere\\else"
  expect_equal(get_path_components(windows_path),
               c("Path", "to somewhere", "else"))
})

test_that("get_path_components works for unix-style root paths", {
  expect_equal(get_path_components("/"), c(""))
})

test_that("get_path_components works for windows-style root paths", {
  expect_equal(get_path_components("D:"), c("D:"))
})

test_that("get_path_components works for windows UNC paths", {
  # Universal Naming Convention path, e .g. '\\theserver\share'
  expect_equal(get_path_components("\\\\theserver\\share"),
               c("\\\\", "theserver", "share"))
})

test_that("path_from_components works for unix-style root paths", {
  linux_path <- "/path/to somewhere/else"
  components <- get_path_components(linux_path)

  # TODO: Run this test only on linux, i.e. in a platform-dependent way (??)
  expect_equal(path_from_components(components), linux_path)
})
