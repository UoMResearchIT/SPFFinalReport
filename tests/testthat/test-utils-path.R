# SPDX-FileCopyrightText: [2024] University of Manchester
# SPDX-License-Identifier: apache-2.0

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

  skip_on_os("windows")

  linux_path <- "/path/to somewhere/else"
  components <- get_path_components(linux_path)

  expect_equal(path_from_components(components), linux_path)
})

test_that("path_from_components works for windows root paths", {

  skip_on_os(c("mac", "linux", "solaris"))

  windows_path <- "D:\\Users\\user"
  components <- get_path_components(windows_path)

  expect_equal(path_from_components(components), windows_path)
})
