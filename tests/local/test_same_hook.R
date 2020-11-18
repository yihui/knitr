library(testthat)

gert::git_branch_checkout("master")
pkgload::load_all(export_all = FALSE)

list <- c("default", grep("render_", getNamespaceExports("knitr"), value = T))

test_same_hooks <- function(hooks_fun) {
  file_save <- file.path("tests/local", hooks_fun)
  if (hooks_fun != "default") {
    render_fun <- getFunction(hooks_fun)
    render_fun()
  }
  expect_known_value(knit_hooks$get(), file_save, update = FALSE)
  knit_hooks$restore()
}


# Reference from master ---------------------------------------------------


res <- purrr::map(list, test_same_hooks)


# Test changes ------------------------------------------------------------

gert::git_branch_checkout("export-hooks")
pkgload::load_all(export_all = FALSE)
res <- purrr::map(list, purrr::safely(test_same_hooks))
## if all true, nothing changed
expect_true(all(is.null(res$error)))

# hooks are correctly sets in the render_* and hooks_* functions
test_same_hooks2 <- function(hooks_fun) {
  message("testing ", hooks_fun)
  if (hooks_fun != "default") {
    render_fun <- getFunction(hooks_fun)
    render_fun()
    hooks_fun2 <- gsub("^render_", "hooks_", hooks_fun)
    expect_identical(knit_hooks$get(),
                     knitr:::merge_list(knitr:::.default.hooks,
                                        getFunction(hooks_fun2)()),
                     ignore.environment = TRUE
                     )
    knit_hooks$restore()
  }
}

res <- purrr::map(list, test_same_hooks2)
