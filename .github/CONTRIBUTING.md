You may expedite bug fixes and new features by doing the work by yourself.
For very simple changes such as fixing typos, you can just edit the file by
clicking the button `Edit` after you open it here on Github. For more
complicated changes, you will have to manually create a [pull
request](https://help.github.com/articles/using-pull-requests) (PR) after
[forking](https://help.github.com/articles/fork-a-repo) this repository. In the
latter case, you are recommended to create a new branch instead of always
working on the `master` branch.

If you submit a non-trivial pull request (e.g. not just fixing a typo), please add an item to `NEWS.md`, and
add your name to the `Authors@R` field as a contributor (`ctb`) in the R package `DESCRIPTION` file.

## Testing

To make sure you did not break anything, you need to run tests, which are
done through the [**testit**](http://cran.rstudio.com/package=testit)
package. If you added any features, add your own tests in `tests/testit/`.

## Travis CI

If you are lazy or do not understand what I said above, just push commits to
your repo, submit a PR, and wait for a few minutes.
[Travis CI](http://yihui.name/en/2013/04/travis-ci-general-purpose/) will run
the tests automatically. If your pull request passes the tests, you see
green check marks, otherwise you see red crosses, and you will probably get
email notifications as well.

- [An example PR that passed the
  check](https://github.com/yihui/knitr/pull/852/commits)
- [An example PR that failed](https://github.com/yihui/knitr/pull/832/commits)

When your PR does not pass the check, you need to click on the red cross to see
details. There are two sets of tests: `R CMD check` on the package source, as
well as the test on examples. The former is basically the unit tests, and the
latter is to make sure your changes does not break my existing examples in the
[knitr-examples](https://github.com/yihui/knitr-examples).

## roxygen2 documentation

If your changes involve the roxygen2 documentation, please run `Rd2roxygen::rab('.')` to generate the Rd files.

## Misc

For a non-trivial fix or feature implementation, please let me know before you
start working on it, since I do not want to waste too much of your time on
something that I may not really want.

Thanks, and happy hacking.
