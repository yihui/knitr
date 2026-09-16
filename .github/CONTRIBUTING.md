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

## roxygen2 documentation

If your changes involve the roxygen2 documentation, please run `Rd2roxygen::rab('.')` to generate the Rd files.

## Misc

For a non-trivial fix or feature implementation, please let me know before you
start working on it, since I do not want to waste too much of your time on
something that I may not really want.

Thanks, and happy hacking.
