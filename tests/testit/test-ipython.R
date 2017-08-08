library(testit)

if(Sys.which("jupyter")!="")
{
  p = knitr:::ipython(debug=T, message=F)

  r = p$exec("a=17")
  assert(identical(r, character()))

  r = p$exec("a+17")
  assert(identical(r, c("```", "34", "```")))

  r = p$exec("a+17", options = list(results="asis"))
  assert(identical(r, "34"))

  # test matplotlib
  r = p$exec("import matplotlib.pyplot")
  if(is.null(attr(r, "status")))
  {
    p$exec("import matplotlib.pyplot as plt")
    r = p$exec("plt.plot([1,2])", options=list(fig.path="fig_path/", label="fig_label"))

    assert(is.null(attr(r, "status")))
    assert(length(r) == 1)
    assert(grepl("^\\!\\[.*\\]\\(fig_path/fig_label_1.png\\)$", r))
    assert(file.exists("fig_path/fig_label_1.png"))
    unlink("fig_path", recursive = TRUE)
  }

  # test error catching
  options = list(code = "not correct", eval=TRUE, error=FALSE, results = "asis", echo=FALSE)
  assert(has_error(r = eng_ipython(options)))

  if(Sys.which("py.test")!="")
  {
    out <- system2("py.test", system.file("ipython", "test_ipython_exec.py", package="knitr"), stdout = TRUE)
    if(!is.null(attr(out, "status")))
      warning(paste(out, collapse="\n"))
    assert(is.null(attr(out, "status")))
  }
}

