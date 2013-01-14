context('Templates')

test_that('templates use data specified', {
  template = 'The value of a is {{a}}, so a + 1 is {{a + 1}}'
  exp = knit_expand(text = template, a = 10)
  act = 'The value of a is 10, so a + 1 is 11'
  expect_equal(exp, act)
})

test_that('templates respect custom delimiter pairs', {
  template = 'The value of a is <% a %>, so a + 1 is <% a + 1 %>'
  exp = knit_expand(text = template, a = 10, delim = c("<%", "%>"))
  act = 'The value of a is 10, so a + 1 is 11'
  expect_equal(exp, act)
})

test_that('templates respect pypi delimiters', {
  template = 'hello $(LETTERS[24]) and $(pi)!'
  exp = knit_expand(text = template, delim = c("$(", ")"))
  act = "hello X and 3.14159265358979!"
  expect_equal(exp, act)
})

test_that('error is thrown when delimiter is not a pair', {
  template = 'The value of a is <% a %>, so a + 1 is <% a + 1 %>'
  expect_error(knit_expand(text = template, a = 10, delim = c("<%")))
})

test_that('templates use data from parent frame', {
  template = 'The value of a is {{a}}, and b + 1 is {{b + 1}}'
  b = -1.21
  exp = knit_expand(text = template, a = 10)
  act = "The value of a is 10, and b + 1 is -0.21"
  expect_equal(exp, act)
})
