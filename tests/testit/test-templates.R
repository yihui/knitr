library(testit)

template = 'The value of a is {{a}}, so a + 1 is {{a + 1}}'
exp = knit_expand(text = template, a = 10)
act = 'The value of a is 10, so a + 1 is 11'
assert('templates use data specified', exp == act)

template = 'The value of a is <% a %>, so a + 1 is <% a + 1 %>'
exp = knit_expand(text = template, a = 10, delim = c("<%", "%>"))
act = 'The value of a is 10, so a + 1 is 11'
assert('templates respect custom delimiter pairs', exp == act)

template = 'hello $(LETTERS[24]) and $(pi)!'
exp = knit_expand(text = template, delim = c("$(", ")"))
act = "hello X and 3.14159265358979!"
assert('templates respect pypi delimiters', exp == act)

template = 'The value of a is <% a %>, so a + 1 is <% a + 1 %>'
assert(
  'error is thrown when delimiter is not a pair',
  has_error(knit_expand(text = template, a = 10, delim = '<%'))
)

template = 'The value of a is {{a}}, and b + 1 is {{b + 1}}'
b = -1.21
exp = knit_expand(text = template, a = 10)
act = "The value of a is 10, and b + 1 is -0.21"
assert('templates use data from parent frame', exp == act)
