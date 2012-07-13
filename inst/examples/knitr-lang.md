# Test of other languages in knitr

## Languages

R works, of course.


```r
set.seed(123)
rnorm(5)
```

```
## [1] -0.56048 -0.23018  1.55871  0.07051  0.12929
```


Can **knitr** deal with other languages? You should have them installed.

### Python


```python
x = 'hello, python world!'
print x
print x.split(' ')
```

```
hello, python world!
['hello,', 'python', 'world!']
```


### Awk

We need to pass a `file` option to Awk.


```awk
# how many non-empty lines?
NF {
  i = i + 1
}
END { print i }
```

```
600
```


### Ruby


```ruby
x = 'hello, ruby world!'
p x.split(' ')
```

```
["hello,", "ruby", "world!"]
```


### Haskell


```haskell
[x | x <- [1..10], odd x]
```

```
[1,3,5,7,9]
```


## Chunk Options

You can use some chunk options like `eval`, `echo` and `results`. For example, `eval=FALSE` (do not evaluate code):


```python
x = 'hello, python world!'
print x
print x.split(' ')
```


or `echo=FALSE` (hide source code):


```
hello, python world!
['hello,', 'python', 'world!']
```


or `results='hide'`:


```python
x = 'hello, python world!'
print x
print x.split(' ')
```


or `results='asis'`:


```python
print '**Write** _something_ in `Markdown` from `Python`!'
```


**Write** _something_ in `Markdown` from `Python`!


## Strict Markdown

You can use strict markdown (i.e. indent by 4 spaces) by setting `render_markdown(TRUE)`.


    render_markdown(TRUE)


Now see how the output is changed:


    x = 'hello, python world!'
    print x
    print x.split(' ')

    hello, python world!
    ['hello,', 'python', 'world!']

