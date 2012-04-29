# Test of other languages in knitr

R works, of course.



```r
set.seed(123)
rnorm(5)
```



```
## [1] -0.56048 -0.23018  1.55871  0.07051  0.12929
```




Can **knitr** deal with other languages? You should have them installed.

## Python

```python
x = 'hello, python world!'
print x
print x.split(' ')
```
```
hello, python world!
['hello,', 'python', 'world!']
```


## Awk

We need to pass a `file` option to Awk.

```awk
# how many non-empty lines?
NF {
  i = i + 1
}
END { print i }
```
```
592
```

