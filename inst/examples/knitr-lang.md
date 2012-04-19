# Test of other languages in knitr

R works, of course.



```r
rnorm(5)
```



```no-highlight
## [1]  0.1143 -0.6663  1.5533 -0.6486  0.4476
```




Can **knitr** deal with other languages? You should have them installed.

## Python

```python
x = 'hello, python world!'
print x
print x.split(' ')
```
```text
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
```text
586
```

