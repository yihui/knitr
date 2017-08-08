# run 'py.test'      in the same directory
# or  'py.test -vrw' to display warnings

from ipython_exec import parse_args, kernel_client, execute, format_result
import re

from jupyter_client import KernelManager
km = KernelManager()
km.start_kernel()
km.write_connection_file()
kc = km.client()

# set breakpoints with
# import pytest
# pytest.set_trace()


def test_set_a():
    config = parse_args(['a=17'])
    r = execute(kc, config.code[0])
    err, result = format_result(r, config)

    assert result == ""
    assert not err

def test_get_a():
    config = parse_args(['a'])
    r = execute(kc, config.code[0])
    err, result = format_result(r, config)

    assert result == '17'
    assert not err

def test_get_a_plus_13():
    config = parse_args(['a+13'])
    r = execute(kc, config.code[0])
    err, result = format_result(r, config)

    assert result == '30'
    assert not err

def test_pandas_html():
    config = parse_args(["""import pandas
pandas.DataFrame({'x':range(10)})"""])

    r = execute(kc, config.code[0])
    err, result = format_result(r, config)

    assert re.match("<div.*", result) is not None
    assert not err

def test_pandas_text():
    config = parse_args(["""import pandas
pandas.DataFrame({'x':range(10)})""", "-r", "nohtml"])

    r = execute(kc, config.code[0])
    err, result = format_result(r, config)

    assert re.match("<div>.*", result) is None
    assert not err

def test_matplotlib_and_print():
    config = parse_args(["""
import numpy as np
import matplotlib.pyplot as plt


N = 50
x = np.random.rand(N)
y = np.random.rand(N)
colors = np.random.rand(N)
area = np.pi * (15 * np.random.rand(N))**2 # 0 to 15 point radiuses

print("Hello")
plt.scatter(x, y, s=area, c=colors, alpha=0.5)
plt.show()
print("Again")
plt.scatter(x, 1-y, s=area, c=colors, alpha=0.5)
plt.show()
print("Finally")
""", "--to", "markdown", "--results", "markup"])
    r = execute(kc, config.code[0])
    err, result = format_result(r, config)

    assert re.sub("0x[0-9a-f]*\\>", "0xXXXXXXXXXXX>", result) == \
"""```
Hello
```
![<matplotlib.figure.Figure at 0xXXXXXXXXXXX>](figure/image_1.png)
```
Again
```
![<matplotlib.figure.Figure at 0xXXXXXXXXXXX>](figure/image_2.png)
```
Finally
```"""
    assert not err

def test_matplotlib_and_print_tex():
    config = parse_args(["""
import numpy as np
import matplotlib.pyplot as plt


N = 50
x = np.random.rand(N)
y = np.random.rand(N)
colors = np.random.rand(N)
area = np.pi * (15 * np.random.rand(N))**2 # 0 to 15 point radiuses

print("Hello")
plt.scatter(x, y, s=area, c=colors, alpha=0.5)
plt.show()
print("Again")
plt.scatter(x, 1-y, s=area, c=colors, alpha=0.5)
plt.show()
print("Finally")
""", "--to", "latex", "--results", "markup"])
    r = execute(kc, config.code[0])
    err, result = format_result(r, config)

    assert result == """\\begin{verbatim}
Hello
\\end{verbatim}
\\includegraphics[width=\\linewidth]{figure/image_1}
\\begin{verbatim}
Again
\\end{verbatim}
\\includegraphics[width=\\linewidth]{figure/image_2}
\\begin{verbatim}
Finally
\\end{verbatim}"""
    assert not err
