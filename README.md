# cytofclean

OK, so this is my first attempt at a package!

VERY early days with this, so tread cautiously!

It should NOT modify any of your fcs files - it should create new ones....

<img src="https://raw.githubusercontent.com/JimboMahoney/cytofclean/master/plots_15_28_57.png"
  align="center" />

<b>Install:</b>

1 - Ensure devtools is installed:

```
if(!require(devtools)){
  install.packages("devtools") # If not already installed
}
```
2 - Install cytofclean package:
```
if(!require(cytofclean)){
  devtools::install_github("JimboMahoney/cytofclean", dependencies = TRUE)
}
```

3 - Load package:
```
library("cytofclean")
```
4 - Run!
```
cytofclean_GUI()
```
