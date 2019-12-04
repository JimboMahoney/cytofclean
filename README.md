# cytofclean

OK, so this is my first attempt at a package!

VERY early days with this, so tread cautiously!

It should NOT modify any of your fcs files - it should create new ones....

<img src="https://raw.githubusercontent.com/JimboMahoney/cytofclean/master/plots_15_28_57.png"
  align="center" />

<b>Install / Run:</b>

Run the below, which will:

1) Ensure devtools is installed (needed to install packages from github)
2) Download and install the cytofclean package.
3) Load the package.
4) Run the GUI.

```
if(!require(devtools)){
  install.packages("devtools") # If not already installed
}

if(!require(cytofclean)){
  devtools::install_github("JimboMahoney/cytofclean", dependencies = TRUE)
}

library("cytofclean")

cytofclean_GUI()
```

<b>Credit:</b>

I re-used some of the [cytofkit](https://github.com/JinmiaoChenLab/cytofkit) code to create the GUI. Hope this is OK!
