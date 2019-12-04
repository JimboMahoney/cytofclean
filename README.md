# cytofclean

OK, so this is my first attempt at a package!

Install:

1 - Ensure devtools is installed:

'''
if(!require(devtools)){
  install.packages("devtools") # If not already installed
}
'''
2 - Install cytofclean package:
'''
if(!require(cytofclean)){
  devtools::install_github("JimboMahoney/cytofclean", dependencies = TRUE)
}
'''

3 - Load package:
'''
library("cytofclean")
'''
4 - Run!
'''
cytofclean_GUI()
'''
