# cytofclean

### Initial Release - 0.1 beta - Please feed back any [issues](https://github.com/JimboMahoney/cytofclean/issues)!

This is a small package to perform the following:

- Import any number of FCS files from a CyTOF v3 (Helios) (NOTE - the files should be normalised before using cytofclean!)
- Auto-gate the Event Length
- Auto-gate the Gaussian parameters (Centre -> Offset - Residual)
- Auto-gate the cells (based on excluding anything high in the Ce140 Channel)
- Output new FCS files in a subdirectory called "CyTOFClean" (they will have "CC" (short for cytofclean) added to their names, or timestamps if they already exist)
- Output a plot (PNG) of the gates used

e.g. as follows (this is not the best example, but gives an idea):

<img src="https://raw.githubusercontent.com/JimboMahoney/cytofclean/master/plots_15_28_57.png"
  align="center" />
  
## Why I created this:

- Gating on Event, Gaussian and Beads (Cells) should be easy enough for a computer to do.
- Reduced file size (typically, an 80% reduction! This is primarily due to the removal of the extra Fluidigm data, which is removed after doing any import -> export of a CyTOF FCS file - e.g. See El-ad's [YouTube](https://www.youtube.com/watch?v=47u4-vGXePY) video) for further analysis (e.g. upload to [Cytobank](https://cytobank.org/)).
- Save time doing boring gating - spend your time gating on the real data / markers of interest! cytofclean is FAST - e.g. 13 seconds for an 800MB FCS file with 1,000,000 events.
- I enjoy playing with data and R.
- Learning to build my first package.

## Testing / Development:

I've tested this script on quite a few datasets and it seems fairly robust. I figure that, even if it fails to clean up the data, not much has been lost because the processing is fast and new files are created.
<br>
At first, I started designing a Shiny GUI, which would allow me to show the plots and run in a browser window, with a more attractive interface. However, uploading files, even to a local instance of Shiny is incredibly slow, which defeats the purpose of a fast, simple script like this (it would take longer to upload the file than to process it!).
<br>
In addition, I've considered adding user-specified options, such as "filter strength" to allow tuning of the gates. This could probably be combined with an option to show the plots for feedback. But again, this somewhat defeats the purpose of the script, and you may as well gate manually if you want to change the output. Feedback would be welcome for anything that may be useful, as well as anything that may go wrong with the script once it's used on more datasets!


## Install / Run:

Run the below, which will:

1) Ensure devtools is installed (needed to install packages from github)
2) Download and install the cytofclean package (as well as its dependencies - svDialogs, tcltk2, flowCore, ggplot2 and cowplot)
3) Load the package
4) Run the GUI

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

## Credits:

I modified some of the [cytofkit](https://github.com/JinmiaoChenLab/cytofkit) code to create the GUI. Hope this is OK!
