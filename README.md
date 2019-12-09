# cytofclean

### Release - 0.6 beta - Please feed back any [issues](https://github.com/JimboMahoney/cytofclean/issues)!

This is a small package to perform the following:

- Import any number of FCS files from a CyTOF v3 (Helios) (NOTE - the files should be normalised either by CyTOF software or the [Finck method](https://github.com/ParkerICI/premessa) before using cytofclean!)
- Auto-gate the Event Length
- Auto-gate the Gaussian parameters (Centre -> Offset - Residual -> Width)
- Option to auto-gate the cells (based on finding the clearest separation of cells vs. beads in one of the bead channels)
- Output new FCS files in a subdirectory called "CyTOFClean" (they will have "CC" (short for cytofclean) added to their names, or timestamps if they already exist)
- Output a plot (PNG) of the gates used

e.g. as follows:

<img src="https://raw.githubusercontent.com/JimboMahoney/cytofclean/master/plots_09_21_13.png"
  align="center" />
  
## Why I created this:

- Gating on Event, Gaussian and Beads (Cells) should be easy enough for a computer to do.
- Reduced file size - typically, an 80% reduction! This is primarily due to the removal of the extra Fluidigm data, which is removed after doing any import -> export of a CyTOF FCS file - e.g. See El-ad's [YouTube](https://www.youtube.com/watch?v=47u4-vGXePY) video. This is beneficial as it reduces upload times and storage requirements for further analysis (e.g. upload to [Cytobank](https://cytobank.org/) and [Omiq](http://www.omiq.ai/)).
- Save time doing boring gating - spend your time gating on the real data / markers of interest! cytofclean is FAST - e.g. 13 seconds for an 800MB FCS file with 1,000,000 events.
- I enjoy playing with data and R.
- Learning to build my first package.

## What are "Gaussian Parameters"?

Fluidigm's [website](https://www.fluidigm.com/faq/helios-9) is pretty thin when it comes to explaining exactly what these are.
<br>
<br>
In simplistic terms, just like gating on the Event Length removes any spurious or "bad" data (such as ion cloud fusions), gating on the Gaussians further ensures that the data to be further analysed is of the highest quality. I have more information from Fluidigm on this, but I'm not sure if it's confidential / proprietary, so I won't post it here.
<br>
<br>
As an example of the difference between the data that comes straight off the instrument vs. data that's been cleaned using Event Length and Gaussians, compare the following two UMAP plots:

<img src="https://raw.githubusercontent.com/JimboMahoney/cytofclean/master/UMAP_raw.png"
  align="center" />
  <br>
  <img src="https://raw.githubusercontent.com/JimboMahoney/cytofclean/master/UMAP_Clean.png"
  align="center" />
  <br>
  The first, upper, plot is the data as it comes off the instrument. Notice there are a few "islands" that sit quite far from the bulk of the data and have very high and very low expression of some markers. This suggests that they could be aggregates / ion cloud fusions (high marker expression) or junk or EQ beads (low / zero expression).
   <br>
   <br>
 The second, lower, plot is the same dataset after being automatically cleaned using cytofclean. The islands have gone and we have a much clearer view of the data. Admittedly, I'm "cheating" a little here and making cytofclean look better / cleverer than it really is because it also does simple, standard things like gate on the Event Length and remove the beads as well as clean up the data using the Gaussian parameters. The upper plot is therefore probably not what most people would use for their downstream analysis.
 <br>
 

## Testing / Development:

I've tested this script on quite a few datasets and it seems fairly robust. I figure that, even if it fails to clean up the data, not much has been lost because the processing is fast and new files are created.
<br>
<br>
At first, I started designing a Shiny GUI, which would allow me to show the plots and run in a browser window, with a more attractive interface. However, uploading files, even to a local instance of Shiny is incredibly slow, which defeats the purpose of a fast, simple script like this (it would take longer to upload the file than to process it!).
<br>
<br>
In addition, I've considered adding user-specified options, such as "filter strength" to allow tuning of the gates. This could probably be combined with an option to show the plots for feedback. But again, this somewhat defeats the purpose of the script, and you may as well gate manually if you want to change the output. Feedback would be welcome for anything that may be useful, as well as anything that may go wrong with the script once it's used on more datasets!


## Install / Run:

1) Download and install [R](https://www.r-project.org/)
2) Optionally, but recommended, download an IDE such as [RStudio](https://rstudio.com/)

Run the below, which will:

1) Ensure devtools is installed (needed to install packages from github)
2) Download and install the cytofclean package (as well as its dependencies - tcltk2, flowCore, ggplot2 and cowplot)
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
<br>
<br>
Thanks also to El-ad of [Astrolabe Diagnostics](https://astrolabediagnostics.com/) for his advice on using the density function instead of histogram smoothing and to Mike Leipold of Stanford for his words of caution on gating beads. (Name-dropping them here doesn't mean they were involved in the development of this code - just in alerting me to some data-handling and gotchas!).

## Inspiration:

A much more complex package for flow cytometry data - [flowAI](https://bioconductor.org/packages/release/bioc/html/flowAI.html)
