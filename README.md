# cytofclean

### Version 0.6 beta - Please feed back any [issues](https://github.com/JimboMahoney/cytofclean/issues)!

This is a small package to perform the following:

- Import any number of FCS files from a CyTOF v3 (Helios) (NOTE - the files should be normalised and/or concatenated either by CyTOF software or the [Finck method](https://github.com/ParkerICI/premessa) <b>before</b> using cytofclean! See link below explaining how CyTOF FCS files are affected by any non-Fluidigm analysis tools) 
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
In simplistic terms, just like gating on the Event Length removes any spurious or "bad" data (such as ion cloud fusions), gating on the Gaussians further ensures that the data to be further analysed is of the highest quality. Further details can be found in the following publications:
<br>
<br>
[Fluidigim Technical Note](https://www.fluidigm.com/binaries/content/documents/fluidigm/marketing/bivariate-analysis-using-the-maxpar-human-immune-monitoring-panel-kit-400270-tn-mktg/bivariate-analysis-using-the-maxpar-human-immune-monitoring-panel-kit-400270-tn-mktg/fluidigm%3Afile) - See pages 5-7.
<br>
[Automated Data Cleanup for Mass Cytometry](https://onlinelibrary.wiley.com/doi/full/10.1002/cyto.a.23926)
<br>
[Multi-site reproducibility using CyTOF](https://onlinelibrary.wiley.com/doi/full/10.1002/cyto.b.21858)
<br>
[Acquisition, Processing and Quality Control of Mass Cytometry Data](https://link.springer.com/protocol/10.1007/978-1-4939-9454-0_2)

<br>
<br>
As an example of the difference between the data that comes straight off the instrument vs. data that's been cleaned using Event Length and Gaussians, compare the following two UMAP plots:
<br>
<br>

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

## Some Details of the Cleanup methods (nobody likes a black box!):

For the Event Length and Gaussian parameters, cytofclean uses the density function to "see" the spread of the data. Where the bulk of the events are located, it will apply a cutoff at a certain density value. This ensures that the shape (i.e. bias or skew) of the data is taken into account. For example, Event Length tends to have a sharp "edge" at around 10 pushes (this is the lower cutoff in CyTOF software) and then trail off into the upper region (e.g. 100). The Gaussian parameters tend to be more symmetrical, but each are slightly different, so cytofclean has been "trained" on a multitude of "good" and "bad" datasets to ensure it tends to produce sensible results in all cases. 
<br><br>
There are some parameters (the Gaussians) that have "safety limits" applied - e.g. preventing negative or zero values - that are only necessary in "bad" datasets.
<br><br>
The removal of beads is much more challenging. This is primarily because cytofclean has no idea whether the bead channels (140, 151, 153, 165 and 175) have also been used for cell markers.
<br><br>
It will first look for which of these channels is present in the data. It will then use the density function of each to determine which is the most suitable for bead discrimination. It does this by finding the channel with the lowest density in the region where we don't expect to find beads - i.e. such that the separation between what might be cells and beads is clearest.
<br><br>
It then uses only this channel to remove the beads by setting the threshold to the lowest density value.
<br><br>
Of all the processing that cytofclean does, this is the most likely to fail or produce eroneous results, hence it's optional.
<br><br>
Having said that, I haven't seen it fail on any of the data available to me, which includes data in which almost all the bead channels are also used as cell markers.

## Disclaimer:

As with any piece of software, there may be bugs, so please report any [issues](https://github.com/JimboMahoney/cytofclean/issues).
<br>
<br>
cytofclean should be pretty safe to use because it creates <b>new</b> files, rather than altering existing ones. Plus, it shows you plots of how it's gated the files. However, like any tool, it should not be relied upon for ensuring that your data is clean / scientifically valid.
<br>
<br>
You <b>will</b> need to do further gating / clean-up (e.g. Live/Dead, doublets, as well as looking at the Event Length) to determine if you are happy with the data.


