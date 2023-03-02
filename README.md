# MultipleExperiment

The MultipleExperiment package defines S4 classes to handle data from multiple experiments or studies by providing features of lists as well as those of a concatenated experiment. Individual experiments can be in the form of SummarizedExperiment, Ranged SummarizedExperiment, SingleCellExperiment, or SpatialExperiment objects. Annotations specific to each experiment are stored thus providing a unified interface to dealing with data from multiple studies. Specialised functions to access experiment data, and to apply functions across experiments are implemented. Existing functions implemented for each individual experiment (e.g., SingleCellExperiment::reducedDim()) can be readily applied across the entire list of experiments.

![schematic](vignettes/EL.jpg)
