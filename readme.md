## Stellenbosch-L linkage
This contains the script and models used to link households in the Stellenbosch-Long opgaafrollen data. It uses the [capelinker](https://github.com/rijpma/capelinker) library in a number of key steps.

1. prep.R -- cleans the data in preparation for training and linkage
1. train.R -- trains an xgboost classifier
1. evaluate.R -- evaluates the classifier
1. predict.R -- makes link predictions
1. reconstruct.R -- reconstruct links on the basis of linkset
1. graphs.R -- graph creation and subsetting of the linkset for evaluation
1. finish.R -- adds numeric data for complete dataset, subset columns
1. addsaf.R -- adds the links to the SAF data
1. describe.R -- describe resulting panel

Datasets

Not part of core workflow

1. extend.R -- contains a function for reconstruction, should perhaps be merged into capelinker
1. longevals.R -- evaluations used in model selection
1. minimalcandidates.R -- file to create a training data set for manual labelling
1. explore.R -- quick attempt at a model
