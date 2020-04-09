# AK_CA_Combustion
Scripts used to model carbon combustion across Canada and Alaska.  This includes scaling differen't pixel sizes, final model predictions and model training.

This v2 branch has made edits to scripts primarily where I no longer am looking at making models where we have all vars, no stand age, no fwi and then a fourth no stand age and no fwi model.  We now just have all vars with fwi and then no fwi.  Stand age has been removed entirely.  In addition I found problems in v1 with how the vegetation predictors (JP, WS, BS etc.) were used which caused problems in scaling and predictions.  I have no updated these scripts and earth engine scripts.  In addition I have upload the final prediction scripts to use type 2 regression and have finalized the monte carlo analysis
