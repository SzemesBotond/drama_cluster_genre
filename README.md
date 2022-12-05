Clustering Dramatic Genres According to Structural Properties
Extracting structural features of dramas based on DraCor metadata, run classification methods and signifiant tests

drama_cluster_genre.R
Extracts all the features of the analysis based on the data downloaded from DraCor collection 
("shakedracor-metadata.csv", "gerdaracor-metadata.csv" and folders "shake_data" and "ger_data"), 
runs the SVM model for calssification, and visualizes the results

all_act_differences.R
Computes the differences between genres without specific acts based on the output of the folder "removed_act_analysis" 
(in our case: "shakespeare_metrics7.csv" and gercore_metrics_fo_5acts.csv")

removed_act_analysis
Substracts one act from 5-act-plays and computes basic netwrok properties based on the TEI XML files of the plays provided by the DraCor collection.
