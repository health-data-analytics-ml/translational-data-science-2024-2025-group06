#PBS -l walltime=48:00:00
#PBS -l select=1:ncpus=2:mem=100gb

module load anaconda3/personal
source activate r441
cd /rds/general/user/cat24/projects/hda_24-25/live/TDS/Group06/Scripts/Clustering/Gaussian_Mixture_Model

Rscript gmm_cluster_all_scores.R
