#PBS -l walltime=20:00:00
#PBS -l select=1:ncpus=1:mem=100gb
#PBS -N extraction

cd /rds/general/user/hc724/projects/hda_24-25/live/TDS/Group06/Scripts/Analysis/Proteins_Analysis

module load anaconda3/personal
source activate r413

Rscript proteins_stability_lasso_full.R