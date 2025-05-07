#PBS -l walltime=48:00:00
#PBS -l select=1:ncpus=2:mem=100gb

module load anaconda3/personal
source activate r441
cd /rds/general/user/cat24/projects/hda_24-25/live/TDS/Group06/Scripts/Clustering/Scores

Rscript 3-pac_biclustering.R