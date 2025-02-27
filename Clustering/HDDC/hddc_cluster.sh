#PBS -l walltime=48:00:00
#PBS -l select=1:ncpus=1:mem=100gb
#PBS -N HDDC

cd /rds/general/user/ilo24/projects/hda_24-25/live/TDS/Group06/Scripts/Clustering/HDDC

module load anaconda3/personal
source activate r413

Rscript 1-hddc_cluster.R
