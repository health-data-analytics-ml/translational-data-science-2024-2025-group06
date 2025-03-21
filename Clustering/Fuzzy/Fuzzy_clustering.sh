#PBS -l walltime=8:00:00
#PBS -l select=1:ncpus=1:mem=250gb
#PBS -N extraction

cd /rds/general/user/tl423/projects/hda_24-25/live/TDS/Group06/Scripts/Clustering/Fuzzy
module load anaconda3/personal
source activate r413

Rscript Fuzzy_clustering.R