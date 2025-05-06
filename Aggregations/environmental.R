# ------------------------------------------------------------------------------
# Purpose: This script outlines the cleaning and recoding only for environmental
# variables in the UK Biobank
# ------------------------------------------------------------------------------

# Data =========================================================================
rm(list = ls())
ukb_recoded <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_recoded.rds')
ukb_extracted <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_extracted.rds')

## No2_2010 2010 ===============================================================
summary(ukb_recoded$no2_2010.0.0)
ukb_recoded <- rename(ukb_recoded, 'no2_2010' = 'no2_2010.0.0')

## Pm10 2010 ===================================================================
summary(ukb_recoded$pm10.0.0)
ukb_recoded <- rename(ukb_recoded, 'pm10' = 'pm10.0.0')

## Pm2.5.0.0 2010 ==============================================================
summary(ukb_recoded$pm2.5.0.0)
ukb_recoded <- rename(ukb_recoded, 'pm2.5' = 'pm2.5.0.0')

## Traffic intensity ===========================================================
summary(ukb_recoded$traffic_intensity.0.0)
ukb_recoded <- rename(ukb_recoded, 'traffic_intensity' = 'traffic_intensity.0.0')

## Inverse distance to major road ==============================================
summary(ukb_recoded$inv_dis_maj_road.0.0)
ukb_recoded <- rename(ukb_recoded, 'inv_dis_maj_road' = 'inv_dis_maj_road.0.0')

## Greenspace percentage, buffer 1000m =========================================
summary(ukb_recoded$greenspace_1000m.0.0)
ukb_recoded <- rename(ukb_recoded, 'greenspace_1000m' = 'greenspace_1000m.0.0')

## Water percentage, buffer 1000m ==============================================
summary(ukb_recoded$water_1000m.0.0)
ukb_recoded <- rename(ukb_recoded, 'water_1000m' = 'water_1000m.0.0')

## Distance (Euclidean) to coast ===============================================
summary(ukb_recoded$coast_distance.0.0)
ukb_recoded <- rename(ukb_recoded, 'coast_distance' = 'coast_distance.0.0')


