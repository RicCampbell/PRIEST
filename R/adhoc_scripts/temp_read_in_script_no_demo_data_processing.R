library(data.table)

##Adhoc script for reading in data that hasn't been processed as do not have demo data yet

#epr
load("data/datasets/epr_tables-2020-09-15 151647.rda")

cad_data <- data.table(readRDS("data/datasets/cad_data-2020-09-15 151647.rds"))

nhs111_data <- data.table(readRDS("data/datasets/nhs111-2020-09-15 151647.rds"))

apc1 <- fread("D:/source_data/nhs_digital/apc/NIC377644_HES_APC_201999_APPROVED_51667_01010001_0.txt")
apc2 <- fread("D:/source_data/nhs_digital/apc/NIC377644_HES_APC_202006_APPROVED_51668_01010001_0.txt")
apc_data <- rbind(apc1, apc2)


critical_care1 <- fread("D:/source_data/nhs_digital/cc/NIC377644_HES_CC_201999_APPROVED_51669_01010001_0.txt")
critical_care2 <- fread("D:/source_data/nhs_digital/cc/NIC377644_HES_CC_202006_APPROVED_51670_01010001_0.txt")
critical_care_data <- rbind(critical_care1, critical_care2)

ecds1 <- fread("D:/source_data/nhs_digital/ecds/NIC377644_ECDS_201999_APPROVED_51664_01010001_0.txt")
ecds2 <- fread("D:/source_data/nhs_digital/ecds/NIC377644_ECDS_202006_APPROVED_51665_01010001_0.txt")
ecds_data <- rbind(ecds1, ecds2)
setnames(ecds_data, "NHS_NUMBER", "patient_id")

gdppr_data <- fread("D:/source_data/nhs_digital/gdppr/NIC377644_GDPPR_20201204_APPROVED_51666_01010001_0.txt")


rm(apc1, apc2, critical_care1, critical_care2, ecds1, ecds2)
