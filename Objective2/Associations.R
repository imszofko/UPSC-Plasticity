SwAspAnno <- read.table("~/UPSC_ThesisWork/Data/ANNOVAR/SwAspanno.Het.MAF.HWE.variant_function", header = T, sep = '\t')
chr8 <- SwAspAnno[SwAspAnno$chr1 == "chr8",]
chr8 <- as.data.frame(chr8)
chr10 <- SwAspAnno[SwAspAnno$chr1 == "chr10",]


SwAspRP <- read.table("~/UPSC_ThesisWork/Data/Associations/All_traits_qvalue/R-stderrEst_1000SNPs_or_qvalue_0.05_annotated.txt", header = T, sep = '\t')
SwAspRG <- read.table("~/UPSC_ThesisWork/Data/Associations/All_traits_qvalue/G-genoEst_1000SNPs_or_qvalue_0.05_annotated.txt", header = T, sep = '\t')
SwAspE <- read.table("~/UPSC_ThesisWork/Data/Associations/All_traits_qvalue/E-stderrEst_1000SNPs_or_qvalue_0.05_annotated.txt", header = T, sep = '\t')
SwAspEG <- read.table("~/UPSC_ThesisWork/Data/Associations/All_traits_qvalue/EG-genoEst_1000SNPs_or_qvalue_0.05_annotated.txt", header = T, sep = '\t')
SwAspS <- read.table("~/UPSC_ThesisWork/Data/Associations/All_traits_qvalue/S-stderrEst_1000SNPs_or_qvalue_0.05_annotated.txt", header = T, sep = '\t')
SwAspSG <- read.table("~/UPSC_ThesisWork/Data/Associations/All_traits_qvalue/SG-genoEst_1000SNPs_or_qvalue_0.05_annotated.txt", header = T, sep = '\t')


