
library(openxlsx)
library(dplyr)
library(tidyr)
library(psych)
library(GPArotation)

# read in dataset and attach for ease of reference
mimicdat = read.csv("Mimicry.csv")
imitdat = read.csv("Imitation_Data.csv")

###################
### Mimicry PCA ###
###################

# scree(mimicdat[,-c(1,6)]) # suggests 3 components

pca_mimic = principal(mimicdat[,-c(1,6)], nfactors=3, rotate="varimax")
print(pca_mimic$loadings, sort=TRUE, cutoff=0.4)
# Loadings:
#                                RC1    RC2    RC3   
# MimicAccent                     0.900              
# MimicPhrases                    0.803              
# MimicRandomSoundsSpontaneously  0.637 -0.467       
# TalkToMyself                           0.840       
# MemoriseLyricsPoems                    0.787       
# MimicRandomSoundsCompulsively                 0.951
# 
#                  RC1   RC2   RC3
# SS loadings    1.930 1.684 1.106
# Proportion Var 0.322 0.281 0.184
# Cumulative Var 0.322 0.602 0.787

mimic_scores = data.frame(
    "ParticipantID" = mimicdat[,1],
    "MimicExternally" = pca_mimic$scores[,1],
    "MimicInternally" = pca_mimic$scores[,2],
    "MimicCompulsively" = pca_mimic$scores[,3]
)

# replace missing values with Medians
mimic_scores[,2:4] <- mimic_scores[,2:4] %>% mutate(across(where(is.numeric),
    ~replace_na(., median(., na.rm=TRUE))))

# Alpha
psych::alpha(mimicdat[,-c(1,6)], check.keys=TRUE)
  # raw_alpha std.alpha G6(smc) average_r S/N  ase mean   sd median_r
  #     0.67      0.69    0.74      0.27 2.2 0.08  2.3 0.66     0.28

# Omega
omega(mimicdat[,-c(1,6)], nfactors=3)
# Alpha:                 0.69 
# G.6:                   0.74 
# Omega Hierarchical:    0.53 
# Omega H asymptotic:    0.64 
# Omega Total            0.83 

###############
### BOX PCA ###
###############

pca_box = principal(imitdat[,c(35,36,39,40)], nfactors=1)
print(pca_box$loadings, sort=TRUE, cutoff=0.4)
# Loadings:
#                    PC1  
# BOX1_Overimitation 0.840
# BOX1_Fidelity      0.855
# BOX2_Overimitation 0.770
# BOX2_Fidelity      0.871
# 
#                  PC1
# SS loadings    2.787
# Proportion Var 0.697

box_scores = data.frame(
    "ParticipantID" = imitdat[,1],
    "BOX_PCA1" = pca_box$scores[,1]
)

#################
### Write PCA ###
#################

pca_write = principal(imitdat[,c(19,20,24,25)], nfactors=1)
print(pca_write$loadings, sort=TRUE, cutoff=0.4)
# Loadings:
#                 PC1  
# Char1_Fidelity  0.634
# Char1_Imitation 0.723
# Char2_Fidelity  0.781
# Char2_Imitation 0.803
# 
#                  PC1
# SS loadings    2.179
# Proportion Var 0.545

write_scores = data.frame(
    "ParticipantID" = imitdat[,1],
    "Write_PCA1" = pca_write$scores[,1]
)

#################
### Speak PCA ###
#################

pca_speak = principal(imitdat[,c(29:34)], nfactors=1)
print(pca_speak$loadings, sort=TRUE, cutoff=0.4)
# Loadings:
#               PC1  
# Bday_Pron     0.744
# Bday_Tone     0.820
# Bday_Chunking 0.574
# Uni_Pron      0.595
# Uni_Tone      0.609
# Uni_Chunking  0.496
# 
#                  PC1
# SS loadings    2.527
# Proportion Var 0.421

speak_scores = data.frame(
    "ParticipantID" = imitdat[,1],
    "Speak_PCA1" = pca_speak$scores[,1]
)

# replace missing values with Medians
speak_scores <- speak_scores %>%
  mutate(Speak_PCA1 = replace_na(Speak_PCA1, median(Speak_PCA1, na.rm = TRUE)))

########################
### Combine and Save ###
########################
pca_scores <- merge(mimic_scores, box_scores, by="ParticipantID") %>%
    merge(write_scores, by="ParticipantID") %>%
    merge(speak_scores, by="ParticipantID")
save(pca_scores, file="pca_scores.rda")


