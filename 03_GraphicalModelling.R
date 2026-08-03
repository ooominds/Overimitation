
library(openxlsx)
library(car)
library(usdm)
library(ppcor)
library(pcalg)
library(igraph)
library(InvariantCausalPrediction)

# read in dataset and attach for ease of reference
Imitation_Data_MasterFile = read.xlsx("Imitation_Data.xlsx")

# View(Imitation_Data_MasterFile) 
# str(Imitation_Data_MasterFile) 

attach(Imitation_Data_MasterFile)

# adding pca scores
load("pca_scores.rda")
Imitation_Data_MasterFile = merge(Imitation_Data_MasterFile, pca_scores,
    by.x="ID", by.y="ParticipantID")


#########################
### MULTICOLLINEARITY ###
#########################

variables = c("GJT", "Collocation", "SRT_S1_Intercept",
    "MimicExternally", "MimicInternally", "MimicCompulsively",
    "BOX_PCA1", "Write_PCA1", "Speak_PCA1",
    "Llama_Score",  "OSPAN_Letter", "OSPAN_Math", "Flanker_DIFF")
imitatedat = na.omit(Imitation_Data_MasterFile[,variables]) # WE LOSE 3: 42 to 39

# # correlations -- nothing terrible
# mcor = round(cor(imitatedat, use="pairwise.complete.obs"), 2)
# mcor[upper.tri(mcor, diag=TRUE)] = NA
# mcor[abs(mcor) < 0.4] = NA
# print(mcor, na.print="")

# partial correlations -- nothing terrible
mpcor = round(pcor(imitatedat)$estimate, 2)
mpcor[upper.tri(mpcor, diag=TRUE)] = NA
mpcor[abs(mpcor) < 0.31] = NA
print(mpcor, na.print="")

# # density plots
# for (i in 1:ncol(imitatedat)) {
#     densityPlot(imitatedat[,i], xlab=colnames(imitatedat)[i])
#     readline()
# }

# vifcor(imitatedat, th=0.7) # at th=0.6 two to remove: OSPAN_Letter, GJT


###########################
### Graphical Modelling ###
###########################

# + This is a custom function that uses Fisher’s Z-transform
#   to convert partial correlations into p-values.
# + It assumes your pcor() matrix is symmetric and reflects
#   conditioning on all other variables.
#   May need adapting if partials are computed differently
#   (e.g., conditioning on subsets).
customCItest <- function(x, y, S, suffStat) {
    r <- suffStat$PC[x, y] # always use the precomputed partial correlation
    # Fisher's Z-transform
    z <- 0.5 * log((1 + r) / (1 - r))
    n <- suffStat$n
    stat <- sqrt(n - length(S) - 3) * abs(z)
    # Two-sided p-value
    pval <- 2 * (1 - pnorm(stat))
    return(pval)
}

set.seed(1828)

n = nrow(imitatedat)
V = colnames(imitatedat) # node names
pc.imitate = pc(suffStat=list(PC=pcor(imitatedat)$estimate, n=n),
             indepTest=customCItest, # indep.test
             alpha=0.05, labels=V, u2pd="retry")

grImitate = graph_from_graphnel(pc.imitate@graph) |>
    set_edge_attr("color", value="black")
# png("GraphicalModelling_pcor_0.05.png", he=5, wi=5, units="in", res=300)
par(mar=c(1,1,1,1))
plot.igraph(grImitate, vertex.size=20, layout=layout_nicely,
    vertex.label.cex=1, vertex.label.color="black",
    edge.arrow.size=0.25)
# dev.off()

# If we want this, I need to tidy all up
# and make some nice(r) graph...


#############################
### Invariant Predictions ###
#############################

invardat = na.omit(cbind(Imitation_Data_MasterFile[,variables], "Box_Type"=as.factor(Imitation_Data_MasterFile$Box_Type)))

X1 <- as.matrix(invardat[,2:13])
Y1 <- invardat[,1]
expVar1 <- invardat[,14] # binary or categorical experimental variable

# Run ICP
imitateICP <- ICP(X=X1, Y=Y1, ExpInd=expVar1, test="exact")
print(imitateICP)
#                     LOWER BOUND  UPPER BOUND  MAXIMIN EFFECT  P-VALUE
# Collocation               0.00         1.92            0.00        1
# SRT_S1_Intercept        -13.15         0.66            0.00        1
# MimicExternally          -2.61         8.43            0.00        1
# MimicInternally          -2.94         7.93            0.00        1
# MimicCompulsively        -4.46         5.66            0.00        1
# BOX_PCA1                 -6.98         2.77            0.00        1
# Write_PCA1               -2.52         8.10            0.00        1
# Speak_PCA1                0.00         0.00            0.00        1
# Llama_Score               0.00         0.00            0.00        1
# OSPAN_Letter              0.00         0.00            0.00        1
# OSPAN_Math                0.00         0.00            0.00        1
# Flanker_DIFF             -0.13         0.16            0.00        1


