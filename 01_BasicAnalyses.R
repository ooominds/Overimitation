# Install packages (run once only)
install.packages("effectsize")

# Load packages
library(effectsize)

# Read dataset
Imitation_Data_MasterFile <- read.csv("Imitation_Data.csv")

#--- change box type to factor variable --
Imitation_Data_MasterFile$Box_Type <- factor(Imitation_Data_MasterFile$Box_Type)
str(Imitation_Data_MasterFile$Box_Type)

# Remove leading and trailing spaces
Imitation_Data_MasterFile$Box_Type <- trimws(
  as.character(Imitation_Data_MasterFile$Box_Type)
)

# Replace blank strings with NA
Imitation_Data_MasterFile$Box_Type[
  Imitation_Data_MasterFile$Box_Type == ""
] <- NA

# Convert to a factor and remove unused levels
Imitation_Data_MasterFile$Box_Type <- droplevels(
  factor(Imitation_Data_MasterFile$Box_Type)
)

# Check the levels
levels(Imitation_Data_MasterFile$Box_Type)
table(Imitation_Data_MasterFile$Box_Type, useNA = "ifany")


# write.csv(
#   Imitation_Data_MasterFile,
#   "Imitation_Data.csv",
#   row.names = FALSE
# )


# --------- Paired t-test ----------------

# t-tests to compare BOX1 and BOX2 results
#significant difference in the Duration only

# --- Overimitation --- 
summary(Imitation_Data_MasterFile$BOX1_Overimitation)
summary(Imitation_Data_MasterFile$BOX2_Overimitation)

t.test(Imitation_Data_MasterFile$BOX1_Overimitation,
       Imitation_Data_MasterFile$BOX2_Overimitation,
       paired = TRUE)
cohens_d(
  Imitation_Data_MasterFile$BOX2_Overimitation,
  Imitation_Data_MasterFile$BOX1_Overimitation,
  paired = TRUE)


# --- Fidelity ---
summary(Imitation_Data_MasterFile$BOX1_Fidelity)
summary(Imitation_Data_MasterFile$BOX2_Fidelity)

t.test(
  Imitation_Data_MasterFile$BOX1_Fidelity,
  Imitation_Data_MasterFile$BOX2_Fidelity,
  paired = TRUE)

cohens_d(
  Imitation_Data_MasterFile$BOX2_Fidelity,
  Imitation_Data_MasterFile$BOX1_Fidelity,
  paired = TRUE
)

# --- Fidelity specific --- 
summary(Imitation_Data_MasterFile$BOX1_Fidelity_Specific)
summary(Imitation_Data_MasterFile$BOX2_Fidelity_Specific)

t.test(
  Imitation_Data_MasterFile$BOX1_Fidelity_Specific,
  Imitation_Data_MasterFile$BOX2_Fidelity_Specific,
  paired = TRUE)

cohens_d(
  Imitation_Data_MasterFile$BOX2_Fidelity_Specific,
  Imitation_Data_MasterFile$BOX1_Fidelity_Specific,
  paired = TRUE
)

# --- Duration ---
summary(Imitation_Data_MasterFile$BOX1_Duration)
summary(Imitation_Data_MasterFile$BOX2_Duration)
t.test(
  Imitation_Data_MasterFile$BOX1_Duration,
  Imitation_Data_MasterFile$BOX2_Duration,
  paired = TRUE)

cohens_d(
  Imitation_Data_MasterFile$BOX2_Duration,
  Imitation_Data_MasterFile$BOX1_Duration,
  paired = TRUE
)


# check whether there is a significant difference between transparent 
# and opaque: no significant differences


# --- Independent sample t-test -----

t.test(BOX1_Overimitation~Box_Type, data=Imitation_Data_MasterFile)
cohens_d(
  BOX1_Overimitation ~ Box_Type,
  data = Imitation_Data_MasterFile
)

t.test(BOX2_Overimitation~Box_Type, data=Imitation_Data_MasterFile)
cohens_d(
  BOX2_Overimitation ~ Box_Type,
  data = Imitation_Data_MasterFile
)


t.test(BOX1_Fidelity~Box_Type, data=Imitation_Data_MasterFile)
cohens_d(
  BOX1_Fidelity ~ Box_Type,
  data = Imitation_Data_MasterFile
)


t.test(BOX2_Fidelity~Box_Type, data=Imitation_Data_MasterFile)
cohens_d(
  BOX2_Fidelity ~ Box_Type,
  data = Imitation_Data_MasterFile
)


t.test(BOX1_Fidelity_Specific~Box_Type, data=Imitation_Data_MasterFile)
cohens_d(
  BOX1_Fidelity_Specific ~ Box_Type,
  data = Imitation_Data_MasterFile
)


t.test(BOX2_Fidelity_Specific~Box_Type, data=Imitation_Data_MasterFile)
cohens_d(
  BOX2_Fidelity_Specific ~ Box_Type,
  data = Imitation_Data_MasterFile
)

t.test(BOX1_Duration~Box_Type, data=Imitation_Data_MasterFile)
cohens_d(
  BOX1_Duration ~ Box_Type,
  data = Imitation_Data_MasterFile
)


t.test(BOX2_Duration~Box_Type, data=Imitation_Data_MasterFile)
cohens_d(
  BOX2_Duration ~ Box_Type,
  data = Imitation_Data_MasterFile
)

# ------------------------------------------------
# -------------- Spoken Imitation ---------------- 

# --- pronunciation ----------------
t.test(
  Imitation_Data_MasterFile$Bday_Pron,
  Imitation_Data_MasterFile$Uni_Pron,
  paired = TRUE)

cohens_d(
  Imitation_Data_MasterFile$Uni_Pron,
  Imitation_Data_MasterFile$Bday_Pron,
  paired = TRUE
)

# --- tone --------------------------
t.test(
  Imitation_Data_MasterFile$Bday_Tone,
  Imitation_Data_MasterFile$Uni_Tone,
  paired = TRUE)

cohens_d(
  Imitation_Data_MasterFile$Uni_Tone,
  Imitation_Data_MasterFile$Bday_Tone,
  paired = TRUE
)

# --- chunking ------------------------
t.test(
  Imitation_Data_MasterFile$Bday_Chunking,
  Imitation_Data_MasterFile$Uni_Chunking,
  paired = TRUE)

cohens_d(
  Imitation_Data_MasterFile$Uni_Chunking,
  Imitation_Data_MasterFile$Bday_Chunking,
  paired = TRUE
)


# --------------------------------------------
# --------- Character Imitation --------------

# --- Overimitation----------------------
t.test(
  Imitation_Data_MasterFile$Char1_Imitation,
  Imitation_Data_MasterFile$Char2_Imitation,
  paired = TRUE
)
cohens_d(
  Imitation_Data_MasterFile$Char2_Imitation,
  Imitation_Data_MasterFile$Char1_Imitation,
  paired = TRUE
)

# ---- Time ------------------------------
t.test(
  Imitation_Data_MasterFile$Char1_Time,
  Imitation_Data_MasterFile$Char2_Time,
  paired = TRUE
)
cohens_d(
  Imitation_Data_MasterFile$Char2_Time,
  Imitation_Data_MasterFile$Char1_Time,
  paired = TRUE
)

# --- Correction ----------------------------
t.test(
  Imitation_Data_MasterFile$Char1_Correction,
  Imitation_Data_MasterFile$Char2_Correction,
  paired = TRUE
)
cohens_d(
  Imitation_Data_MasterFile$Char2_Correction,
  Imitation_Data_MasterFile$Char1_Correction,
  paired = TRUE)

# --------- Fidelity ------------------------
t.test(
  Imitation_Data_MasterFile$Char1_Fidelity,
  Imitation_Data_MasterFile$Char2_Fidelity,
  paired = TRUE
)
cohens_d(
  Imitation_Data_MasterFile$Char2_Fidelity,
  Imitation_Data_MasterFile$Char1_Fidelity,
  paired = TRUE
)

# ------------ Structure -------------------
t.test(
  Imitation_Data_MasterFile$Char1_STR,
  Imitation_Data_MasterFile$Char2_STR,
  paired = TRUE
)
cohens_d(
  Imitation_Data_MasterFile$Char2_STR,
  Imitation_Data_MasterFile$Char1_STR,
  paired = TRUE
)

# -------------------------------------------
# ---------- Other Tasks --------------------

# ------------ SRT --------------------------
t.test(Imitation_Data_MasterFile$SRT_S1_Intercept,
       Imitation_Data_MasterFile$SRT_S2_Intercept,
       paired = TRUE)

cohens_d(
  Imitation_Data_MasterFile$SRT_S2_Intercept,
  Imitation_Data_MasterFile$SRT_S1_Intercept,
  paired = TRUE
)

t.test(Imitation_Data_MasterFile$SRT_S1_Slope,
       Imitation_Data_MasterFile$SRT_S2_Slope,
       paired = TRUE)

cohens_d(
  Imitation_Data_MasterFile$SRT_S2_Slope,
  Imitation_Data_MasterFile$SRT_S1_Slope,
  paired = TRUE
)


# ---------- Flanker -----------------------

mean(Imitation_Data_MasterFile$Flanker_CG, na.rm = TRUE)
sd(Imitation_Data_MasterFile$Flanker_CG, na.rm = TRUE)


mean(Imitation_Data_MasterFile$Flanker_INCG, na.rm = TRUE)
sd(Imitation_Data_MasterFile$Flanker_INCG, na.rm = TRUE)

# ---------------------------------------------------------------
# ---------------------------------------------------------------



