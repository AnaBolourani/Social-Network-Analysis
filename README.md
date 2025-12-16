# Individual Differences Shape Emotional and Cognitive Similarity Networks Across Social Scenarios

This repository contains the code and materials for the final project of **STAT 218: Statistical Analysis of Networks** at **UCLA**, taught by **Professor Mark S. Handcock**.

**Author:** Anahita Bolourani
**Course:** UCLA STAT 218 – Statistical Analysis of Networks
**Term:** Fall 2025

---

## 📌 Project Overview

This project investigates how **emotional and cognitive similarity** between individuals emerges across common social scenarios, and whether such similarity is driven primarily by **network structure** (e.g., clustering and transitivity) or by **stable individual differences** such as personality, emotional intelligence, and cognitive performance.

Rather than treating similarity as a simple pairwise statistic, the project models similarity as a **network tie** and applies **Exponential Random Graph Models (ERGMs)** to study the mechanisms governing tie formation and tie strength.

A full description of the methods, results, and discussion is provided in the accompanying paper .

---

## 🧠 Research Questions

The project addresses three core questions:

1. **Do stable individual differences predict who becomes connected in high-similarity networks?**
2. **How much of the observed similarity structure is explained by network mechanics (sparsity and transitivity) rather than attributes?**
3. **Do conclusions differ across emotion vs. appraisal domains, and binary vs. valued network representations?**

---

## 📊 Data Description

* **Participants:** n individuals (complete-case sample)
* **Scenarios:** some hypothetical social scenarios 
* **Responses per scenario:**

  * **Emotions:** sadness, anger, neutral, fear, worry, shame, guilt
  * **Appraisals:** valence, urgency, coping, expectedness, relevance, morality, cause
* **Individual-level covariates:**

  * Demographics (age, gender)
  * Big Five personality traits
  * Emotional intelligence measures (e.g., STEM, STEU)
  * Cognitive/performance measures (e.g., GERT)

⚠️ **Note:** The dataset is **private** and used solely for course purposes. It is **not included** in this repository.

---

## 🔧 Methodology

### Similarity Computation

* Responses are **z-scored within scenario**
* Pairwise similarity is computed using **standardized Euclidean distance**
* Distances are mapped to similarities on ([0,1])

### Network Construction

* **Scenario-specific binary networks** (emotion & appraisal)

  * Edge if similarity ≥ 0.90
* **Aggregated mean-profile binary networks**
* **Valued emotion network**

  * Edge weight = number of scenarios (0–6) with similarity ≥ 0.85

### Modeling

* **Binary ERGMs (MPLE)** for scenario-specific networks
* **MCMC-based ERGMs (MCMLE)** for aggregated and valued emotion networks
* Structural terms (edges, GWESP) included in all models
* Attribute effects modeled via homophily / complementarity (absolute differences)

---

## 📈 Key Findings

* **Network structure dominates tie existence**
  High-similarity ties are rare but strongly clustered; transitivity is the most stable predictor.
* **Individual differences matter more for tie strength than tie presence**
  Valued ERGMs reveal strong homophily in emotional intelligence (especially STEM and STEU).
* **Emotion similarity is more stable across scenarios than appraisal similarity**
* Evidence of **both homophily and complementarity**, depending on the trait and modeling level.


## 🧪 Reproducibility Notes

* Analyses are implemented in **R**
* Key packages include: `ergm`, `network`, `statnet`, `ggplot2`
* Random seeds are set where applicable for reproducibility
* Because the dataset is private, scripts assume access to locally stored data files



## 🎓 Course Context

This repository was created as the **final project for UCLA STAT 218 (Statistical Analysis of Networks)**.
The project emphasizes:

* Network thinking over pairwise analysis
* Careful interpretation of ERGM results
* Distinguishing structural effects from attribute-driven mechanisms


