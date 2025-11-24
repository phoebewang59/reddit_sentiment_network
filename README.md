# Reddit Mental Health Sentiment & Network Analysis

This project analyzes conversational dynamics within mental health–related Reddit communities using **sentiment analysis** and **social network analysis**. The goal is to understand how emotional tone flows through discussion threads, how supportive vs. unsupportive exchanges differ structurally, and whether **signed triads** reveal patterns of emotional tension or repair.

## Overview
Using a cleaned dataset of Reddit submissions, comments, and replies, this project:

- Computes sentiment scores for each message using `sentimentr` 
- Constructs directed reply networks using `igraph`  
- Identifies key structural patterns such as hubs, chains, and local clusters  
- Examines **triad sentiment flow** (increasing, decreasing, mixed)  
- Visualizes ego networks and sentiment diffusion patterns  
- Explores relationships between engagement, centrality, and sentiment 

---

## Sample Figures

### Full Reply Network Colored by Sentiment
<img src="figures/full_network_sentiment_centrality.png" width="400"/>

### Positive and Negative Sentiment Subgraphs

<p float="left">
  <img src="figures/positive_sentiment_subgraph.png" width="480"/>
  <img src="figures/negative_sentiment_subgraph.png" width="480"/>
</p>

---

## Key Findings
- Positive sentiment formed cohesive, supportive clusters centered around the original poster.
- Negative sentiment propagated in longer, chain-like reply patterns.
- Mixed or unbalanced triads often shifted toward more positive exchanges, suggesting emotional repair.
- Engagement volume was not a strong predictor of sentiment; structural context may play a larger role.

---

## Project Paper

The full academic write-up of this study can be found in:

[project_paper.pdf](writeup/project_paper.pdf)

It summarizes the motivation, theoretical background (including structural balance theory and sentiment diffusion), methods, and key findings.

---

## Tools

**R**  
- tidyverse  
- igraph  
- sentimentr  
- ggplot2  

---

## Notes on Data

All Reddit data used in this project were collected using the Communalytic platform (https://communalytic.org/).  
Due to ethical and platform restrictions, the full dataset cannot be shared.  

A small synthetic sample ([sample data.csv](data/sample_data.csv)) is included only to demonstrate formatting.
