library(tidyverse)
library(igraph)
library(scales)
library(RColorBrewer)
library(sentimentr)
library(ggraph)
library(tidygraph)
library(gridExtra)
library(knitr)
library(signnet)

## Read data & construct graph ----

# Load dataset
data <- read_csv("data/reddit_mental_health.csv")

# Preprocess data
data <- data |> 
  mutate(
    date = mdy_hms(date),
    time_only = format(date, "%H:%M:%S"),
    sentiment = sentiment_by(text)$ave_sentiment,
    sign = ifelse(sentiment > 0, 1,
                  ifelse(sentiment < -0, -1, 0)))

# Build edge list (reply relationships)
edgelist <- data |> 
  filter(comment_on != "") |> 
  select(id, comment_on)

# Create directed graph
g <- graph_from_data_frame(edgelist, directed = TRUE)

# Assign node attributes
V(g)$sentiment <- data$sentiment[match(V(g)$name, data$id)]
V(g)$sign <- data$sign[match(V(g)$name, data$id)]



## Basic Network Measures ----

edge_density(g)
table(degree(g))
mean(degree(g))
diameter(g, directed = TRUE)
mean_distance(g)

# Centrality Measures

in_deg <- degree(g, mode = "in")
scaled_size <- sqrt(in_deg) * 2


# Visualization

# Color nodes by sentiment
norm_sentiment <- rescale(V(g)$sentiment, to = c(0, 1), na.rm = TRUE)
V(g)$color <- colorRampPalette(c("red", "white", "green"))(100)[
  ceiling(norm_sentiment * 99) + 1
]

plot(g,
     vertex.label = NA,
     vertex.size = scaled_size,
     edge.arrow.size = 0.05,
     main = "Network Colored by Sentiment and sized by In-Degree Centrality")


## Ego Graph of Most Connected Node

most_connected_ID <- names(which.max(degree(g)))
gUser <- make_ego_graph(g, diameter(g), nodes = most_connected_ID, mode = "all")[[1]]
dists <- distances(gUser, most_connected_ID)

# Color frames by distance
frame_colors <- c("black", "goldenrod2", "deepskyblue3", "darkorchid3", "mediumseagreen", "firebrick3")
V(gUser)$frame.color <- frame_colors[dists + 1]

# Color ego graph by sentiment
V(gUser)$sentiment <- data$sentiment[match(V(gUser)$name, data$id)]
norm_sentiment <- rescale(V(gUser)$sentiment, to = c(0, 1), na.rm = TRUE)
V(gUser)$color <- colorRampPalette(c("red", "white", "green"))(100)[
  ceiling(norm_sentiment * 99) + 1
]

plot(gUser, 
     vertex.label = dists,  
     vertex.label.color = "black",
     vertex.label.cex = .6,
     edge.color = 'gray',
     vertex.size = 8,
     edge.arrow.size = .2,
     #vertex.frame.color = V(gUser)$frame_colors,
     #vertex.frame.width = 4,
     main = paste("Geodesic Distances from", most_connected_ID, "with Node Sentiment"))



## Supportive vs. Unsupportive Clusters ----

# Positive subgraph
supportive_nodes <- V(g)[sentiment > 0]
supportive_subgraph <- induced_subgraph(g, vids = supportive_nodes)
components(supportive_subgraph)
cs <- components(supportive_subgraph)
gs.large <- induced_subgraph(supportive_subgraph, which(cs$membership == which.max(cs$csize)))
plot(gs.large,
     vertex.label = NA,
     vertex.label.cex = 0.8,
     edge.arrow.size = 0.2,
     main = "Largest Positive Sentiment Network Component")

# Legend for sentiment (only negative to neutral range)
legend_gradient <- colorRampPalette(c("green", "white"))(100)
legend_colors <- legend_gradient[c(1, 25, 50, 75, 100)]
legend(x = -2, y = 1,
       legend = c("Very positive", "", "Somewhat positive", "", "Neutral"),
       col = "black",
       pch = 21,
       pt.bg = legend_colors,
       pt.cex = 2,
       pt.lwd = 1,
       bty = "n")

# Negative/neutral subgraph
negative_nodes <- V(g)[sentiment <= 0]
negative_subgraph <- induced_subgraph(g, vids = negative_nodes)
components(negative_subgraph)
csn <- components(negative_subgraph)
gsn.large <- induced_subgraph(negative_subgraph, which(csn$membership == which.max(csn$csize)))
plot(gsn.large,
     vertex.label = NA,
     vertex.label.cex = 0.8,
     edge.arrow.size = 0.2,
     main = "Largest Neutral/Negative Sentiment Network Component")

# Legend for sentiment (only negative to neutral range)
legend_gradient <- colorRampPalette(c("red", "white"))(100)
legend_colors <- legend_gradient[c(1, 25, 50, 75, 100)]
legend(x = -2, y = 1,
       legend = c("Very negative", "", "Somewhat negative", "", "Neutral"),
       col = "black",
       pch = 21,
       pt.bg = legend_colors,
       pt.cex = 2,
       pt.lwd = 1.3,
       bty = "n")

# Structural comparison of components
supportive_metrics <- list(
  num_nodes = vcount(gs.large),
  num_edges = ecount(gs.large),
  avg_degree = round(mean(degree(gs.large)), 2),
  diameter = diameter(gs.large),
  avg_sentiment = round(mean(V(gs.large)$sentiment, na.rm = TRUE), 2)
)

nonsupportive_metrics <- list(
  num_nodes = vcount(gsn.large),
  num_edges = ecount(gsn.large),
  avg_degree = round(mean(degree(gsn.large)), 2),
  diameter = diameter(gsn.large),
  avg_sentiment = round(mean(V(gsn.large)$sentiment, na.rm = TRUE), 2)
)

comparison_table <- data.frame(
  Metric = c("Number of Nodes", "Number of Edges", "Average Degree", "Diameter", "Average Sentiment Score"),
  Supportive = unlist(supportive_metrics),
  Non_Supportive = unlist(nonsupportive_metrics)
)

print(comparison_table)



## Sentiment Flows in Triad ----

triad_census(g)

# Extract triads (subgraphs of size 3)
triad_nodes <- which(sapply(decompose(g), vcount) == 3)
triad_graphs <- decompose.graph(g)[triad_nodes]

# Extract text for each triad
triad_texts <- lapply(triad_graphs, function(triad) {
  node_ids <- V(triad)$name
  triad_df <- data[data$author %in% node_ids | data$id %in% node_ids, c("id", "author", "text", "type", "time_only")]
  return(triad_df)
})

# Compute sentiment for each submission/comment/reply
triad_sentiments <- lapply(triad_texts, function(triad_df) {
  triad_df$sentiment <- sentiment_by(triad_df$text)$ave_sentiment
  return(triad_df)
})

# Classify sentiment flow (increasing / decreasing / mixed)
triad_sentiment_flows <- lapply(triad_sentiments, function(triad_df) {
  ordered_df <- triad_df[order(triad_df$time_only), ]
  s <- ordered_df$sentiment
  s_diff <- diff(ordered_df$sentiment)
  
  flow <- if (s[1] < 0 && s[2] > 0 && s[3] > 0) {
    "increasing"
  } 
  else if (all(s_diff > 0)) {
    "increasing"
  } else if (s[1] > 0 && s[2] < 0 && s[3] < 0) {
    "decreasing"
  } else if (all(s_diff < 0)) {
    "decreasing"
  } else if (all(s_diff == 0)) {
    "no change"
  } else {
    "mixed"
  }
  
  ordered_df$sentiment_flow <- flow
  return(ordered_df)
})

table(sapply(triad_sentiment_flows, function(df) unique(df$sentiment_flow)))



## Triad Flow Visualization ----

flow_summary <- sapply(triad_sentiment_flows, function(df) unique(df$sentiment_flow))
flow_counts <- as.data.frame(table(flow_summary))

ggplot(flow_counts, aes(x = flow_summary, y = Freq, fill = flow_summary)) +
  geom_bar(stat = "identity") +
  labs(title = "Sentiment Flow Types Across Triads",
       x = "Sentiment Flow Type", y = "Number of Triads") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# Helper function to generate plots for a list of triad indices
plot_triads_by_type <- function(triad_indices, sentiment_type) {
  triad_plots <- lapply(triad_indices, function(i) {
    triad <- triad_graphs[[i]]
    sentiment_df <- triad_sentiment_flows[[i]]
    flow_label <- unique(sentiment_df$sentiment_flow)
    
    # Sort by time
    ordered_df <- sentiment_df[order(sentiment_df$time_only), ]
    
    # Assign sentiment to graph nodes
    V(triad)$sentiment <- ordered_df$sentiment[match(V(triad)$name, ordered_df$id)]
    
    # Create custom layout
    node_order <- match(V(triad)$name, ordered_df$id)
    layout_df <- data.frame(x = rank(node_order), y = 0)
    
    # Plot
    ggraph(triad, layout = "manual", x = layout_df$x, y = layout_df$y) +
      geom_edge_link() +
      geom_node_point(aes(color = sentiment), size = 6) +
      geom_node_text(aes(label = name), vjust = 1.5, size = 3) +
      scale_color_gradient2(low = "red", mid = "white", high = "green", midpoint = 0) +
      ggtitle(paste0("Triad ", i)) +
      theme_void()
  })
  
  # Show plots in grid
  grid.arrange(grobs = triad_plots, ncol = 3, top = paste("Sentiment Flow:", sentiment_type))
}

# Categorize triads by sentiment type
flow_labels <- sapply(triad_sentiment_flows, function(df) unique(df$sentiment_flow))
increasing_ids <- which(flow_labels == "increasing")
decreasing_ids <- which(flow_labels == "decreasing")
mixed_ids      <- which(flow_labels == "mixed")

# Plot each group on its own page
plot_triads_by_type(sample(increasing_ids, min(6, length(increasing_ids))), "Increasing")
plot_triads_by_type(sample(decreasing_ids, min(6, length(decreasing_ids))), "Decreasing")
plot_triads_by_type(sample(mixed_ids, min(6, length(mixed_ids))), "Mixed")



## Engagement vs. Supportiveness (sentiment) ----
engagement <- table(data$author)

avg_sentiment <- data %>%
  group_by(author) %>%
  summarise(avg_sent = mean(sentiment, na.rm = TRUE)) %>%
  filter(author %in% names(engagement))  # make sure it aligns

# Convert engagement to a data frame
engagement_df <- as.data.frame(engagement)
colnames(engagement_df) <- c("author", "engagement")

# Merge the two by author
merged_df <- merge(engagement_df, avg_sentiment, by = "author")

cor.test(merged_df$engagement, merged_df$avg_sent, method = "pearson")

# Scatterplot
ggplot(merged_df, aes(x = engagement, y = avg_sent)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  scale_x_log10() +
  labs(
    title = "User Engagement vs. Supportiveness (Log Scale)",
    x = "Engagement (Log-Transformed)",
    y = "Average Sentiment"
  ) +
  theme_minimal()



## In-Degree vs. Supportiveness (sentiment) ----
indegree_df <- data.frame(
  post_id = V(g)$name,                             
  in_degree = degree(g, mode = "in"),          
  sentiment = V(g)$sentiment                      
)

indegree_df <- indegree_df[!is.na(indegree_df$sentiment), ]
cor.test(indegree_df$in_degree, indegree_df$sentiment)

ggplot(indegree_df, aes(x = in_degree, y = sentiment)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "In-Degree Centrality vs. Sentiment of Posts",
    x = "Replies Received (In-Degree)",
    y = "Sentiment Score"
  ) +
  theme_minimal()


## Balance Theory & Structural Tension ----

# Signed edges for triads
triad_edges_signed <- lapply(triad_graphs, function(triad) {
  edges_df <- as_data_frame(triad, what = "edges")
  edges_df$from_sign <- data$sign[match(edges_df$from, data$id)]
  edges_df$to_sign <- data$sign[match(edges_df$to, data$id)]
  edges_df$sign <- edges_df$from_sign
  return(edges_df)
})

# Determine structural balance in triad
triad_balance <- lapply(triad_edges_signed, function(df) {
  signs <- na.omit(c(df$from_sign, df$to_sign))
  
  if (length(signs) < 3 || 0 %in% signs) {
    return("neutral or incomplete")
  }
  
  num_pos <- sum(signs == 1)
  num_neg <- sum(signs == -1)
  
  if (num_pos == 3 || (num_pos == 1 && num_neg == 2)) {
    return("balanced")
  } else {
    return("unbalanced")
  }
})

table(unlist(triad_balance))

triad_balance_info <- data.frame(
  triad_id = seq_along(triad_graphs),
  balance = unlist(triad_balance),
  sentiment_flow = sapply(triad_sentiment_flows, function(df) unique(df$sentiment_flow))
)

t <- table(triad_balance_info$balance, triad_balance_info$sentiment_flow)
chisq.test(t)
fisher.test(t) 

df_plot <- as.data.frame(t)

# Plot
ggplot(df_plot, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Triad Balance Type",
    y = "Count",
    fill = "Sentiment Flow",
    title = "Sentiment Flow Counts by Triad Balance Type"
  ) +
  theme_minimal()