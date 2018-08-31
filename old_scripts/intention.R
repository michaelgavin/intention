################   A MATHEMATICAL THEORY OF AUTHORIAL INTENTION   #############

# This document records the script used for the essay, A Mathematical Theory of
# Intention. In the original analysis, pieces of this script were run discontin-
# ously. They are here assembled into a unified whole. 

################   FUNCTIONS   ################################################
# Cosine distance
semantic_distance = function(x, y) {
  x[is.na(x)] = 0
  y[is.na(y)] = 0
  sim = sqrt(x^2) %*% sqrt(y^2) / (sqrt(x %*% x) * sqrt(y %*% y))
  return(1 - sim)
}


# Conceptual work
conceptual_work = function(freq, cos_dist) {
  return(freq * cos_dist)
}

# Semantic transparency
semantic_transparency = function(work) {
  work = work[!is.na(work)]
  work = work[work > 0]
  n = length(work)
  p = work / sum(work)
  st = -1 * (p %*% log(p, base = n))
  return(st)
}


# Set threshold
set_threshold = function(avgs, devs, mu = NULL) {
  x = 1:length(avgs)
  fita = lm(avgs~log(x))
  anew = predict(fita,newdata=list(x=x),interval="confidence")
  fitd = lm(devs~log(x))
  dnew = predict(fitd,newdata=list(x=x),interval="confidence")
  delta = anew[,1]
  psi = dnew[,1]
  if (any(mu) == F) {
    mu = (1-delta[1]) / psi[1]
  }
  theta = delta + mu * psi
  res = list()
  res$mu = mu
  res$avg_alpha = coefficients(fita)[2]
  res$avg_beta = coefficients(fita)[1]
  res$psi_alpha = coefficients(fitd)[2]
  res$psi_beta = coefficients(fitd)[1]
  res$delta = delta
  res$psi = psi
  res$theta = theta
  return(res)
}

### Adjust for corpus norms ###
conceptual_work_adj = function(freq, cos_dist, threshold) {
  hits = which(freq > 0)
  threshold_scores = rep(0, length(freq))
  threshold_scores[hits] = threshold[freq[hits]]
  adj_cos_dist = cos_dist - threshold_scores
  adj_cos_dist[is.na(adj_cos_dist)] = 0
  wk = freq * adj_cos_dist
  return(wk)
}


#### Section 2. Locke, Behn, & Temporality ####
load("C:/Users/mgavin/Desktop/intention/averages.rda")
load("C:/Users/mgavin/Desktop/intention/deviations.rda")
load("C:/Users/mgavin/Desktop/intention/deebo.rda")
load("C:/Users/mgavin/Desktop/intention/distances.rda")
load("/Users/mgavin/Desktop/EEBO-RDA/A48901.rda")


res = set_threshold(avgs = averages, devs = deviations)

d = distances[,"A48901"]
f = deebo[,"A48901"]

wk = conceptual_work(freq = f, cos_dist = d)
wk_adj = conceptual_work_adj(freq = f, cos_dist = d, threshold = res$theta)

# Find most similar to Locke
vec = similarity(deebo, "A48901", margin = 2, fullResults = T)
ids = names(sort(vec, decreasing = T))[1:100]

# Load compiled subcorpus of similar docs
load("C:/Users/mgavin/Desktop/locke/locke_mat.rda")

# Check semantic work of local context
dists = c()
for (i in 1:nrow(m)) {
  print(i)
  dists = c(dists, semantic_distance(locke_mat[i,],eebo[i,]))
}
names(dists) = rownames(m)
freqs = rowSums(deebo)
lc_wk = freqs * dists

# Compare Locke to local context
d_lc = c()
for (i in 1:nrow(m)) {
  print(i)
  d_lc = c(d_lc, semantic_distance(locke_mat[i,],m[i,]))
}
names(d_lc) = rownames(m)

# Triangulate between local and primary contexts
triangulated_d = d - (d_lc + dists)

# Triangulated work
twk = triangulated_d * f

# Remove effect of local context
r_d = d - res$mu * dists
rwk = r_d * f

#### Behn ####
load("C:/Users/mgavin/Desktop/intention/deebo.rda")
load("C:/Users/mgavin/Desktop/intention/distances.rda")
load("C:/Users/mgavin/Desktop/intention/behn/behn_mat.rda")
load("C:/Users/mgavin/Desktop/intention/behn/behn_ids.rda")
load("/Users/mgavin/Desktop/EEBO-RDA/A27301.rda")

f = rowSums(deebo[,ids])

d = c()
for (i in 1:nrow(m)) {
  print(i)
  d = c(d, semantic_distance(behn_mat[i,],eebo[i,]))
}
names(d) = rownames(m)

wk = conceptual_work(f, d)
awk = conceptual_work_adj(f, d, res$theta)

dll = c()
for (i in 1:nrow(m)) {
  print(i)
  dll = c(dll, semantic_distance(behn_mat[i,],m[i,]))
}
names(dll) = rownames(m)

wk_dll = conceptual_work(deebo[,"A27301"], dll)

dlle = c()
for (i in 1:nrow(m)) {
  print(i)
  dlle = c(dlle, semantic_distance(eebo[i,],m[i,]))
}
names(dlle) = rownames(m)

rwk = deebo[,"A27301"] * (dlle - (d + dll))


# Now look at transparencies
transparencies = c()
for (i in 1:ncol(matb)){
  print(i)
  transparencies = c(transparencies, semantic_transparency(work = matb[,i]))
}
for (i in 1:ncol(matf)){
  print(i)
  transparencies = c(transparencies, semantic_transparency(work = matf[,i]))
}
for (i in 1:ncol(matl)){
  print(i)
  transparencies = c(transparencies, semantic_transparency(work = matl[,i]))
}
names(transparencies) = c(behn_ids, fiction_ids, locke_ids)
grps = c(rep(1,length(behn_ids)), rep(2, length(fiction_ids)), rep(3, length(locke_ids)))

# Now look at temporality

################   KEY METRICS   ##############################################

# Average deviance by document frequency

# Standard deviation of deviance by document frequency

load("distances.rda")
load("deebo.rda")

# Output of this is dataframe of metrics
ids = colnames(deebo)
work_per_word = c()
transparency = c()
frequency = c()
types = c()
for (i in 1:length(ids)) {
  print(i)
  id = ids[i]
  freqs = deebo[,id]
  w = work[,id]
  work_per_word = c(work_per_word, sum(w, na.rm = T) / sum(freqs))
  transparency = c(transparency, semantic_transparency(w))
  frequency = c(frequency, sum(freqs))
  types = c(types, length(w[is.finite(w)]))
}
df = data.frame(work_per_word, transparency, frequency, types)
rownames(df) = ids


# Now need freq averages and deviations
hits = which(deebo > 0)
frequencies = deebo[hits]
dist_vec = distances[hits]

n = 1:max(frequencies)
averages = c()
deviations = c()

for (i in 1:length(n)) {
  print(paste(i, "of", length(n)))
  ord = which(frequencies == i)
  averages = c(averages, mean(dist_vec[ord], na.rm = T))
  deviations = c(deviations, sd(dist_vec[ord], na.rm = T))
}
save(averages, file = "averages.rda")
save(deviations, file = "deviations.rda")

n.averages = c()
counter = 0
for (i in 1:length(averages)) {
  print(i)
  if (i < 75) {
    n.averages[i] = mean(averages[i])
  } else {
    counter = counter + 1
    n.averages[i] = mean(averages[(i - counter):(i + counter)], na.rm=T)
  }
}

n.deviations = c()
counter = 0
for (i in 1:length(deviations)) {
  print(i)
  if (i < 50) {
    n.deviations[i] = mean(deviations[i])
  } else {
    counter = counter + 1
    n.deviations[i] = mean(deviations[(i - counter):(i + counter)], na.rm=T)
  }
}

beta = set_beta(avgs = n.averages, devs = n.deviations)

threshold = set_threshold(avgs = n.averages, devs = n.deviations, beta=beta)

for (i in 1:ncol(distances)) {
  print(i)
  d = semantic_distance_adj(freq = deebo[,i], cos_dist = distances[,i], threshold = n.averages)
  w = conceptual_work_adj(freq = deebo[,i], cos_dist_adj = d)
  distances[,i] = w
}

#### Convert dist_mat to adjusted work mat ####
for (i in 1:nrow(distances)) {
  print(i)
  acd = semantic_distance_adj(deebo[i,], cos_dist = distances[i,], threshold = threshold)
  w = conceptual_work_adj(deebo[i,], acd)
  w[is.na(w)] = 0
  distances[i,] = w
}


################   HOW THE DATASETS WERE BUILT   ##############################

# Process texts for initial term document matrix (aka 'deebo')
library(XML)
setwd("/home/mgavin/r_tests")

## Note that vocab, keywords, and ids are inherited from previous research
load("vocab.rda")
load("keywords.rda")
load("ids.rda")

xml_loc = "/home/mgavin/EEBO-TCP XML/"
mat_loc = "/home/mgavin/EEBO-RDA/"

filenames = paste(xml_loc, ids, ".xml", sep="")

## Initialize deebo
deebo = matrix(0, length(vocab), length(ids))
rownames(deebo) = vocab
colnames(deebo) = ids

## Begin for loop
for (i in 1:length(ids)) {
  print(i)
  fp = filenames[i]
  id = ids[i]
  parsedText = htmlTreeParse(fp, useInternalNodes = TRUE)
  nodes = getNodeSet(parsedText, "//text")
  if (length(nodes) > 1) nodes = nodes[1]
  txt = lapply(nodes, xmlValue)
  txt = gsub("non-Latin alphabet", " ", txt)
  txt = gsub("1 page duplicate", " ", txt)
  txt = gsub("Å¿", "s", txt)
  txt = gsub("[0-9]", "", txt)
  txt = gsub("vv", "w", txt)
  txt = gsub("'d ", "ed ", txt)
  txt = gsub("'ring ", "ering ", txt)
  txt = strsplit(txt, "\\W")
  txt = unlist(txt)
  txt = tolower(txt)
  txt = txt[txt %in% vocab]
  freqs = table(txt)
  words = names(freqs)
  if (i == 1) {
    deebo[words,i] = as.integer(freqs) 
  } else {
    deebo = cbind(deebo, rep(0, nrow(deebo)))
    colnames(deebo)[ncol(deebo)] = id
    deebo[words,i] = as.integer(freqs)
  }
}

save(deebo, file = "deebo.rda")

################   BUILD DOC MATRICES  ########################################

# Now build word-context matrix for each document
m = matrix(0, length(vocab), length(keywords))
rownames(m) = vocab
colnames(m) = keywords


## Now define functions
cleanup = function (filepath, stopwords = c(), normalize = TRUE) {
  if (length(grep(".txt", filepath)) == 1) {
    text = scan(filepath, what = "character", sep = "\n", 
                fileEncoding = "UTF-8")
    text = paste(text, collapse = " ")
  }
  else if (length(grep(".xml", filepath)) == 1) {
    parsedText = htmlTreeParse(filepath, useInternalNodes = TRUE)
    nodes = getNodeSet(parsedText, "//text")
    text = lapply(nodes, xmlValue)
  }
  text = gsub("non-Latin alphabet", " ", text)
  text = gsub("1 page duplicate", " ", text)
  if (normalize == TRUE) {
    text = gsub("Å¿", "s", text)
    text = gsub("[0-9]", "", text)
    text = gsub("vv", "w", text)
    text = gsub("'d ", "ed ", text)
    text = gsub("'ring ", "ering ", text)
  }
  text = strsplit(text, "\\W")
  text = unlist(text)
  text = text[text != ""]
  if (normalize == TRUE) {
    text = tolower(text)
    text = text[text %in% stopwords == FALSE]
    if (any(grep("[^ -~]", text))) 
      text = text[-grep("[^ -~]", text)]
  }
  return(text)
}

get_context = function(x) {
  x = sapply(hits, FUN = function(x) return(seq(x-5, x+5)))
  x = c(x)
  return(x)
}

# Begin for loop
for (i in 1:length(ids)) {
  print(i)
  id = ids[i]
  filepath = paste(xml_loc, id, ".xml", sep = "")
  
  m[which(m > 0)] = 0
  
  txt = cleanup(filepath)
  txt = txt[txt %in% vocab]
  
  kws = keywords[which(keywords %in% txt)]
  if (length(kws) > 0) {
    for(j in 1:length(kws)) {
      keyword = kws[j]
      hits = which(txt == keyword)
      contexts = get_context(hits)
      contexts = contexts[which(contexts > 0)]
      contexts = contexts[which(contexts <= length(txt))]
      freqs = table(txt[contexts])
      
      words = names(freqs)
      m[words,keyword] = freqs
    }  
  }
  save(m, file = paste(mat_loc, id, ".rda", sep=""))
}


################   NOW COMPILE INTO SINGLE WORD-CONTEXT MATRIX   ##############


eebo = matrix(0, length(vocab), length(keywords))
rownames(eebo) = vocab
colnames(eebo) = keywords

filenames = paste(mat_loc, ids, ".rda", sep="")

for (i in 1:length(filenames)) {
  print(i)
  load(filenames[i])
  eebo = eebo + m
}
save(eebo, file = "eebo.rda")


################   NOW GET SEMANTIC DISTANCES FOR EACH WORD   #################
setwd("/home/mgavin/r_tests")

load("ids.rda")
load("eebo.rda")

xml_loc = "/home/mgavin/EEBO-TCP XML/"
mat_loc = "/home/mgavin/EEBO-RDA/"

filenames = paste(mat_loc, ids, ".rda", sep="")

cos_sim = function(x,y) {
  return((x %*% y) / (sqrt(x %*% x) * sqrt(y %*% y)))
}

distances = matrix(0, nrow(eebo), length(ids))
colnames(distances) = ids
rownames(distances) = rownames(eebo)

for (i in 1:length(filenames)) {
  print(i)
  id = ids[i]
  load(filenames[i])
  vec = c()
  for (j in 1:nrow(m)) {
    distance = 1 - cos_sim(m[j,], eebo[j,])
    vec = c(vec, distance)
  }
  distances[,i] = vec
}

save(distances, file = "../r_tests/distances.rda")

#### How the Markov-chain Text was compiled  ####
# Build simulated Locke
ids = colnames(deebo)
data(eebo)
ids = sample(ids, 5000)

totals = rowSums(eebo)
totals = sort(totals, decreasing = T)[1:10000]
vocab = names(totals)

markov = matrix(0, length(vocab), length(vocab))
rownames(markov) = vocab
colnames(markov) = vocab


for (i in 1:length(ids)) {
  id = ids[i]
  fp = paste("/Users/mgavin/Documents/MLH/EEBO-XML/EEBO-TCP XML/", id, ".xml", sep = "")
  txt = cleanup(fp)
  txt = txt[txt %in% vocab]
  end = length(txt) - 1
  if (end > 2) {
    for (j in 1:end) {
      print(paste("Word", j, "of", length(txt), "in document", i))
      word = txt[j]
      next_word = txt[j+1]
      markov[word,next_word] = markov[word,next_word] + 1
    } 
  }
}
## Saved as markov.rda

p = markov
for (i in 1:nrow(p)) {
  p[i,] = p[i,] / sum(p[i,])
}

starting_word = "power"
sim_txt = c()
for (i in 1:50000) {
  print(i)
  vec = p[starting_word,]
  next_word = sample(vec, size = 1, prob = vec)
  next_word = names(next_word)
  sim_txt = c(sim_txt, next_word)
  starting_word = next_word
}

txt = sim_txt
markov_mat = matrix(0, nrow(eebo), ncol(eebo))
rownames(markov_mat) = rownames(eebo)
colnames(markov_mat) = colnames(eebo)
kws = colnames(eebo)[which(colnames(eebo) %in% txt)]
for(j in 1:length(kws)) {
  keyword = kws[j]
  hits = which(txt == keyword)
  contexts = get_context(hits)
  contexts = contexts[which(contexts > 0)]
  contexts = contexts[which(contexts <= length(txt))]
  freqs = table(txt[contexts])
  
  words = names(freqs)
  markov_mat[words,keyword] = freqs
}  

markov_dist = c()
for (i in 1:nrow(markov_mat)) {
  print(i)
  markov_dist = c(markov_dist, semantic_distance(markov_mat[i,], eebo[i,]))
}
names(markov_dist) = rownames(eebo)

freqs = table(sim_txt)

markov_dist = markov_dist[names(freqs)]
freqs = as.numeric(freqs)
plot(freqs, markov_dist)
