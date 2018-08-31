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
  print(paste("doc", i, "of", length(ids)))
  id = ids[i]
  fp = paste("/Users/mgavin/Documents/MLH/EEBO-XML/EEBO-TCP XML/", id, ".xml", sep = "")
  txt = cleanup(fp)
  txt = txt[txt %in% vocab]
  end = length(txt) - 1
  if (end > 2) {
    for (j in 1:end) {
      word = txt[j]
      next_word = txt[j+1]
      markov[word,next_word] = markov[word,next_word] + 1
    } 
  }
}

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

get_context = function(x) {
  x = sapply(hits, FUN = function(x) return(seq(x-5, x+5)))
  x = c(x)
  return(x)
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



