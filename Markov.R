setwd("D:/CodeProject/R/Groupwork1/Statistical-programming") ## comment out of submitted 
a <- scan("4300-0.txt",what="character",skip=73,nlines=32858-73) 
a <- gsub("_(","",a,fixed=TRUE) ## remove "_("

# Question 4/5.the punctuation mark, remove it from the word, and add the mark as a new entry in 
# the vector of words

split_punct <- function(words, punctuation_mark) {
 
  # To find punctuations ,.:;-!？
  ipunct <- grep(punctuation_mark,words,fixed=TRUE) ## find words with ‘,’ 
 
  # create a vector to store
  result_vec <- rep("", length(ipunct) + length(words))
  
  # the puctuation "," goes to
  new_ip <- ipunct + 1:length(ipunct)
  
  # add the punctuations in the new vector 
  result_vec[new_ip] <- punctuation_mark
  
  # add the words without punctuation marks into the new vector 
  result_vec[-new_ip] <- gsub(punctuation_mark,"",words, fixed = TRUE)
  
  return(result_vec)
}

# Test Examples

input_vector <- a
result_vec <- split_punct(input_vector,",")
result_vec <- split_punct(result_vec,".")
result_vec <- split_punct(result_vec, "?")
result_vec <- split_punct(result_vec, "!")
result_vec <- split_punct(result_vec, ":")
result_vec <- split_punct(result_vec, ";")
# print(result_vec)

# 6(a) find the vector of unique words.
unique_words <- unique(tolower(result_vec))

# 6(b) find the indices indicating the unique words
indices <- match(tolower(result_vec),unique_words)

# 6(c) count up how many each word occurs
frequency <- tabulate(indices)

# 6(d) decide a threshold number to find the most used words
frequency_sorted <- sort(frequency, decreasing = TRUE)

# after several tests, we notice that the most used words with frequency 27 are in the list 1:1005
# (the 1006th word is with frequency 26). So we choose the threshold 26 is the frequency of the decreasing 
# sorted 1005th.

threshold = frequency_sorted[1005]

# 6(e) find the most 1005 commonly used words

b_indices <- which(frequency > threshold - 1) 

# b stands for all the 1005 common words but without sorting
b = unique_words[b_indices]

# 7(a) create a vector 
d <- match(tolower(result_vec),b)

general_common_word <- cbind(1:1005,b)


# 7(b) three columns matrix with index of words
d1 <- d
d2 <- c(d[2:length(d)],NA)
d3 <- c(d2[2:length(d2)],NA)

T <- cbind(d1,d2,d3)

# 7(c) identify the common word triplets and drop the other word triplets
T_common <- T[is.na(rowSums(T)) == FALSE,]

# 7(d) Two column common word pairs matrix
P <- cbind(d1,d2)
P_common <- P[is.na(rowSums(P)) == FALSE,]

# 8(a)(b) Extract the sub-matrix from T whose rows all have k[i] and k[j]
# 8(c) Select the 3rd column at random

# k: generated text
k = character(50)
k[1] = sample(general_common_word[,1], 1)

# submatrix of P based on k1
submatrix_of_k1 = P_common[which(P_common[,1] == k[1]),]

# judge if the submatrix is empty
# if (dim(submatrix_of_k1)[1] == 0 && dim(submatrix_of_k1)[2] == 0){
if (length(submatrix_of_k1[, 2]) == 0){
  # if it is empty,k[2] is chosen randomly from b
  k[2] = sample(general_common_word[,1], 1)
  # print("i am here!")
}else{
  # if it is non-empty, choose k[2] from submatrix of P_common
  k[2] = general_common_word[,1][sample(submatrix_of_k1[,2], size = 1)]
   # print("hello i am here!!")
}

for (i in 3:50){

  #submatrix of T
  submatrix_of_T = T_common[which(T_common[,1] == k[i-2] & T_common[,2] == k[i-1]),,drop = FALSE]
  
  #submatrix of P
  submatrix_of_P = P_common[which(P_common[,1] == k[i-1]),,drop = FALSE]
  
  # if the subT is empty
  if (length(submatrix_of_T[, 2]) == 0){
  
    # if subT is empty and the subP is empty
    if (length(submatrix_of_P[, 2]) == 0){
      k[i] <- sample(general_common_word[,1], size = 1)
      # print("hello i am here!")
    }else{ # subT is empty, subP nonempty
      k[i] <- sample(submatrix_of_P[,2], size = 1)
      # print("hello i am here!!!!")
    }
  }else{# subT is nonempty
    k[i] <- sample(submatrix_of_T[, 3], size = 1)
    # print("hello i am here!!!!!!!!!")
  }
} 

# transform the type of k and print out the new text 
k <- as.numeric(k)

cat(general_common_word[,2][k])

# Question 9. simulate 50 word sections of text

freq_b <- c()

# compute the frequency of b elements occuring in a 
lower_result = tolower(result_vec)

for (i in 1:length(b)){
  freq_b[i] <- sum(lower_result == b[i])
}

prob_b <- freq_b/sum(table(result_vec))
comp_b = sample(b,50, prob = prob_b)

cat(comp_b)

# Question 10

# select all the words starting with capital letter out
upper_word_indices <- grep("[A-Z]", result_vec)

# find all of upper characters in the result vector
upper_characters <- result_vec[upper_word_indices] # upper_chars

# use match() function to find the position where the result vector element are located in upper characters
match_i <- match(result_vec,upper_characters) 

# upper letter counts
ucounts <- tabulate(match_i) 

# find the most uses 200 common uppercase words
sorted_upper_counts <- rev(order(ucounts))[1:200]
common_upper_words <- upper_characters[sorted_upper_counts]

# the text we have in quation 8
final_text <- c(general_common_word[,2][k])

# substitute the most used 200 uppercase words into our text
general_text_indices <- match(final_text,tolower(common_upper_words))
general_text_indices <- common_upper_words[general_text_indices]
NA_indices <- which(is.na(general_text_indices))
general_text_indices[NA_indices] <- final_text[NA_indices]
cat(general_text_indices)
