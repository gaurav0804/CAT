library(readr)
library(catR)

ItemBank <- as.matrix(read_csv("./ItemBank.csv"))
QuestionBank <- as.matrix(read_csv("./QuestionBank.csv"))
questions_administered <- c()
theta <- 0
response_vector <- c()
theta_list <- c()
se_list <- c()

next_question <- function(input_theta,input_out){
  next_question_number <- nextItem(ItemBank, theta = theta,out = questions_administered)
  question_number <- next_question_number$item
  return(question_number)
  
}

print_question_and_options <-function(question_number){
  question <- toString(QuestionBank[question_number,2])
  print(question)
  OptionA <- toString(QuestionBank[question_number,3])
  print(OptionA)
  OptionB <- toString(QuestionBank[question_number,4])
  print(OptionB)
  OptionC <- toString(QuestionBank[question_number,5])
  print(OptionC)
  OptionD <- toString(QuestionBank[question_number,6])
  print(OptionD)
}

check_answer <- function(question_number, given_response){
  correct_answer=toString(QuestionBank[question_number,7])
  if (correct_answer==given_response){
    return(1)
  }
  else{
    return(0)
  }
}
question_number <- 1
while (length(questions_administered)<16){
  question_number <- next_question(theta,questions_administered)
  questions_administered <- c(questions_administered,question_number)
  if (length(questions_administered)<=1){
    item_mat <- t(as.matrix(ItemBank[questions_administered,2:5]))
  } else {
    item_mat <- as.matrix(ItemBank[questions_administered,2:5])
  }
  print_question_and_options(question_number)
  given_response <- readline(prompt = "Enter your choice here: ")
  evaluated_response <- check_answer(question_number,given_response)
  response_vector <- c(response_vector,evaluated_response)
  theta <- thetaEst(item_mat,response_vector)
  theta_list <- c(theta_list,theta)
  se <- semTheta(theta,item_mat,response_vector)
  se_list <- c(se_list,se)
}
