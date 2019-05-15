#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Feb  6 18:31:49 2019

@author: gaurav
"""

import pandas as pd
import numpy as np
import math
from scipy.optimize import differential_evolution
import random

ItemBank=pd.read_csv("./ItemBank.csv")

#Define class Question. It will read the question from index given and ItemBank.
#Also evaluate the response and print questions
class Question:
    def __init__(self, question_number, QuestionBank):
        self.question=QuestionBank.iloc[question_number,5]
        self.optionA=QuestionBank.iloc[question_number,6]
        self.optionB=QuestionBank.iloc[question_number,7]
        self.optionC=QuestionBank.iloc[question_number,8]
        self.optionD=QuestionBank.iloc[question_number,9]
        self.answer=QuestionBank.iloc[question_number,10]

    def print_question(self):
        print(self.question+"?")
        print("A: "+str(self.optionA))
        print("B: "+str(self.optionB))
        print("C: "+str(self.optionC))
        print("D: "+str(self.optionD))
    
    def check_answer(self,response):
        if self.answer==response:
            print('Well Done!')
            return 1
        else:
            print('Sorry wrong answer!')
            return 0

#Define class for testtaker with methods to record responses, administered questions and
#update values of theta after evry attempt
class Examinee:
    def __init__(self,name,theta):
        self.name=name
        self.theta=theta
        self.responses=[]
        self.administered_items=[]
        
    def update_examinee(self,question, response,items):
        
        self.responses.append(response)
        responses=self.responses
        self.administered_items.append(question)
        administered_items=self.administered_items
        administered_items=np.array(administered_items)
        theta=self.theta
        self.theta=estimate_theta(responses,administered_items,theta, items)
    
    
def split_parameters(ItemBank):
    items=ItemBank.iloc[:,1:5]
    return items

def calculate_p(theta,b):
    p=1/(1+math.e**(b-theta))
    return p

def calculate_p_array(theta,administered_items):
    
    b=administered_items.iloc[:,2]
   
    p=[]
    
    for i in range(len(b)):
        beta=b[b.index[i]]
        p_value=calculate_p(theta,beta)
        p.append(p_value)
    return p
    
def calculate_information(question,theta):
    b=question[2]
    p=calculate_p(theta,b)
    information=p*(1-p)
    return information
def calculate_information_array(theta,remaining_items):
    p=calculate_p_array(theta,remaining_items)
    p=np.array(p)
    information_array=p*(1-p)
    return information_array
def max_info_selector(theta,remaining_items):
    information=calculate_information_array(theta,remaining_items)
    remaining_items['information']=information
    remaining_items=remaining_items.sort_values('information',ascending=False)
    max_info_item=remaining_items.index[0]
    return max_info_item

def negativelogLik(est_theta: float, response_vector: list, administered_items: np.ndarray, ItemBank) -> float:
    
    b=ItemBank.iloc[administered_items,2]
    if len(response_vector) != administered_items.shape[0]:
        raise ValueError(
            'Response vector and administered items must have the same number of items'
        )
    # print(response_vector)
    # print(set(response_vector) - set([True, False]))
    if len(set(response_vector) - set([True, False])) > 0:
        raise ValueError('Response vector must contain only Boolean elements')

    LL = 0

    # try:
    for i in range(len(response_vector)):
        p = calculate_p(
            est_theta, b[b.index[i]]
        )

        # The original funtion is as follows, but since log(0) is undefined, a math domain error occurs
        # LL += (response_vector[i] * math.log(p)) + (
        #     (1 - response_vector[i]) * math.log(1 - p))

        if p < 0:
            print('p = ' + str(p))

        # This way, no error occurs, at the expense of some conditional checks
        if response_vector[i]:
            LL += math.log(p)
        else:
            try:
                LL += math.log(1 - p)
            except:
                print('p = ' + str(p))
                print('1 - p = ' + str(1 - p))
                print('log(1 - p) = ' + math.log(1 - p))

    return -LL

def estimate_theta(response_vector, administered_items, current_theta,ItemBank):
    b=ItemBank.iloc[:,2]
    b_min=min(b)
    b_max=max(b)
    bounds=[[b_min,b_max]]
    res = differential_evolution(negativelogLik, bounds, args=(response_vector,administered_items,ItemBank) )

    return res.x[0]     

 


some_random_number=random.randint(-5,5)
random_theta_initial=some_random_number/11
student=Examinee(input("Enter your name:"),random_theta_initial)

items=split_parameters(ItemBank)



for question_number in range(10):
    administered_items=student.administered_items
    administered_items=np.array(administered_items)
    est_theta=student.theta
    remaining_items=ItemBank.drop((administered_items))
    next_item=max_info_selector(est_theta,remaining_items)
    question=Question(next_item,ItemBank)
    question.print_question()
    selected_option=input('Enter your choice here:')
    current_response=question.check_answer(selected_option)
    student.update_examinee(next_item,current_response,ItemBank)

    
      
print(student.responses)
print(student.theta)
    