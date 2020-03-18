---
title: 'Individual Claim Reserving via Multi-state Models: \n A Case Study and Recipe'
author: "Alexandra Taggart and Rajesh Sahasrabuddhe"
date: '\today'
output: 
  pdf_document:
    number_sections: true
---



# Motivation and Introduction 

The most common actuarial models used to develop unpaid claim estimates require the analysis of claim triangles. The ubiquitous use of triangles is natural as it provides a natural visualization of the movement of a _portfolio_ of claims. As such, both actuaries and non-actuaries (i.e., stakeholders) find the claim triangle understandable and, as a result, acceptable. 

However, a claim triangle is "data-expensive." 

* We require a point-in-time claim listing to construct each diagonal of claims triangle and we require several of these point-in-time listing evaluated at evenly-spaced intervals. That is, we need to _save_ significant amounts of data.

* Then we _spend_ the data by aggregating the claim-listing by cohorts (such as accident year, report year or policy year).

* We _give away_ all the features of the claim other than its maturity. 


# Modeling Transitions

This section will focus on state tarnsitions and the minimum outcome will be probability distrubution as to how claims close.

## Defining States

Discuss how the most basic states of "open" and "closed" and how model complexity increases as we refine states

## Organizing Data

Suggest focusing on how to organize data to measure state transitions

## Visualizing a Transition Matrix

Visualizing a transition matrix
Parhaps we think about supplementing the current design with a chart of
'off-diagonal' movements.

## Selecting Transition Probabilities

Considerations for selecting transition probabilities.
Also provide the code to create a markov-chain object.

## Simulation Results

Running the simulation and understanding/visualizing the output

# Severity

The conditional severity model

# Conclusions

Conclusions and Areas for further reseacrh.
