Preventive Maintenance for Marine Engines: Data-Driven Insights

## Introduction

Marine engine failures can result in costly downtime, safety risks, and operational inefficiencies. This project leverages machine learning to predict maintenance needs, enabling ship operators to prevent unexpected breakdowns. Using a simulated dataset, we analyze key engine parameters and develop predictive models to classify maintenance status into three categories:

Normal

Requires Maintenance

Critical

## Project Overview

This project explores preventive maintenance strategies for marine engines by analyzing operational data and applying machine learning techniques. The goal is to build a predictive maintenance model that provides actionable insights for ship operators.

## Dataset

The dataset includes simulated sensor readings from marine engines, capturing critical operational parameters such as:

- The opearting Temperature of the Engine

- Lubricating Oil pressure mesurem

- Amount of fuel consumed on weekly basis

- Tracks of abnormal vibration

- The speed of the engine

- The Percentage at which the Engine is operting

- The temperature of Engine coolant

- The temperature of the Engine's Gases being emitted 

and more...

Each record is labeled with a maintenance status, allowing us to train and evaluate predictive models.

## Methodology

- Exploratory Data Analysis (EDA):

- Visualize engine parameter distributions

- Identify correlations between features

- Detect anomalies and trends


## Machine Learning Models:

We are using R's tidymodels framework for modeling.

We are experimenting with different models, including:

- Tree-based models (Random Forest, XGBoost)

- Support Vector Machines (SVM)

- k-Nearest Neighbors (KNN)

And more

- Model evaluation using accuracy, auc , precision, recall, and F1-score

## Deployment & Insights:

- Implement model interpretability techniques

- Provide actionable insights for maintenance 







