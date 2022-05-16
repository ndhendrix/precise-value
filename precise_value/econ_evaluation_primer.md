---
title: "Economic Evaluation Primer"
output: html_document
---


## Economic Evaluation

* Uses a mathematical model to compare two or more alternative courses of action in terms of both their costs and effectiveness. 

## Value

* Trade-off between costs and effectiveness (health outcomes) when comparing multiple health interventions. 

<center>

![](value_figure.png)

</center>

## Measures of Effectiveness (Health Outcomes)

* Quality-adjusted life years (QALYs) combines:
    * Quantity of life (survival) 
    * Quality of life 
* Number of adverse events
* Others possible 

## Results of Economic Evaluation


```
## Error in library(kableExtra): there is no package called 'kableExtra'
```

```
## Error in kable_styling(.): could not find function "kable_styling"
```

## Costs

* Total costs of each intervention 
* Incremental costs comparing
  * Inc.costs = Cost of Intervention A - Cost of Intervention B
	
## Effectiveness 

* Total QALYs of each intervention.
* Incremental effectiveness 
  * Inc. QALYs = QALYs of Intervention A - QALYs of Intervention B

## Incremental costs and effectiveness ratio (ICER)

* ICER = (Cost of Intervention A - Cost of Intervention B)/(QALYs of Intervention A - QALYs of Intervention B)

# Decision Made after Economic Evaluation:

* What is your willingness to pay (WTP) for one unit increase in effectiveness? 
* US WTP between \$ 50K and \$ 150K per QALY gained.
* How does the estimated ICER compare to your WTP?
  * If ICER is lower than WTP, then consider implementing the tested intervention. Provides good value for the investment.
  * If ICER is higher than WTP, then consider implementing the alternative intervention.