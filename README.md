# Master-Thesis-Data-Science 

_**Disclaimer**_: Material of the Master Thesis on Public Policy (Hertie School): _**Is all Publicity Good Publicity? The Effect of Media Coverage of EU Affairs on Institutional Trust in the EU During the COVID-19 Crisis in the Netherlands.**_

**Executive Summary**

In this Master Thesis we aim to empirically invesLgate the relationship between institutional trust in the EU and media coverage of EU affairs in the Netherlands. Based on media effect theory, “virtuous circle” approaches to media consumption and theories on trust as result of a evaluative assessment of institutions, we hypothesise that (i) there is a positive relationship between higher media coverage of EU affairs and trust levels in the EU in the Netherlands; (ii) a positive relation exists between positive media coverage of the EU and trust levels in the EU in the Netherlands; and, last, that (iii) crisis at the EU level are positively associated to an increase in salience of the EU in the Dutch media environment.

To investigate these hypotheses, we examine data for the period 2008 to 2021 from the Longitudinal Internet Studies for the Social sciences (LISS) panel. Our empirical strategy consists of a **difference-in-differences (DiD) analysis** of institutional trust levels in the EU by media consumption patterns, a **text sentiment analysis** of selected headlines of articles covering EU affairs, and **linear regression to estimate** the impact of media coverage tone on institutional trust in the EU. In our main model, institutional trust in the EU is our dependent variable, while other independent variables are average sentiment in the generalised media environment and control variables of sociodemographic nature. The entire analytical dataset contains **64,777 observations** corresponding to **13,447 unique households**.

The results of our analysis shows that there is a positive relationship between consuming media and granting higher levels of trust to the EU. While we could not establish a clear causal relationship to determine the impact of media coverage of EU affairs during the COVID-19 crisis on institutional trust in the EU, we have visually established the association between the COVID-19 crisis scenario and a greater salience of the EU in the news media environment together with more positive coverage of it. Our most robust finding shows that there is a relationship between consuming media and granting higher levels of trust to the EU. This result goes in line with previously hypothesised theories, particularly with Norris’ “virtuous circle” approach for the interpretation of media impact on trust (Norris, 2000). While our results show that during the COVID-19 crisis in the Netherlands, the EU gained salience in the media as well as more positive coverage compared to the period previous to the outbreak of the pandemic, we are not able to demonstrate a causal relationship between media coverage and the COVID-19 crisis.



git checkout --orphan assets
git reset --hard
cp /Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/figure_4_thesis.png .
git add .
git commit -m 'Figure 4'
git push -u origin assets
git rev-parse HEAD
