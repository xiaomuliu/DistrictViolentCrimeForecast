# District-level Violent Crime Forecast
Distribute daily citywide violent crime count predications through spatial crime density (estimated via mesh modeling). Then integrate (sum) the rates inside a police district, which give the expected number of violent crimes in that district for that day. 

Evaluation: 2013-2014 daily violent crime count predictions over 22 police district in Chicago.

*Evaluation example*  
1 .Tomorrow's forecast - Friday, Aug 12, 2016. [*red*]  
2. historical overall average number of crimes for a given District/Beat (how different is tomorrow from an average day in the past 5 years?) [*yellow*]  
3. same, but for a given time of year (how different is tomorrow from an average August day in the past 5 years?) [*green*]  
4. same, but for a given day-of-week at a given time of year (how different is tomorrow compared with an average Friday in August in the past 5 years?) [*blue*]  
5. same, but for the three most recent Fridays [*purple*]  

Median absolute percentage error
![alt text](https://github.com/xiaomuliu/DistrictViolentCrimeForecast/blob/master/MdAPE_district.png)

Mean squared error
![alt text](https://github.com/xiaomuliu/DistrictViolentCrimeForecast/blob/master/MSE_district.png)
