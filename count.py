text = """
Data Preparation and Variable Selection 

The CitiBike ride activity data for October 2019 was aggregated at the census tract level. Key variables were drawn from the NYC 2020 Census Data. The dependent variables for the regression models were derived from CitiBike ride counts, segmented by: 

Weekday and weekend ride activity: Total rides activity on weekdays and weekends. 

Peak and off-peak periods: Rides activity during peak hours (between 7-9am and 5-7pm) and off-peak hours. 

Overall ride activity: Total ride activity. 

Independent variables included demographic factors (e.g., population by age group, family count, average household size), spatial factors (e.g., number of CitiBike stations, total housing units, residential and commercial land use), and temporal factors (e.g., average peak ride ratio and weekday ride ratio. 

Spatial Weight Matrix Construction 

To account for spatial dependence, a spatial weight matrix was constructed based on census tract boundaries. The adjacency-based weight matrix used a row-standardized style, ensuring that neighboring tracts had proportional influence. An additional neighbor list was generated using centroids of census tracts within a distance threshold of 1.6 units, excluding self-neighbors. 

Model Selection 

The selection between the Spatial Autoregressive Model (SAR) and Spatial Error Model (SEM) was guided by both the Akaike Information Criterion (AIC) and insights from the Exploratory Spatial Data Analysis (ESDA). The ESDA revealed strong spatial autocorrelation in CitiBike usage across January, July, and October 2019, with significant Moran’s I values (0.445–0.478, \(p < 2.2e{-16}\)) and persistent clustering in lower Manhattan and Brooklyn. While SEM performed slightly better in most cases based on AIC, the SAR model was prioritized for interpretation due to its ability to capture spatial lag effects, aligning with the observed clustering patterns and spatial dependencies identified in the ESDA. 

Model Specification 

The following models were estimated: 

Weekday and weekend models: Separate regression models for weekday ride activity and weekend ride activity to explore differences in spatial and demographic influences on ride activity. 

Peak and off-peak models: Four regression models (weekday peak ride activity, weekday off peak ride activity, weekend peak ride activity, weekend off-peak ride activity) were used to identify key factors influencing CitiBike ride activity during different time periods. 

Total and filtered data models: total ride activity model included average peak ride ratio to assess the impact of peak period rides on overall activity. Additionally, to evaluate the effect of newly added stations in October 2019, the dataset was filtered to exclude census tracts without full ride records, and regression results were compared before and after filtering. 

Results 

Model Comparision: SAR vs. SEM 

Across the 8 models, SAR and SEM captured similar spatial characteristics, as indicated by comparable AIC values. However, SAR had a lower AIC in 2 models, while SEM performed better in 5 models. One model showed no difference in AIC. The small AIC differences suggest that both models effectively captured spatial dependencies. 

Weekday vs. Weekend SAR Models 


 

Key findings: 

The weekend SAR model outperformed the weekday SAR model, with a lower AIC and residual variance. This suggests that weekend ride activity exhibits stronger spatial clustering and is better explained by the selected variables. 

Pop19t64 (population aged 19–64) had a significant negative effect on both weekday and weekend rides, but weekend rides were also negatively influenced by PopU18 (population under 18) and Pop65pl (population aged 65+). This indicates that weekend rides are more sensitive to population age structure. 

The number of CitiBike stations had a significant positive effect on weekday ride activity but was not significant for weekend ride activity, reflecting the weekday reliance on CitiBike for commuting. 

 

Peak vs. Off-Peak SAR Models 

Among the four peak and off-peak models, the weekend peak model had the lowest AIC (6332.2) and the best fit, with residual variance (125,190) far lower than the weekday peak model (6549,400). This indicates that weekend peak rides are more predictable and exhibit stronger spatial clustering. 

Role of Peak Ride Activity Ratio 

In the total ride activity model, average peak ride activity ratio had a significant positive effect, indicating that a higher proportion of peak period rides promotes overall ride activity. This effect was more pronounced in the filtered dataset, where the coefficient for peak_ride_ratio increased from 7115 to 16,248. 

Impact of Filtering 




Key findings: 

Filtering improved model fit, as evidenced by a lower AIC and higher Log-likelihood. This suggests that excluding incomplete data enhances the reliability of results and indicates that the usage of newly added stations was relatively unstable. 

After filtering, peak ride activity ratio became more significant, with its effect nearly doubling. Conversely, the influence of weekday ride activity ratio weakened, highlighting the importance of peak-period rides in explaining total activity. 

Family count also became significant in the filtered model, suggesting that family demographics play a more important role when data quality improves. 

General Observations 

Across all weekday models, average peak ride activity ratio had a significant positive effect, reinforcing the importance of peak-period rides in driving weekday activity. 

Among demographic variables, Pop19t64 consistantly showed a significant negative effect, indicating that areas with higher working-age populations may rely less on CitiBike for leisure or non-commuting purposes. 

The number of CitiBike stations was a strong positive predictor for weekday rides but not significant for weekend rides, reflecting the system’s system’s role in weekday commuting. 
"""
### Count the number of words in the text
# Split the text into words
words = text.split()
# Count the number of words
num_words = len(words)
print(num_words)