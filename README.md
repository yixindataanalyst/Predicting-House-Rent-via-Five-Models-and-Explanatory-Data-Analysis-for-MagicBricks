# Background:

As the cost of living in urban areas continues to rise, many people struggle to find affordable housing. This was the problem that I wanted to tackle with my project. I worked with a company called MagicBricks, which provides rental listings and other real estate services in India. My goal was to build predictive models that could help MagicBricks price their rental properties more accurately, making it easier for people to find affordable housing.

# My practice:

To do this, I collected a large dataset of rental properties from various sources, including MagicBricks' own listings. I cleaned the data and selected the most important features, such as location, number of bedrooms, and amenities. I then used six different models to predict rent prices: logistic regression, logistic regression regularized via Lasso, random forest, support vector machines (SVM), multilayer perceptron (MLP), and principal component analysis (PCA).

Each model had its own strengths and weaknesses. For example, logistic regression and SVM are linear models that work well when there is a clear linear relationship between the features and the target variable (in this case, rent). Random forest and MLP, on the other hand, are nonlinear models that can capture more complex relationships between the features and rent. PCA was used to reduce the dimensionality of the data and improve the performance of the models.

After testing each model on a validation dataset, I found that they all performed well, with out-of-sample (OOS) accuracy ranging from 90% to 93%. The most important drivers of rent that I identified were location, number of bedrooms, amenities, and proximity to public transportation, among others. I used these drivers to improve the performance of the models and make more accurate predictions.

To show the potential impact of my project, I provided MagicBricks with a cost-benefit analysis. I estimated the costs of implementing the predictive models, such as data collection and model training, as well as the potential benefits, such as increased revenue from more accurate pricing. I showed that the benefits would outweigh the costs and that the models would be cost-effective for MagicBricks.

# Conclusion:

Overall, my project demonstrated that predictive models can be a powerful tool for improving the rental market. By identifying the most important drivers of rent and building accurate models, we can help people find more affordable housing and make the rental market more efficient. While there are still limitations to the models, such as the quality of the data and the complexity of the rental market, my project provides a solid foundation for further research and development in this area.

For more detail, please refer to my medium article:
[Medium Article:Predicting House Rent via Five Models and Explanatory Data Analysis for MagicBricks](https://medium.com/@yixinwang42/predicting-house-rent-via-five-models-and-explanatory-data-analysis-for-magicbricks-7f6a4a8e87c8)
