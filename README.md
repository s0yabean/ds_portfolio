# Data Science Portfolio
A collection of my data science projects, code snippets and documentation.

### CNN_Classifier_YOGA_Pose.ipynb
Last Updated: May 2019 <br>
Language: Python <br>
Main Libraries: Keras, numpy <br>
One of my first starter projects into deep learning and image recognition, I created a yoga classifier that takes in an image, and predicts which of 3 poses the person is in (garland, downward dog, reverse warrior).

Using a pretrained VGG model, I was able to achieve ~90% accuracy on a validation set using between 200-300 images for each class.

### Churn_Prediction_Ensemble_Model.r 
Last Updated: May 2018 <br>
Language: R <br>
Main Libraries: Caret, dplyr <br>
Description: A paid project for a subscription-based app, predicting the likeliest customers who are about to churn based on in-app data points like logins, app interactions and user history.

I went through a couple of popular ML algorithms for this binary classification problem to find baseline performance, also trying out an ensemble method to see if performance improves.

For single algorithm, having boosted classification trees gave the best performane - tree-based algorithms seem to work well for this problem type.

During the ensembling, there was severe overfitting on the training data, as the training accuracy went up to 99% but testing accuracy remained similar to baseline. 

This suggests that the main bottleneck is not from algorithms, perhaps we need to focus on getting more/ better data instead.



