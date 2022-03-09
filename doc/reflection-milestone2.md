Through the development process, we were limited by our a) data, b) development languages and knowledge of them, and c) time. 

**The data**

Although the data we used was complete (no missing values), and clean (no typos, different spellings, etc), it was still limiting and time consuming. This was real data from a company, and only included 148 responses across ~20 countries. As a result, some countries had a depth of response which required country level analysis, whereas others only had a 1-5 responses for which certain graphs required for larger countries were not appropriate. Moreover, our area chart was challenging given that even for 10-20 repsonses, each response had a distinct score. While we tried other approaches to splicing this visual representation, we were held back by our ability to implement in Dash. Moving forward, we will explore other ways of splicing the data, and consider buckets for scores. Another challenge we had with the data was the proper visualization of ranked data, where 1 was the most important (the boxplot). We did not want to change the data (e.g. invert the values so that 1 = 6, 2 = 5, etc), however the current boxplots are confusing, given that the lowest value is the highest priority. Similarly, we explored the option of casting the data to ordinal or categorical, however, this would not have allowed us to calculate middle values (mean/median). We will explore other ways to represent this hierarchy in the future. 


**The language**

The next category of challenge we had was with our development languages. Our main challenge was in managing development complexity in the Dash language. While our teammate responsible for driving the layout of the dashboard was familiar with front-end dashboard development, he was new to Dash. We overestimated the complexity we could represent through our Bootstrap layout, and as such, our choice to include global features represented in our sketch was not feasible. Similarly, for the area chart, we tried to address the above data problem with a checkbox filter, so that users could choose many different countries at once. However, this resulted in an unwieldy checklist which was unattractive, hard to format, and ultimately hard to read for the user.


**Time**

Finally, we were also constrained by time, and have multiple improvements in our future development pipeline. There are future development opportunities to add a people analytics by incorporating an inferential modelling to understand which factors affect employee’s work life qualities that can aid management in improving this dimension. People analytics module can train a generative model on the data to pop up most statistically significant factors that are affecting the overall happiness. Furthermore, dash app front-end can be technically improved so that it can be used on multiple platforms such as mobile and tablet. This can improve usability of the app for management to check employee work life quality on the go.
