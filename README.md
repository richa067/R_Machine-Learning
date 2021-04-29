# Project_Big_Mart_R_Machine-Learning

I researched about applications of AI and ML in various industry verticals. Out of all the sectors such as HealthCare, Marketing, Education, Finance, Retail, etc., we chose the Retail industry considering the fair amount of understanding and exposure to this field in our daily lives.We often experience that the same item is being sold at different prices in different stores and that the type, location and size of a store impact the sales.By analysing the sales data for different stores, we intend to identify the sales fluctuations.  We aim to understand the impact of several variables on sales and develop a framework to solve the most common business problem faced by retail industry of how to improve sales. We intend to leverage this learning and apply this in real world business decisions.

I decided to use BigMart dataset consists of sales data for 1559 items in different stores across different cities for year 2013. By using big mart data, we intent to analyze the sales and its fluctuation for different stores.I have used linear regression, knn, neural nets & regression trees to find out which attributes were significant in contributing to the sales, how much a store planogram plays an importsnt role for sales of each item catergor & which store has the highest sales.

What does the data contain?
•	This dataset contains sales details for 1559 products across 10 different stores in different cities.
•	Dimensions of the dataset: Rows - 14205, Columns - 12 
•	Columns description:
•	Item Identifier: Unique identification code for the sold item
•	Item_Weight: Weight of the item
•	Item_Fat_Content: Whether item has low fat or regular fat content
•	Item_Visibility: Visibility in terms of percentage of total display area of the item
•	Item_Type: Category of the item
•	Item_MRP: Maximum retail price of the item
•	Outlet_Identifier: Unique identification code for the outlet
•	Outlet_Establishment_Year: The year outlet was established
•	Outlet_Size: Ground area covered by the outlet
•	Outlet_Location_Type: Type of city where outlet is located
•	Outlet_Type: Whether outlet is supermarket or grocery outlet
•	Item_Outlet_Sales: Total sales of the item at the outlet

Conclusion & Recommendation from the analysis:

• Item MRP and other Outlet characteristics (like Size, location, and Type) are needed to predict the sales of all Items in different stores. ANN Model should be deployed as it is giving the best accuracy
• Tier 3 cities have both Supermarket Type1 and Grocery store. Supermarket Type 1 is ahead of Grocery Stores in terms of total Sales. Therefore, to attract more customers to Grocery Stores,
they should stock more varieties in each Item category
• In Tier 1 cities the dataset has both Grocery and Supermarket Type 1 stores. It is being observed that Grocery stores is keeping low MRP for certain common products being sold at both the stores. They can think of maintaining competitive price for those products as MRP is positively related to Sales
• Supermarket type 3 (Outlet 27) has shown better sales as compared to any other stores. Big Mart should consider opening more SuperMarket type 3 in other cities as well
• Food Type category has the highest sales amongst all other categories. All the stores should have adequate and quality stock of all these items
