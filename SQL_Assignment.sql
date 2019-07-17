/*Task 1: Understanding the data in hand 
	A. Describe the data in hand in your own words. (Word Limit is 500) 
	
	Answer:  
	These data files are providing the sales, profit, order management, product relation details, customer details 
    information about a Super store. If we design the relationship among 5 tables (4 dimensions and 1 fact tables), 
    we can get star schema model.  That means this data model is in denormalized form. The dimension tables customer, 
    order, product and shipping contains customer details , order details product details and shipping details information respectively.
    Finally the market fact table contains numerical fact information of the company. Based on this company’s performance can be analyzed.*/

/*	B. Identify and list the Primary Keys and Foreign Keys for this dataset (Hint: If a table don’t have Primary Key or Foreign Key, then specifically mention it in your answer.) 
	
 cust_dimen - >  Primary Key : Cust_id
orders_dimen ->Primary Key:  Ord_id
prod_dimen -> Primary Key: Prod_id
shipping_dimen  -> Primary Key: Ship_id
market_fact  -> Foreign keys : Ord_id,	Prod_id, Ship_id, Cust_id*/

/*Task 2: Basic Analysis 
Write the SQL queries for the following: */
	/*A. Find the total and the average sales (display total_sales and avg_sales) */

select sum(sales) as total_sales , avg(sales) as avg_sales 
from market_fact;
	
	/*B. Display the number of customers in each region in decreasing order of no_of_customers. 
    The result should contain columns Region, no_of_customers */

Select Region , count(cust_id) as no_of_customers
from cust_dimen 
group by Region 
order by no_of_customers desc;

	/*C. Find the region having maximum customers (display the region name and max(no_of_customers) */

select Region , count(cust_id) as max_no_of_customers  
from cust_dimen 
group by Region 
order by max_no_of_customers  desc limit 1;



	/*D. Find the number and id of products sold in decreasing order of products sold (display product id, no_of_products sold) */

select  Prod_id, sum(Order_Quantity) as no_of_products_sold 
from market_fact 
group by prod_id
 order by no_of_products_sold  desc;
	
	/*E. Find all the customers from Atlantic region who have ever purchased ‘TABLES’ and the number of tables purchased 
    (display the customer name, no_of_tables purchased) */
	
	select cust.Customer_Name, sum(mfact.Order_Quantity) as no_of_tables_purchased
	from cust_dimen cust, prod_dimen prod, market_fact mfact
	where cust.cust_id=mfact.cust_id
	and prod.prod_id=mfact.prod_id
	and cust.region='Atlantic'
	and prod.Product_Sub_Category='TABLES'
	group by cust.Customer_Name
    order by no_of_tables_purchased desc ;

/*Task 3: Advanced Analysis 
Write sql queries for the following: */
	/* A. Display the product categories in descending order of profits (display the product category wise profits i.e. product_category, profits)? */

   select prod.Product_Category, sum(mfact.Profit) as profits
    from  prod_dimen prod,  market_fact mfact
    where prod.prod_id=mfact.prod_id
    group by prod.Product_Category
    order by profits desc;

	/*B. Display the product category, product sub-category and the profit within each sub-category in three columns. */
	
select prod.Product_Category, prod.Product_Sub_Category, sum(mfact.Profit) as profits
 from    prod_dimen prod, market_fact mfact    
where prod.prod_id=mfact.prod_id
 group by prod.Product_Category, prod.Product_Sub_Category
order by prod.Product_Category  asc,profits desc;

	/*C. Where is the least profitable product subcategory shipped the most? For the least profitable product sub-category, 
    display the region-wise no_of_shipments and the profit made in each region in decreasing order of profits 
    (i.e. region, no_of_shipments, profit_in_each_region) o Note: You can hardcode the name of the least profitable product sub-category */
	 
     select  prod.Product_Sub_Category ,mfact.prod_id,sum(mfact.Profit) as profits
	    from    prod_dimen prod, market_fact mfact
	    where prod.prod_id=mfact.prod_id
        group by prod.Product_Sub_Category,mfact.prod_id
	    order by profits asc limit 1 ;  /*TABLES  - Least Profitable Product Subcategory*/
        
	select cust.region,  count(mfact.ship_id) as no_of_shipments
    	from cust_dimen cust,  market_fact mfact
where cust.cust_id=mfact.cust_id and mfact.prod_id='Prod_11'
            group by cust.region
           order by no_of_shipments desc limit 1 ; /*ONTARIO - Least profitable product subcategory shipped most to this region*/
        
	          
select distinct  cust.region, count(mfact.ship_id) as no_of_shipments, sum(mfact.Profit) as profit_in_each_region
    	from cust_dimen cust,  market_fact mfact, prod_dimen prod
where cust.cust_id=mfact.cust_id AND prod.prod_id=mfact.prod_id and prod.Product_Sub_Category='TABLES'
            group by cust.region
           order by profit_in_each_region desc; /*For the least profitable product sub-category, 
    displayinf the region-wise no_of_shipments and the profit made in each region in decreasing order of profits*/

   
