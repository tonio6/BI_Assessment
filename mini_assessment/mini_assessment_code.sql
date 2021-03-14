with cities as(
    select city, count(distinct order_id) as num_orders
    from `bi-2019-test.ad_hoc.orders_jan2021`
    group by city
),
breakfast_orders as(
    select o.city, count(distinct order_id) as num_breakfast_orders
    from `bi-2019-test.ad_hoc.orders_jan2021` o 
    join cities c on c.city = o.city
    where c.num_orders > 500 and o.cuisine_parent = 'Breakfast'
    group by o.city
),
breakfast_users as(
    select o.city, count(distinct user_id) as num_breakfast_users
    from `bi-2019-test.ad_hoc.orders_jan2021` o 
    join cities c on c.city = o.city
    where c.num_orders > 500 and o.cuisine_parent = 'Breakfast'
    group by o.city
),
basket as(
    select o.city, round(avg(o.basket),2) as avg_breakfast_user_basket
    from `bi-2019-test.ad_hoc.orders_jan2021` o 
    join cities c on c.city = o.city
    where c.num_orders > 500 and o.user_id in (select user_id from `bi-2019-test.ad_hoc.orders_jan2021` where cuisine_parent = 'Breakfast')
    group by o.city
),
findings as (
    select c.*, bo.num_breakfast_orders, bu.num_breakfast_users, b.avg_breakfast_user_basket 
    from cities c
    join breakfast_orders bo on bo.city = c.city
    join breakfast_users bu on bu.city = c.city
    join basket b on b.city = c.city
    order by bo.num_breakfast_orders desc
)
select * from findings
limit 10
