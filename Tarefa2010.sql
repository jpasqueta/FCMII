-- Tarefa 20/10/23 - SENHA DA AULA: Cachorrinhos =)
-- Verificar o valor máximo e mínimo de cada marca de veículo vendido

SELECT 
	brand,
	MAX(price) AS maior_preco,
	MIN(price) AS menor_preco,
	COUNT(brand) AS total_vendas
FROM sales.products
GROUP BY brand
ORDER BY total_vendas DESC;

-- Tabelas das sales
/* SELECT *
FROM sales.funnel */

/*SELECT *
FROM sales.customers*/

/*SELECT *
FROM sales.products*/

/*SELECT *
FROM sales.stores*/

-- LEFT JOIN
/*SELECT *
FROM sales.funnel LEFT JOIN sales.customers
ON sales.funnel.customer_id = sales.customers.customer_id
WHERE sales.customers.state = 'SP' -- filtro do estado de sp*/

/*SELECT COUNT(*), sales.customers.state
FROM sales.funnel LEFT JOIN sales.customers
ON sales.funnel.customer_id = sales.customers.customer_id
GROUP BY sales.customers.state*/

--SELECT MAX(visit_page_date), MIN(visit_page_date)
--from sales.funnel

--SELECT COUNT(*), sales.customers.state
--FROM sales.funnel LEFT JOIN sales.customers
--ON sales.funnel.customer_id = sales.customers.customer_id
--GROUP BY sales.customers.state
--HAVING YEAR(sales.funnel.visit_page_date) = 2020