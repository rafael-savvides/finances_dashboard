# Finances dashboard

Finances dashboard built with R Shiny.

Run with `Rscript --vanilla run.R data categories`, where: 

- `data` is a csv or rds file that contains columns `date`, `amount`, `payer`, `receiver`, `message` (optional)
- `categories` (optional) is a json file of the form `{category: [terms]}`. 
    - A transaction is assigned a category `c`, if its `receiver` or `message` contains a term in `c`'s `terms`.
    - The default category is `"None"`.
