# An SQL parser

[db-academy.io/sql_parser](https://db-academy.io/sql_parser) reference implementation in Rust

The following list of statements is ordered not only by difficulty but also by
the time required for implementation, from easier and quicker to harder and 
more time-consuming. There are exceptions where certain statements are simpler
or faster to implement than the preceding ones. These exceptions are 
intentional, made solely for grouping related statements together.

The items were taken from the official SQLite documentation 
[here](https://www.sqlite.org/lang.html)

## Simple statements ![progress](https://progress-bar.xyz/14/?scale=14&suffix=%%%20(14%20of%2014)&width=140)
1. [drop-table-stmt](https://www.sqlite.org/lang_droptable.html) &#9989;
1. [drop-index-stmt](https://www.sqlite.org/lang_dropindex.html) &#9989;
1. [drop-trigger-stmt](https://www.sqlite.org/lang_droptrigger.html) &#9989;
1. [drop-view-stmt](https://www.sqlite.org/lang_dropview.html) &#9989;
1. [vacuum-stmt](https://www.sqlite.org/lang_vacuum.html) &#9989;
1. [detach-stmt](https://www.sqlite.org/lang_detach.html) &#9989;
1. [analyze-stmt](https://www.sqlite.org/lang_analyze.html) &#9989; 
1. [reindex-stmt](https://www.sqlite.org/lang_reindex.html) &#9989; 
1. [commit-stmt](https://www.sqlite.org/lang_transaction.html) &#9989;
1. [begin-stmt](https://www.sqlite.org/lang_transaction.html) &#9989;
1. [savepoint-stmt](https://www.sqlite.org/lang_savepoint.html) &#9989;
1. [release-stmt](https://www.sqlite.org/lang_savepoint.html) &#9989;
1. [rollback-stmt](https://www.sqlite.org/lang_savepoint.html) &#9989;
1. [pragma-stmt](https://www.sqlite.org/pragma.html) &#9989;

## Parsing sql expr ![progress](https://progress-bar.xyz/17/?scale=17&suffix=%%%20(17%20of%2017)&width=140)
1. [literal-value](https://www.sqlite.org/lang_expr.html#literalvalue) &#9989;
1. [bind-parameter](https://www.sqlite.org/lang_expr.html#bindvar)&#9989;
1. [identifiers](https://www.sqlite.org/lang_expr.html#columnname)&#9989;
1. [unary-operator](https://www.sqlite.org/lang_expr.html#unaryexp)&#9989;
1. [binary-operator](https://www.sqlite.org/lang_expr.html#binaryexp)&#9989;
1. [function](https://www.sqlite.org/lang_expr.html#function.html)&#9989;
1. [parenthesized-expression](https://www.sqlite.org/lang_expr.html#parenexp)&#9989;
1. [cast-expression](https://www.sqlite.org/lang_expr.html#cast)&#9989;
1. [collate-expression](https://www.sqlite.org/lang_expr.html#collateexp)&#9989;
1. [like-expression](https://www.sqlite.org/lang_expr.html#likeexp)&#9989;
1. [null-expression](https://www.sqlite.org/lang_expr.html#likeexp)&#9989;
1. [expression-from-expression](https://www.sqlite.org/lang_expr.html#exprlist)&#9989;
1. [between-expression](https://www.sqlite.org/lang_expr.html#betweenexp)&#9989;
1. [expression-in-select](https://www.sqlite.org/lang_expr.html#exprlist)&#9989;
1. [exists-expression](https://www.sqlite.org/lang_expr.html#exprlist)&#9989;
1. [case-expression](https://www.sqlite.org/lang_expr.html#caseexp)&#9989;
1. [raise-function](https://www.sqlite.org/lang_expr.html#raisefunc)&#9989;

## Intermediate statements ![progress](https://progress-bar.xyz/0/?scale=7&suffix=%%%20(0%20of%207)&width=140)
1. [attach-stmt](https://www.sqlite.org/lang_attach.html)
1. [math-functions](https://www.sqlite.org/lang_mathfunc.html)
1. [date-and-time-functions](https://www.sqlite.org/lang_datefunc.html)
1. [json-functions](https://www.sqlite.org/json1.html)
1. [core-functions](https://www.sqlite.org/lang_corefunc.html)
1. [aggregate-functions](https://www.sqlite.org/lang_aggfunc.html)
1. [alter-table-stmt](https://www.sqlite.org/lang_altertable.html)

## Advanced statements ![progress](https://progress-bar.xyz/0/?scale=19&suffix=%%%20(0%20of%2019)&width=140)

### SELECT Statement ![progress](https://progress-bar.xyz/0/?scale=4&suffix=%%%20(0%20of%204)&width=140)
1. indexed-by-part
1. window-functions-part
1. select-stmt
1. with-clause-part (CTE)

### INSERT Statement ![progress](https://progress-bar.xyz/0/?scale=5&suffix=%%%20(0%20of%205)&width=140)
1. on-conflict-clause-part
1. returning-clause-part
1. upsert-part
1. insert-stmt
1. replace-stmt

### CREATE Statements ![progress](https://progress-bar.xyz/0/?scale=45&suffix=%%%20(0%20of%205)&width=140)
1. create-virtual-table-stmt
1. create-index-stmt
1. create-view-stmt
1. create-table-stmt
1. create-trigger-stmt

### DELETE Statements ![progress](https://progress-bar.xyz/0/?scale=2&suffix=%%%20(0%20of%202)&width=140)
1. delete-stmt
1. delete-stmt-limited

### UPDATE Statements ![progress](https://progress-bar.xyz/0/?scale=2&suffix=%%%20(0%20of%202)&width=140)
1. update-stmt
1. update-stmt-limited

### EXPLAIN Statements ![progress](https://progress-bar.xyz/0/?scale=1&suffix=%%%20(0%20of%201)&width=140)
1. explain-stmt 
