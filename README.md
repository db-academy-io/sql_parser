# An SQL parser

## [db-academy.io/sql_parser](https://db-academy.io/sql_parser) course reference implementation in Rust

The following list of statements is ordered from easy to hard from an 
implementation standpoint. There are some exception statement items in the list
where the implementation is easier than the previous statement, but the 
exception is made for grouping purposes only, i.e., grouping related statements
together. 

The items were taken from the official SQLite documentation from 
[here](https://www.sqlite.org/lang.html)

## Easy level ![progress](https://progress-bar.xyz/14/?scale=14)

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

## Intermediate level ![progress](https://progress-bar.xyz/0/?scale=8&suffix=%%%20(0%20of%208))

1. math-functions
1. date-and-time-functions
1. json-functions
1. expression
1. core-functions
1. aggregate-functions
1. attach-stmt
1. alter-table-stmt

## Hard level ![progress](https://progress-bar.xyz/0/?scale=19&suffix=%%%20(0%20of%2019))

### SELECT Statement ![progress](https://progress-bar.xyz/0/?scale=4&suffix=%%%20(0%20of%204))

1. indexed-by-part
1. window-functions-part
1. select-stmt
1. with-clause-part (CTE)

### INSERT Statement ![progress](https://progress-bar.xyz/0/?scale=5&suffix=%%%20(0%20of%205))

1. on-conflict-clause-part
1. returning-clause-part
1. upsert-part
1. insert-stmt
1. replace-stmt

### CREATE Statements ![progress](https://progress-bar.xyz/0/?scale=45&suffix=%%%20(0%20of%205))

1. create-virtual-table-stmt
1. create-index-stmt
1. create-view-stmt
1. create-table-stmt
1. create-trigger-stmt

### DELETE Statements ![progress](https://progress-bar.xyz/0/?scale=2&suffix=%%%20(0%20of%202))

1. delete-stmt
1. delete-stmt-limited

### UPDATE Statements ![progress](https://progress-bar.xyz/0/?scale=2&suffix=%%%20(0%20of%202))
1. update-stmt
1. update-stmt-limited

### EXPLAIN Statements ![progress](https://progress-bar.xyz/0/?scale=1&suffix=%%%20(0%20of%201))
After implementing all items above, it must become an easy ride
1. explain-stmt 
