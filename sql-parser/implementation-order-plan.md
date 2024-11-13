# An SQLite statement parser implementation order

The following list of statements is ordered from easy to hard from an 
implementation standpoint. There are some exception statement items in the list
where the implementation is easier than the previous statement, but the 
exception is made for grouping purposes only, i.e., grouping related statements
together.
The items were taken from the official SQLite documentation from [here](https://www.sqlite.org/lang.html)

## Easy level

- OK - drop-index-stmt
- OK - drop-table-stmt
- OK - drop-trigger-stmt
- OK - drop-view-stmt

- vacuum-stmt
- detach-stmt
- analyze-stmt
- reindex-stmt

- begin-stmt
- commit-stmt
- rollback-stmt
- release-stmt
- savepoint-stmt
- pragma-stmt

## Intermediate level 

- math-functions
- date-and-time-functions
- json-functions
- expression
- core-functions
- aggregate-functions

- attach-stmt
- alter-table-stmt

## Hard level

### SELECT Statement

- indexed-by-part
- window-functions-part
- select-stmt
- with-clause-part (CTE)

### INSERT Statement

- on-conflict-clause-part
- returning-clause-part
- upsert-part
- insert-stmt
- replace-stmt

### CREATE Statements

- create-virtual-table-stmt
- create-index-stmt
- create-view-stmt
- create-table-stmt
- create-trigger-stmt

### DELETE Statements

- delete-stmt
- delete-stmt-limited

### UPDATE Statements
- update-stmt
- update-stmt-limited

### EXPLAIN Statements 
After implementing all items above, it must become an easy ride
- explain-stmt 