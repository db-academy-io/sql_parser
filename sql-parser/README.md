# An SQLite statement parser

The following list of statements is ordered from easy to hard from an 
implementation standpoint. There are some exception statement items in the list
where the implementation is easier than the previous statement, but the 
exception is made for grouping purposes only, i.e., grouping related statements
together.
The items were taken from the official SQLite documentation from [here](https://www.sqlite.org/lang.html)

## Easy level ![progress](https://progress-bar.xyz/4/?scale=14&width=120&color=babaca&suffix=%20of%2014)

1. drop-index-stmt &#9989;
1. drop-table-stmt &#9989;
1. drop-trigger-stmt &#9989;
1. drop-view-stmt &#9989;

1. vacuum-stmt
1. detach-stmt
1. analyze-stmt
1. reindex-stmt

1. begin-stmt
1. commit-stmt
1. rollback-stmt
1. release-stmt
1. savepoint-stmt
1. pragma-stmt

## Intermediate level ![progress](https://progress-bar.xyz/0/?scale=14&width=120&color=babaca&suffix=%20of%208)

- math-functions
- date-and-time-functions
- json-functions
- expression
- core-functions
- aggregate-functions

- attach-stmt
- alter-table-stmt

## Hard level ![progress](https://progress-bar.xyz/0/?scale=14&width=120&color=babaca&suffix=%20of%2019)

### SELECT Statement ![progress](https://progress-bar.xyz/0/?scale=14&width=120&color=babaca&suffix=%20of%204)

- indexed-by-part
- window-functions-part
- select-stmt
- with-clause-part (CTE)

### INSERT Statement ![progress](https://progress-bar.xyz/0/?scale=14&width=120&color=babaca&suffix=%20of%205)

- on-conflict-clause-part
- returning-clause-part
- upsert-part
- insert-stmt
- replace-stmt

### CREATE Statements ![progress](https://progress-bar.xyz/0/?scale=14&width=120&color=babaca&suffix=%20of%205)

- create-virtual-table-stmt
- create-index-stmt
- create-view-stmt
- create-table-stmt
- create-trigger-stmt

### DELETE Statements ![progress](https://progress-bar.xyz/0/?scale=14&width=120&color=babaca&suffix=%20of%202)

- delete-stmt
- delete-stmt-limited

### UPDATE Statements ![progress](https://progress-bar.xyz/0/?scale=14&width=120&color=babaca&suffix=%20of%202)
- update-stmt
- update-stmt-limited

### EXPLAIN Statements ![progress](https://progress-bar.xyz/0/?scale=14&width=120&color=babaca&suffix=%20of%201)
After implementing all items above, it must become an easy ride
- explain-stmt 