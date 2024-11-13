# An SQLite statement parser implementation order

The following list of statements is ordered from easy to hard from an 
implementation standpoint. There are some exception statement items in the list
where the implementation is easier than the previous statement, but the 
exception is made for grouping purposes only, i.e., grouping related statements
together

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

- attach-stmt
- alter-table-stmt

## Hard level 

- select-stmt
- insert-stmt

## Extra hard level 

- create-virtual-table-stmt
- create-index-stmt
- create-view-stmt
- create-table-stmt
- create-trigger-stmt

- delete-stmt
- delete-stmt-limited

- update-stmt
- update-stmt-limited

## Easy 
Easy to implement, only after implementing all items from Extra hard level group  
- explain-stmt 