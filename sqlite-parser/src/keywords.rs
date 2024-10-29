use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::convert::TryFrom;

/// #### Keyword tokens
///
/// The following keywords are recognized as distinct tokens:
///
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Keyword {
    /// H41503: SQLite shall recognize the 5-character sequenence "ABORT" in any combination of upper and lower case letters as the keyword token ABORT.
    Abort,
    /// TODO
    Action,
    /// H41506: SQLite shall recognize the 3-character sequenence "ADD" in any combination of upper and lower case letters as the keyword token ADD.
    Add,
    /// H41509: SQLite shall recognize the 5-character sequenence "AFTER" in any combination of upper and lower case letters as the keyword token AFTER.
    After,
    /// H41512: SQLite shall recognize the 3-character sequenence "ALL" in any combination of upper and lower case letters as the keyword token ALL.
    All,
    /// H41515: SQLite shall recognize the 5-character sequenence "ALTER" in any combination of upper and lower case letters as the keyword token ALTER.
    Alter,
    /// TODO
    Always,
    /// H41518: SQLite shall recognize the 7-character sequenence "ANALYZE" in any combination of upper and lower case letters as the keyword token ANALYZE.
    Analyze,
    /// H41521: SQLite shall recognize the 3-character sequenence "AND" in any combination of upper and lower case letters as the keyword token AND.
    And,
    /// H41524: SQLite shall recognize the 2-character sequenence "AS" in any combination of upper and lower case letters as the keyword token AS.
    As,
    /// H41527: SQLite shall recognize the 3-character sequenence "ASC" in any combination of upper and lower case letters as the keyword token ASC.
    Asc,
    /// H41530: SQLite shall recognize the 6-character sequenence "ATTACH" in any combination of upper and lower case letters as the keyword token ATTACH.
    Attach,
    /// H41533: SQLite shall recognize the 13-character sequenence "AUTOINCREMENT" in any combination of upper and lower case letters as the keyword token AUTOINCR.
    Autoincrement,
    /// H41536: SQLite shall recognize the 6-character sequenence "BEFORE" in any combination of upper and lower case letters as the keyword token BEFORE.
    Before,
    /// H41539: SQLite shall recognize the 5-character sequenence "BEGIN" in any combination of upper and lower case letters as the keyword token BEGIN.
    Begin,
    /// H41542: SQLite shall recognize the 7-character sequenence "BETWEEN" in any combination of upper and lower case letters as the keyword token BETWEEN.
    Between,
    /// H41545: SQLite shall recognize the 2-character sequenence "BY" in any combination of upper and lower case letters as the keyword token BY.
    By,
    /// H41548: SQLite shall recognize the 7-character sequenence "CASCADE" in any combination of upper and lower case letters as the keyword token CASCADE.
    Cascade,
    /// H41551: SQLite shall recognize the 4-character sequenence "CASE" in any combination of upper and lower case letters as the keyword token CASE.
    Case,
    /// H41554: SQLite shall recognize the 4-character sequenence "CAST" in any combination of upper and lower case letters as the keyword token CAST.
    Cast,
    /// H41557: SQLite shall recognize the 5-character sequenence "CHECK" in any combination of upper and lower case letters as the keyword token CHECK.
    Check,
    /// H41560: SQLite shall recognize the 7-character sequenence "COLLATE" in any combination of upper and lower case letters as the keyword token COLLATE.
    Collate,
    /// H41563: SQLite shall recognize the 6-character sequenence "COLUMN" in any combination of upper and lower case letters as the keyword token COLUMNKW.
    Column,
    /// H41566: SQLite shall recognize the 6-character sequenence "COMMIT" in any combination of upper and lower case letters as the keyword token COMMIT.
    Commit,
    /// H41569: SQLite shall recognize the 8-character sequenence "CONFLICT" in any combination of upper and lower case letters as the keyword token CONFLICT.
    Conflict,
    /// H41572: SQLite shall recognize the 10-character sequenence "CONSTRAINT" in any combination of upper and lower case letters as the keyword token CONSTRAINT.
    Constraint,
    /// H41575: SQLite shall recognize the 6-character sequenence "CREATE" in any combination of upper and lower case letters as the keyword token CREATE.
    Create,
    /// H41578: SQLite shall recognize the 5-character sequenence "CROSS" in any combination of upper and lower case letters as the keyword token JOIN_KW.
    Cross,
    /// TODO
    Current,
    /// H41581: SQLite shall recognize the 12-character sequenence "CURRENT_DATE" in any combination of upper and lower case letters as the keyword token CTIME_KW.
    CurrentDate,
    /// H41584: SQLite shall recognize the 12-character sequenence "CURRENT_TIME" in any combination of upper and lower case letters as the keyword token CTIME_KW.
    CurrentTime,
    /// H41587: SQLite shall recognize the 17-character sequenence "CURRENT_TIMESTAMP" in any combination of upper and lower case letters as the keyword token CTIME_KW.
    CurrentTimestamp,
    /// H41590: SQLite shall recognize the 8-character sequenence "DATABASE" in any combination of upper and lower case letters as the keyword token DATABASE.
    Database,
    /// H41593: SQLite shall recognize the 7-character sequenence "DEFAULT" in any combination of upper and lower case letters as the keyword token DEFAULT.
    Default,
    /// H41599: SQLite shall recognize the 10-character sequenence "DEFERRABLE" in any combination of upper and lower case letters as the keyword token DEFERRABLE.
    Deferrable,
    /// H41596: SQLite shall recognize the 8-character sequenence "DEFERRED" in any combination of upper and lower case letters as the keyword token DEFERRED.
    Deferred,
    /// H41602: SQLite shall recognize the 6-character sequenence "DELETE" in any combination of upper and lower case letters as the keyword token DELETE.
    Delete,
    /// H41605: SQLite shall recognize the 4-character sequenence "DESC" in any combination of upper and lower case letters as the keyword token DESC.
    Desc,
    /// H41608: SQLite shall recognize the 6-character sequenence "DETACH" in any combination of upper and lower case letters as the keyword token DETACH.
    Detach,
    /// H41611: SQLite shall recognize the 8-character sequenence "DISTINCT" in any combination of upper and lower case letters as the keyword token DISTINCT.
    Distinct,
    /// TODO
    Do,
    /// H41614: SQLite shall recognize the 4-character sequenence "DROP" in any combination of upper and lower case letters as the keyword token DROP.
    Drop,
    /// H41620: SQLite shall recognize the 4-character sequenence "EACH" in any combination of upper and lower case letters as the keyword token EACH.
    Each,
    /// H41623: SQLite shall recognize the 4-character sequenence "ELSE" in any combination of upper and lower case letters as the keyword token ELSE.
    Else,
    /// H41617: SQLite shall recognize the 3-character sequenence "END" in any combination of upper and lower case letters as the keyword token END.
    End,
    /// H41626: SQLite shall recognize the 6-character sequenence "ESCAPE" in any combination of upper and lower case letters as the keyword token ESCAPE.
    Escape,
    /// H41629: SQLite shall recognize the 6-character sequenence "EXCEPT" in any combination of upper and lower case letters as the keyword token EXCEPT.
    Except,
    /// TODO
    Exclude,
    /// H41632: SQLite shall recognize the 9-character sequenence "EXCLUSIVE" in any combination of upper and lower case letters as the keyword token EXCLUSIVE.
    Exclusive,
    /// H41635: SQLite shall recognize the 6-character sequenence "EXISTS" in any combination of upper and lower case letters as the keyword token EXISTS.
    Exists,
    /// H41638: SQLite shall recognize the 7-character sequenence "EXPLAIN" in any combination of upper and lower case letters as the keyword token EXPLAIN.
    Explain,
    /// H41641: SQLite shall recognize the 4-character sequenence "FAIL" in any combination of upper and lower case letters as the keyword token FAIL.
    Fail,
    /// TODO
    Filter,
    /// TODO
    First,
    /// TODO
    Following,
    /// H41644: SQLite shall recognize the 3-character sequenence "FOR" in any combination of upper and lower case letters as the keyword token FOR.
    For,
    /// H41647: SQLite shall recognize the 7-character sequenence "FOREIGN" in any combination of upper and lower case letters as the keyword token FOREIGN.
    Foreign,
    /// H41650: SQLite shall recognize the 4-character sequenence "FROM" in any combination of upper and lower case letters as the keyword token FROM.
    From,
    /// H41653: SQLite shall recognize the 4-character sequenence "FULL" in any combination of upper and lower case letters as the keyword token JOIN_KW.
    Full,
    /// TODO
    Generated,
    /// H41656: SQLite shall recognize the 4-character sequenence "GLOB" in any combination of upper and lower case letters as the keyword token LIKE_KW.
    Glob,
    /// H41659: SQLite shall recognize the 5-character sequenence "GROUP" in any combination of upper and lower case letters as the keyword token GROUP.
    Group,
    /// TODO
    Groups,
    /// H41662: SQLite shall recognize the 6-character sequenence "HAVING" in any combination of upper and lower case letters as the keyword token HAVING.
    Having,
    /// H41665: SQLite shall recognize the 2-character sequenence "IF" in any combination of upper and lower case letters as the keyword token IF.
    If,
    /// H41668: SQLite shall recognize the 6-character sequenence "IGNORE" in any combination of upper and lower case letters as the keyword token IGNORE.
    Ignore,
    /// H41671: SQLite shall recognize the 9-character sequenence "IMMEDIATE" in any combination of upper and lower case letters as the keyword token IMMEDIATE.
    Immediate,
    /// H41674: SQLite shall recognize the 2-character sequenence "IN" in any combination of upper and lower case letters as the keyword token IN.
    In,
    /// H41677: SQLite shall recognize the 5-character sequenence "INDEX" in any combination of upper and lower case letters as the keyword token INDEX.
    Index,
    /// TODO
    Indexed,
    /// H41680: SQLite shall recognize the 9-character sequenence "INITIALLY" in any combination of upper and lower case letters as the keyword token INITIALLY.
    Initially,
    /// H41683: SQLite shall recognize the 5-character sequenence "INNER" in any combination of upper and lower case letters as the keyword token JOIN_KW.
    Inner,
    /// H41686: SQLite shall recognize the 6-character sequenence "INSERT" in any combination of upper and lower case letters as the keyword token INSERT.
    Insert,
    /// H41689: SQLite shall recognize the 7-character sequenence "INSTEAD" in any combination of upper and lower case letters as the keyword token INSTEAD.
    Instead,
    /// H41692: SQLite shall recognize the 9-character sequenence "INTERSECT" in any combination of upper and lower case letters as the keyword token INTERSECT.
    Intersect,
    /// H41695: SQLite shall recognize the 4-character sequenence "INTO" in any combination of upper and lower case letters as the keyword token INTO.
    Into,
    /// H41698: SQLite shall recognize the 2-character sequenence "IS" in any combination of upper and lower case letters as the keyword token IS.
    Is,
    /// H41701: SQLite shall recognize the 6-character sequenence "ISNULL" in any combination of upper and lower case letters as the keyword token ISNULL.
    Isnull,
    /// H41704: SQLite shall recognize the 4-character sequenence "JOIN" in any combination of upper and lower case letters as the keyword token JOIN.
    Join,
    /// H41707: SQLite shall recognize the 3-character sequenence "KEY" in any combination of upper and lower case letters as the keyword token KEY.
    Key,
    Last,
    /// H41710: SQLite shall recognize the 4-character sequenence "LEFT" in any combination of upper and lower case letters as the keyword token JOIN_KW.
    Left,
    /// H41713: SQLite shall recognize the 4-character sequenence "LIKE" in any combination of upper and lower case letters as the keyword token LIKE_KW.
    Like,
    /// H41716: SQLite shall recognize the 5-character sequenence "LIMIT" in any combination of upper and lower case letters as the keyword token LIMIT.
    Limit,
    /// H41719: SQLite shall recognize the 5-character sequenence "MATCH" in any combination of upper and lower case letters as the keyword token MATCH.
    Match,
    /// TODO
    Materialized,
    /// H41722: SQLite shall recognize the 7-character sequenence "NATURAL" in any combination of upper and lower case letters as the keyword token JOIN_KW.
    Natural,
    /// TODO
    No,
    /// H41725: SQLite shall recognize the 3-character sequenence "NOT" in any combination of upper and lower case letters as the keyword token NOT.
    Not,
    /// TODO
    Nothing,
    /// H41728: SQLite shall recognize the 7-character sequenence "NOTNULL" in any combination of upper and lower case letters as the keyword token NOTNULL.
    Notnull,
    /// H41731: SQLite shall recognize the 4-character sequenence "NULL" in any combination of upper and lower case letters as the keyword token NULL.
    Null,
    /// TODO
    Nulls,
    /// H41734: SQLite shall recognize the 2-character sequenence "OF" in any combination of upper and lower case letters as the keyword token OF.
    Of,
    /// H41737: SQLite shall recognize the 6-character sequenence "OFFSET" in any combination of upper and lower case letters as the keyword token OFFSET.
    Offset,
    /// H41740: SQLite shall recognize the 2-character sequenence "ON" in any combination of upper and lower case letters as the keyword token ON.
    On,
    /// H41743: SQLite shall recognize the 2-character sequenence "OR" in any combination of upper and lower case letters as the keyword token OR.
    Or,
    /// H41746: SQLite shall recognize the 5-character sequenence "ORDER" in any combination of upper and lower case letters as the keyword token ORDER.
    Order,
    /// TODO
    Others,
    /// H41749: SQLite shall recognize the 5-character sequenence "OUTER" in any combination of upper and lower case letters as the keyword token JOIN_KW.
    Outer,
    /// TODO
    Over,
    /// TODO
    Partition,
    /// H41752: SQLite shall recognize the 4-character sequenence "PLAN" in any combination of upper and lower case letters as the keyword token PLAN.
    Plan,
    /// H41755: SQLite shall recognize the 6-character sequenence "PRAGMA" in any combination of upper and lower case letters as the keyword token PRAGMA.
    Pragma,
    /// TODO
    Preceding,
    /// H41758: SQLite shall recognize the 7-character sequenence "PRIMARY" in any combination of upper and lower case letters as the keyword token PRIMARY.
    Primary,
    /// H41761: SQLite shall recognize the 5-character sequenence "QUERY" in any combination of upper and lower case letters as the keyword token QUERY.
    Query,
    /// H41764: SQLite shall recognize the 5-character sequenence "RAISE" in any combination of upper and lower case letters as the keyword token RAISE.
    Raise,
    /// TODO
    Range,
    /// TODO
    Recursive,
    /// H41767: SQLite shall recognize the 10-character sequenence "REFERENCES" in any combination of upper and lower case letters as the keyword token REFERENCES.
    References,
    /// H41770: SQLite shall recognize the 6-character sequenence "REGEXP" in any combination of upper and lower case letters as the keyword token LIKE_KW.
    Regexp,
    /// H41773: SQLite shall recognize the 7-character sequenence "REINDEX" in any combination of upper and lower case letters as the keyword token REINDEX.
    Reindex,
    /// TODO
    Release,
    /// H41776: SQLite shall recognize the 6-character sequenence "RENAME" in any combination of upper and lower case letters as the keyword token RENAME.
    Rename,
    /// H41779: SQLite shall recognize the 7-character sequenence "REPLACE" in any combination of upper and lower case letters as the keyword token REPLACE.
    Replace,
    /// H41782: SQLite shall recognize the 8-character sequenence "RESTRICT" in any combination of upper and lower case letters as the keyword token RESTRICT.
    Restrict,
    /// TODO
    Returning,
    /// H41785: SQLite shall recognize the 5-character sequenence "RIGHT" in any combination of upper and lower case letters as the keyword token JOIN_KW.
    Right,
    /// H41788: SQLite shall recognize the 8-character sequenence "ROLLBACK" in any combination of upper and lower case letters as the keyword token ROLLBACK.
    Rollback,
    /// H41791: SQLite shall recognize the 3-character sequenence "ROW" in any combination of upper and lower case letters as the keyword token ROW.
    Row,
    /// TODO
    Rows,
    /// TODO
    Savepoint,
    /// H41794: SQLite shall recognize the 6-character sequenence "SELECT" in any combination of upper and lower case letters as the keyword token SELECT.
    Select,
    /// H41797: SQLite shall recognize the 3-character sequenence "SET" in any combination of upper and lower case letters as the keyword token SET.
    Set,
    /// H41800: SQLite shall recognize the 5-character sequenence "TABLE" in any combination of upper and lower case letters as the keyword token TABLE.
    Table,
    /// H41803: SQLite shall recognize the 4-character sequenence "TEMP" in any combination of upper and lower case letters as the keyword token TEMP.
    Temp,
    /// H41806: SQLite shall recognize the 9-character sequenence "TEMPORARY" in any combination of upper and lower case letters as the keyword token TEMP.
    Temporary,
    /// H41809: SQLite shall recognize the 4-character sequenence "THEN" in any combination of upper and lower case letters as the keyword token THEN.
    Then,
    /// TODO
    Ties,
    /// H41812: SQLite shall recognize the 2-character sequenence "TO" in any combination of upper and lower case letters as the keyword token TO.
    To,
    /// H41815: SQLite shall recognize the 11-character sequenence "TRANSACTION" in any combination of upper and lower case letters as the keyword token TRANSACTION.
    Transaction,
    /// H41818: SQLite shall recognize the 7-character sequenence "TRIGGER" in any combination of upper and lower case letters as the keyword token TRIGGER.
    Trigger,
    /// TODO
    Unbounded,
    /// H41821: SQLite shall recognize the 5-character sequenence "UNION" in any combination of upper and lower case letters as the keyword token UNION.
    Union,
    /// H41824: SQLite shall recognize the 6-character sequenence "UNIQUE" in any combination of upper and lower case letters as the keyword token UNIQUE.
    Unique,
    /// H41827: SQLite shall recognize the 6-character sequenence "UPDATE" in any combination of upper and lower case letters as the keyword token UPDATE.
    Update,
    /// H41830: SQLite shall recognize the 5-character sequenence "USING" in any combination of upper and lower case letters as the keyword token USING.
    Using,
    /// H41833: SQLite shall recognize the 6-character sequenence "VACUUM" in any combination of upper and lower case letters as the keyword token VACUUM.
    Vacuum,
    /// H41836: SQLite shall recognize the 6-character sequenence "VALUES" in any combination of upper and lower case letters as the keyword token VALUES.
    Values,
    /// H41839: SQLite shall recognize the 4-character sequenence "VIEW" in any combination of upper and lower case letters as the keyword token VIEW.
    View,
    /// H41842: SQLite shall recognize the 7-character sequenence "VIRTUAL" in any combination of upper and lower case letters as the keyword token VIRTUAL.
    Virtual,
    /// H41845: SQLite shall recognize the 4-character sequenence "WHEN" in any combination of upper and lower case letters as the keyword token WHEN.
    When,
    /// H41848: SQLite shall recognize the 5-character sequenence "WHERE" in any combination of upper and lower case letters as the keyword token WHERE.
    Where,
    /// TODO
    Window,
    /// TODO
    With,
    /// TODO
    Without,
}

impl TryFrom<&str> for Keyword {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let uppercased = value.to_ascii_uppercase();
        match KEYWORD_MAP.get(uppercased.as_str()) {
            Some(&keyword) => Ok(keyword),
            None => Err(()),
        }
    }
}

static KEYWORD_MAP: Lazy<HashMap<&'static str, Keyword>> = Lazy::new(|| {
    use Keyword::*;

    let pairs = [
        ("ABORT", Abort),
        ("ACTION", Action),
        ("ADD", Add),
        ("AFTER", After),
        ("ALL", All),
        ("ALTER", Alter),
        ("ALWAYS", Always),
        ("ANALYZE", Analyze),
        ("AND", And),
        ("AS", As),
        ("ASC", Asc),
        ("ATTACH", Attach),
        ("AUTOINCREMENT", Autoincrement),
        ("BEFORE", Before),
        ("BEGIN", Begin),
        ("BETWEEN", Between),
        ("BY", By),
        ("CASCADE", Cascade),
        ("CASE", Case),
        ("CAST", Cast),
        ("CHECK", Check),
        ("COLLATE", Collate),
        ("COLUMN", Column),
        ("COMMIT", Commit),
        ("CONFLICT", Conflict),
        ("CONSTRAINT", Constraint),
        ("CREATE", Create),
        ("CROSS", Cross),
        ("CURRENT", Current),
        ("CURRENT_DATE", CurrentDate),
        ("CURRENT_TIME", CurrentTime),
        ("CURRENT_TIMESTAMP", CurrentTimestamp),
        ("DATABASE", Database),
        ("DEFAULT", Default),
        ("DEFERRABLE", Deferrable),
        ("DEFERRED", Deferred),
        ("DELETE", Delete),
        ("DESC", Desc),
        ("DETACH", Detach),
        ("DISTINCT", Distinct),
        ("DO", Do),
        ("DROP", Drop),
        ("EACH", Each),
        ("ELSE", Else),
        ("END", End),
        ("ESCAPE", Escape),
        ("EXCEPT", Except),
        ("EXCLUDE", Exclude),
        ("EXCLUSIVE", Exclusive),
        ("EXISTS", Exists),
        ("EXPLAIN", Explain),
        ("FAIL", Fail),
        ("FILTER", Filter),
        ("FIRST", First),
        ("FOLLOWING", Following),
        ("FOR", For),
        ("FOREIGN", Foreign),
        ("FROM", From),
        ("FULL", Full),
        ("GENERATED", Generated),
        ("GLOB", Glob),
        ("GROUP", Group),
        ("GROUPS", Groups),
        ("HAVING", Having),
        ("IF", If),
        ("IGNORE", Ignore),
        ("IMMEDIATE", Immediate),
        ("IN", In),
        ("INDEX", Index),
        ("INDEXED", Indexed),
        ("INITIALLY", Initially),
        ("INNER", Inner),
        ("INSERT", Insert),
        ("INSTEAD", Instead),
        ("INTERSECT", Intersect),
        ("INTO", Into),
        ("IS", Is),
        ("ISNULL", Isnull),
        ("JOIN", Join),
        ("KEY", Key),
        ("LAST", Last),
        ("LEFT", Left),
        ("LIKE", Like),
        ("LIMIT", Limit),
        ("MATCH", Match),
        ("MATERIALIZED", Materialized),
        ("NATURAL", Natural),
        ("NO", No),
        ("NOT", Not),
        ("NOTHING", Nothing),
        ("NOTNULL", Notnull),
        ("NULL", Null),
        ("NULLS", Nulls),
        ("OF", Of),
        ("OFFSET", Offset),
        ("ON", On),
        ("OR", Or),
        ("ORDER", Order),
        ("OTHERS", Others),
        ("OUTER", Outer),
        ("OVER", Over),
        ("PARTITION", Partition),
        ("PLAN", Plan),
        ("PRAGMA", Pragma),
        ("PRECEDING", Preceding),
        ("PRIMARY", Primary),
        ("QUERY", Query),
        ("RAISE", Raise),
        ("RANGE", Range),
        ("RECURSIVE", Recursive),
        ("REFERENCES", References),
        ("REGEXP", Regexp),
        ("REINDEX", Reindex),
        ("RELEASE", Release),
        ("RENAME", Rename),
        ("REPLACE", Replace),
        ("RESTRICT", Restrict),
        ("RETURNING", Returning),
        ("RIGHT", Right),
        ("ROLLBACK", Rollback),
        ("ROW", Row),
        ("ROWS", Rows),
        ("SAVEPOINT", Savepoint),
        ("SELECT", Select),
        ("SET", Set),
        ("TABLE", Table),
        ("TEMP", Temp),
        ("TEMPORARY", Temporary),
        ("THEN", Then),
        ("TIES", Ties),
        ("TO", To),
        ("TRANSACTION", Transaction),
        ("TRIGGER", Trigger),
        ("UNBOUNDED", Unbounded),
        ("UNION", Union),
        ("UNIQUE", Unique),
        ("UPDATE", Update),
        ("USING", Using),
        ("VACUUM", Vacuum),
        ("VALUES", Values),
        ("VIEW", View),
        ("VIRTUAL", Virtual),
        ("WHEN", When),
        ("WHERE", Where),
        ("WINDOW", Window),
        ("WITH", With),
        ("WITHOUT", Without),
    ];
    pairs.iter().cloned().collect()
});
