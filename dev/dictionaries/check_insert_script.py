# dev/dictionaries/check_insert_script.py
#
# Checks an INSERT script written by convert_definitions.R without SQL Server:
#   1. it is plain ASCII, and each statement but the THROW guard parses as
#      T-SQL (sqlglot does not know THROW);
#   2. it runs in SQLite against the tables of inst/sql/dictionary_tables.sql,
#      with their keys, NOT NULL, CHECK and foreign key constraints;
#   3. every value read back equals the rows R produced.
#
# Usage, from the root of the repository, in a cloud session (pip install
# sqlglot first):
#   Rscript -e 'source("dev/dictionaries/convert_definitions.R");
#     x <- convert_definitions("definitions.xlsx", tables, "linkage.sql");
#     jsonlite::write_json(x$rows, "rows.json", dataframe = "rows",
#                          na = "null", digits = NA)'
#   python3 dev/dictionaries/check_insert_script.py \
#     inst/sql/dictionary_tables.sql linkage.sql rows.json
# Write linkage.sql and rows.json outside the repository: they hold the
# workbook's contents.
import json, re, sqlite3, sys
import sqlglot

ddl_path, script_path, rows_path = sys.argv[1:4]
script = open(script_path, encoding="ascii").read()


# The script's statements: split at semicolons outside literals
def statements(sql):
    parts, cur, quoted = [], [], False
    for ch in re.sub(r"(?m)^--.*\n", "", sql):
        cur.append(ch)
        if ch == "'":
            quoted = not quoted
        elif ch == ";" and not quoted:
            parts.append("".join(cur).strip())
            cur = []
    return [p for p in parts if p]


kinds = {}
for s in statements(script):
    if s.startswith("IF EXISTS") and "THROW 50000" in s:
        kind = "THROW guard (not parsed)"
    else:
        kind = type(sqlglot.parse_one(s, read="tsql")).__name__
    kinds[kind] = kinds.get(kind, 0) + 1
print("T-SQL statements:", kinds)

# The tables in SQLite, with GLOB for SQL Server's LIKE character classes
ddl = open(ddl_path, encoding="ascii").read()
db = sqlite3.connect(":memory:")
db.execute("PRAGMA foreign_keys = ON")
for c in re.findall(r"CREATE TABLE .*?\n\);", ddl, flags=re.S):
    c = re.sub(r"--[^\n]*", "", c).replace("ref.", "")
    c = c.replace("LIKE\n        N'#", "GLOB '#").replace("N'", "'")
    db.execute(c.replace("nvarchar(max)", "text"))


# The inserts in SQLite: outside literals, N'..' is '..', + is || and NCHAR char
def to_sqlite(sql):
    out, i, n = [], 0, len(sql)
    while i < n:
        if sql[i] == "N" and sql[i + 1:i + 2] == "'":
            i += 1
        elif sql[i] == "'":
            j = i + 1
            while True:
                j = sql.index("'", j)
                if sql[j + 1:j + 2] != "'":
                    break
                j += 2
            out.append(sql[i:j + 1])
            i = j + 1
        elif sql.startswith("NCHAR(", i):
            out.append("char(")
            i += 6
        elif sql[i] == "+":
            out.append("||")
            i += 1
        else:
            out.append(sql[i])
            i += 1
    return "".join(out)


body = script[script.index("BEGIN TRANSACTION;") + 18:script.index("COMMIT TRANSACTION;")]
inserts = statements(to_sqlite(body.replace("ref.", "")))
for s in inserts:
    db.execute(s)
db.commit()
print("SQLite:", len(inserts), "INSERT statements; foreign key check:",
      db.execute("PRAGMA foreign_key_check").fetchall() or "clean")

# Every value read back, against R's rows
rows = json.load(open(rows_path, encoding="utf8"))
differ = 0
for table, want in rows.items():
    cols = list(want[0].keys())
    want = sorted((tuple(int(w[c]) if isinstance(w[c], bool) else w[c]
                         for c in cols) for w in want), key=repr)
    got = sorted(db.execute(f"SELECT {', '.join(cols)} FROM {table}").fetchall(),
                 key=repr)
    same = want == got
    differ += not same
    print(f"  {table}: {len(got)} rows, {'equal to' if same else 'DIFFERENT from'} R's")
print("All values equal" if not differ else f"{differ} tables differ")
sys.exit(1 if differ else 0)
