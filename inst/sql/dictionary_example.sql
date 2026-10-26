-- inst/sql/dictionary_example.sql
--
-- A worked example of adding, changing and removing rows in the dictionary
-- tables that dictionary_tables.sql creates. It builds a small profile,
-- Example, for Analysis.dbo.APC, shows its rows and then rolls back, so
-- running it as it is changes nothing. To keep rows of your own, copy the
-- statements needed, change the values, and end with COMMIT TRANSACTION in
-- place of ROLLBACK TRANSACTION. The column names and settings are
-- illustrations only.
--
-- The Linkage profile is loaded from definitions.xlsx by the script
-- dev/dictionaries/convert_definitions.R in the actepir repository.

USE Analysis;
SET XACT_ABORT ON;  -- an error rolls back every change
BEGIN TRANSACTION;

-- 1. The profile, with the title lines of the band at the top of its pages
INSERT INTO ref.DictionaryProfiles (Profile, Title, Subtitle)
VALUES (N'Example', N'Example Dataset Metadata',
        N'Data Analytics Branch: Epidemiology Section');

-- 2. Its closing sections, in SectionOrder. CHAR(10) starts a new line.
INSERT INTO ref.DictionarySections (Profile, SectionOrder, Heading, SectionText)
VALUES (N'Example', 1, N'General Enquiries',
        N'Epidemiology Section' + CHAR(10) + N'healthinfo@act.gov.au'),
       (N'Example', 2, N'Accessibility',
        N'If you have difficulty reading a standard printed document and '
        + N'would like an alternative format, please phone 13 22 81.');

-- 3. A caveat: Symbol, in Colour, is shown beside each column with this
--    CaveatCode, and CaveatText as a footnote. NCHAR(9650) is a black
--    triangle; NCHAR keeps the script in plain ASCII.
INSERT INTO ref.DictionaryCaveats (Profile, CaveatCode, Symbol, Colour, CaveatText)
VALUES (N'Example', N'detail', NCHAR(9650), N'#CF1F25',
        N'Degree of granularity subject to disclosure risk assessment');

-- 4. The order of themes. Themes not listed follow alphabetically, then the
--    columns without a theme, under Uncategorised. Steps of 10 leave room
--    for a theme between two others.
INSERT INTO ref.DictionaryThemes (Profile, Theme, ThemeOrder)
VALUES (N'Example', N'Record identifiers', 10),
       (N'Example', N'Demography', 20),
       (N'Example', N'Diagnoses/Procedures', 30);

-- 5. A dataset of the profile: its code, full name and table. DOI is NULL
--    until the dataset has one.
INSERT INTO ref.DictionaryDatasets
    (Profile, DatasetCode, FullName, DatabaseName, SchemaName, TableName, DOI, Version)
VALUES (N'Example', N'APC', N'Admitted Patient Care', N'Analysis', N'dbo', N'APC',
        NULL, 1);

-- 6. Its columns, one row each. A column without a row is not in the
--    profile. ColumnAction is INCLUDE unless given.
INSERT INTO ref.DictionaryColumns (Profile, DatasetCode, ColumnName, Theme, CaveatCode)
VALUES (N'Example', N'APC', N'APCEpisodeNumber', N'Record identifiers', NULL),
       (N'Example', N'APC', N'AgeDays', N'Demography', N'detail');

-- A column whose value labels are hidden, and one kept but left out
INSERT INTO ref.DictionaryColumns
    (Profile, DatasetCode, ColumnName, Theme, ValueAction, ColumnAction)
VALUES (N'Example', N'APC', N'AdmittingWard', N'Presentation details',
        N'SUPPRESS', N'INCLUDE'),
       (N'Example', N'APC', N'ATSIStatus', N'Demography',
        NULL, N'EXCLUDE');

-- An alias renames a column, and DescriptionAlias replaces its label
INSERT INTO ref.DictionaryColumns
    (Profile, DatasetCode, ColumnName, Theme, ColumnAlias, DescriptionAlias)
VALUES (N'Example', N'APC', N'Diagnosis1', N'Diagnoses/Procedures',
        N'Diagnosis1', N'Primary diagnosis');

-- An array: Diagnosis2 to Diagnosis100, one row each, shown as one row,
-- Diagnosis2-100. Its members share the alias, description and settings.
INSERT INTO ref.DictionaryColumns
    (Profile, DatasetCode, ColumnName, Theme, ColumnAlias, DescriptionAlias)
SELECT N'Example', N'APC', CONCAT(N'Diagnosis', n), N'Diagnoses/Procedures',
       N'Diagnosis2-100', N'Secondary diagnoses'
FROM (SELECT TOP (99) ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) + 1 AS n
      FROM sys.all_columns) AS numbers;

-- A column added to the table later is left out until it has a row. To add
-- Diagnosis101 to the array, add its row and rename the alias of the others.
INSERT INTO ref.DictionaryColumns
    (Profile, DatasetCode, ColumnName, Theme, ColumnAlias, DescriptionAlias)
VALUES (N'Example', N'APC', N'Diagnosis101', N'Diagnoses/Procedures',
        N'Diagnosis2-101', N'Secondary diagnoses');
UPDATE ref.DictionaryColumns
SET ColumnAlias = N'Diagnosis2-101'
WHERE Profile = N'Example' AND DatasetCode = N'APC'
  AND ColumnAlias = N'Diagnosis2-100';

-- 7. The dataset information table, in NoteOrder
INSERT INTO ref.DictionaryNotes (Profile, DatasetCode, NoteOrder, Aspect, Issue, Comment)
VALUES (N'Example', N'APC', 1, N'Dataset Description', NULL,
        N'Inpatient separations from the public hospitals of the ACT.'),
       (N'Example', N'APC', 2, N'Data Quality Statement', N'Dates and times',
        N'A note on how dates and times are recorded.'),
       (N'Example', N'APC', 3, N'Links & Resources', N'METeOR',
        N'https://meteor.aihw.gov.au/');

-- 8. Changing and removing rows. A dataset can be removed only after its
--    columns and notes, and a caveat only while no column uses it.
UPDATE ref.DictionaryColumns
SET ColumnAction = N'INCLUDE'
WHERE Profile = N'Example' AND DatasetCode = N'APC' AND ColumnName = N'ATSIStatus';

DELETE FROM ref.DictionaryNotes
WHERE Profile = N'Example' AND DatasetCode = N'APC' AND NoteOrder = 2;

-- 9. The rows of the profile
SELECT * FROM ref.DictionaryProfiles WHERE Profile = N'Example';
SELECT * FROM ref.DictionarySections WHERE Profile = N'Example' ORDER BY SectionOrder;
SELECT * FROM ref.DictionaryCaveats  WHERE Profile = N'Example';
SELECT * FROM ref.DictionaryThemes   WHERE Profile = N'Example' ORDER BY ThemeOrder;
SELECT * FROM ref.DictionaryDatasets WHERE Profile = N'Example';
SELECT ColumnAlias, COUNT(*) AS Columns, MIN(ColumnName) AS FirstColumn
FROM ref.DictionaryColumns
WHERE Profile = N'Example'
GROUP BY ColumnAlias;
SELECT * FROM ref.DictionaryNotes    WHERE Profile = N'Example' ORDER BY NoteOrder;

ROLLBACK TRANSACTION;
