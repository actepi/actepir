-- inst/sql/dictionary_tables.sql
--
-- Creates the tables that hold data dictionary profiles in Analysis.ref. A
-- profile, such as Linkage, is a set of datasets with, for each, the columns
-- it offers and its notes, and the profile's title, closing sections, caveats
-- and order of themes. Column labels and value labels are not held here: they
-- come from the label tables through the labels lookup, unless a column has a
-- DescriptionAlias.
--
-- Run it in SQL Server Management Studio. It creates only the tables that do
-- not exist, so it can be run again. The rows are then edited in SSMS;
-- dictionary_example.sql shows how.
--
-- The tables, in the order rows are added:
--   DictionaryProfiles  one row per profile: the title lines of its pages
--   DictionarySections  the profile's closing sections, in order
--   DictionaryCaveats   caveats: the symbol shown beside a column, its colour
--                       and its footnote
--   DictionaryThemes    the order in which themes are shown; themes not listed
--                       follow alphabetically, then Uncategorised
--   DictionaryDatasets  the datasets of the profile, and where each table is
--   DictionaryColumns   one row per column of a dataset; a column without a
--                       row is not in the profile
--   DictionaryNotes     the dataset information table, in order
-- Text columns are nvarchar, as the caveat symbols and some notes are not in
-- the database's code page.

USE Analysis;
GO

IF OBJECT_ID(N'ref.DictionaryProfiles', N'U') IS NULL
CREATE TABLE ref.DictionaryProfiles (
    Profile  nvarchar(50)  NOT NULL,
    Title    nvarchar(200) NOT NULL,
    Subtitle nvarchar(200) NULL,
    CONSTRAINT PK_DictionaryProfiles PRIMARY KEY (Profile)
);
GO

IF OBJECT_ID(N'ref.DictionarySections', N'U') IS NULL
CREATE TABLE ref.DictionarySections (
    Profile      nvarchar(50)  NOT NULL,
    SectionOrder int           NOT NULL,
    Heading      nvarchar(200) NOT NULL,
    SectionText  nvarchar(max) NOT NULL,
    CONSTRAINT PK_DictionarySections PRIMARY KEY (Profile, SectionOrder),
    CONSTRAINT FK_DictionarySections_Profiles FOREIGN KEY (Profile)
        REFERENCES ref.DictionaryProfiles (Profile)
);
GO

IF OBJECT_ID(N'ref.DictionaryCaveats', N'U') IS NULL
CREATE TABLE ref.DictionaryCaveats (
    Profile    nvarchar(50)  NOT NULL,
    CaveatCode nvarchar(20)  NOT NULL,
    Symbol     nvarchar(4)   NOT NULL,
    Colour     nchar(7)      NOT NULL,
    CaveatText nvarchar(500) NOT NULL,
    CONSTRAINT PK_DictionaryCaveats PRIMARY KEY (Profile, CaveatCode),
    CONSTRAINT FK_DictionaryCaveats_Profiles FOREIGN KEY (Profile)
        REFERENCES ref.DictionaryProfiles (Profile),
    -- A hex colour, such as #CF1F25
    CONSTRAINT CK_DictionaryCaveats_Colour CHECK (Colour LIKE
        N'#[0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]')
);
GO

IF OBJECT_ID(N'ref.DictionaryThemes', N'U') IS NULL
CREATE TABLE ref.DictionaryThemes (
    Profile    nvarchar(50)  NOT NULL,
    Theme      nvarchar(100) NOT NULL,
    ThemeOrder int           NOT NULL,
    CONSTRAINT PK_DictionaryThemes PRIMARY KEY (Profile, Theme),
    CONSTRAINT UQ_DictionaryThemes_Order UNIQUE (Profile, ThemeOrder),
    CONSTRAINT FK_DictionaryThemes_Profiles FOREIGN KEY (Profile)
        REFERENCES ref.DictionaryProfiles (Profile)
);
GO

IF OBJECT_ID(N'ref.DictionaryDatasets', N'U') IS NULL
CREATE TABLE ref.DictionaryDatasets (
    Profile      nvarchar(50)  NOT NULL,
    DatasetCode  nvarchar(20)  NOT NULL,  -- as APC, in file names and titles
    FullName     nvarchar(200) NOT NULL,
    DatabaseName nvarchar(128) NOT NULL,
    SchemaName   nvarchar(128) NOT NULL,
    TableName    nvarchar(128) NOT NULL,
    DOI          nvarchar(100) NULL,
    Version      int           NOT NULL,  -- the release number in the footer
    Active       bit           NOT NULL
        CONSTRAINT DF_DictionaryDatasets_Active DEFAULT (1),
    CONSTRAINT PK_DictionaryDatasets PRIMARY KEY (Profile, DatasetCode),
    CONSTRAINT UQ_DictionaryDatasets_Table
        UNIQUE (Profile, DatabaseName, SchemaName, TableName),
    CONSTRAINT FK_DictionaryDatasets_Profiles FOREIGN KEY (Profile)
        REFERENCES ref.DictionaryProfiles (Profile)
);
GO

-- Columns sharing a ColumnAlias are shown as one row under the alias, with
-- the DescriptionAlias in place of the variable label; each member keeps its
-- own row here. ValueAction SUPPRESS hides a column's value labels. Comments
-- and Other are internal notes, never shown.
IF OBJECT_ID(N'ref.DictionaryColumns', N'U') IS NULL
CREATE TABLE ref.DictionaryColumns (
    Profile          nvarchar(50)  NOT NULL,
    DatasetCode      nvarchar(20)  NOT NULL,
    ColumnName       nvarchar(128) NOT NULL,
    Theme            nvarchar(100) NULL,
    ColumnAlias      nvarchar(128) NULL,
    DescriptionAlias nvarchar(500) NULL,
    ColumnAction     nvarchar(7)   NOT NULL
        CONSTRAINT DF_DictionaryColumns_ColumnAction DEFAULT (N'INCLUDE'),
    ValueAction      nvarchar(8)   NULL,
    CaveatCode       nvarchar(20)  NULL,
    Comments         nvarchar(500) NULL,
    Other            nvarchar(500) NULL,
    CONSTRAINT PK_DictionaryColumns PRIMARY KEY (Profile, DatasetCode, ColumnName),
    CONSTRAINT FK_DictionaryColumns_Datasets FOREIGN KEY (Profile, DatasetCode)
        REFERENCES ref.DictionaryDatasets (Profile, DatasetCode),
    CONSTRAINT FK_DictionaryColumns_Caveats FOREIGN KEY (Profile, CaveatCode)
        REFERENCES ref.DictionaryCaveats (Profile, CaveatCode),
    CONSTRAINT CK_DictionaryColumns_ColumnAction
        CHECK (ColumnAction IN (N'INCLUDE', N'EXCLUDE')),
    CONSTRAINT CK_DictionaryColumns_ValueAction
        CHECK (ValueAction IN (N'SUPPRESS'))
);
GO

-- Aspect groups the rows, such as Dataset Description or Links & Resources;
-- Comment is the text shown
IF OBJECT_ID(N'ref.DictionaryNotes', N'U') IS NULL
CREATE TABLE ref.DictionaryNotes (
    Profile     nvarchar(50)  NOT NULL,
    DatasetCode nvarchar(20)  NOT NULL,
    NoteOrder   int           NOT NULL,
    Aspect      nvarchar(100) NOT NULL,
    Issue       nvarchar(200) NULL,
    Comment     nvarchar(max) NOT NULL,
    CONSTRAINT PK_DictionaryNotes PRIMARY KEY (Profile, DatasetCode, NoteOrder),
    CONSTRAINT FK_DictionaryNotes_Datasets FOREIGN KEY (Profile, DatasetCode)
        REFERENCES ref.DictionaryDatasets (Profile, DatasetCode)
);
GO
