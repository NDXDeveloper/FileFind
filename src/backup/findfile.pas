{
  FileFind - Advanced File Search Utility
  Author: Nicolas DEOUX <NDXDev@gmail.com>
  Description:
    This program is an advanced file search utility designed to efficiently locate files based on various criteria such as name, size, attributes, and modification dates.

  License:
    This program is licensed under the GNU General Public License v3 (GPLv3). It can be freely redistributed and/or modified under the terms of the GPLv3.

  Disclaimer:
    Distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    For more details, refer to <https://www.gnu.org/licenses/>.
}

unit FindFile;

interface

uses
  Classes, SysUtils, DateUtils
  {$IFDEF MSWINDOWS}
  , Windows
  {$ELSE}
  , BaseUnix
  {$ENDIF};

type

  // Enum representing different file attributes
  TFileAttributes = (faReadOnly, faHidden, faSysFile, faDirectory, faArchive);

  // Set of file attributes used for filtering
  TSetOfFileAttributes = set of TFileAttributes;

  // File size type for compatibility with large files
  TFileSize = Int64;

  // Options for content-based searching
  TContentSearchOption = (csoCaseSensitive, csoWholeWord, csoNegate);

  // Set of options to define content search behavior
  TContentSearchOptions = set of TContentSearchOption;

  // Enum defining how folders should be ignored during a search
  TFolderIgnore = (fiNone, fiJustThis, fiJustSubfolders, fiThisAndSubfolders);

  // Event type for notifying when a folder is encountered during the search
  TFolderChangeEvent = procedure(Sender: TObject; const Folder: String; var IgnoreFolder: TFolderIgnore) of object;

  // Record to store detailed information about a file or folder
  TFileDetails = record
    Location: String;        // Path of the file
    Name: TFileName;         // Name of the file
    Size: TFileSize;         // Size of the file
    CreatedTime: TDateTime;  // File creation date
    ModifiedTime: TDateTime; // File last modification date
    AccessedTime: TDateTime; // File last accessed date
    IsDirectory : Boolean;   // Indicates whether the item is a directory
  end;


  // Event type for handling matching files found during the search
  TFileMatchEvent = procedure(Sender: TObject; const FileInfo: TFileDetails) of object;

  // Class to handle criteria based on file attributes
  TAttributesCriteria = class(TPersistent)
  private
    FRequired: TSetOfFileAttributes;  // Set of required attributes
    FExcluded: TSetOfFileAttributes;  // Set of excluded attributes
  public
    constructor Create;  // Constructor to initialize the sets
    procedure Assign(Source: TPersistent); override; // Assigns values from another instance
    procedure Clear; virtual; // Resets the criteria to default
    function Matches(const Path: String;const SearchRec: TSearchRec): Boolean; // Checks if a file matches the criteria
    property Required: TSetOfFileAttributes read FRequired write FRequired; // Get/Set required attributes
    property Excluded: TSetOfFileAttributes read FExcluded write FExcluded; // Get/Set excluded attributes
  end;

  // Class to handle criteria based on file timestamps
  TDateTimeCriteria = class(TPersistent)
  private
    FCreatedBefore: TDateTime;  // Upper limit for file creation time
    FCreatedAfter: TDateTime;   // Lower limit for file creation time
    FModifiedBefore: TDateTime; // Upper limit for file modification time
    FModifiedAfter: TDateTime;  // Lower limit for file modification time
    FAccessedBefore: TDateTime; // Upper limit for file access time
    FAccessedAfter: TDateTime;  // Lower limit for file access time
  public
    procedure Assign(Source: TPersistent); override; // Assigns values from another instance
    procedure Clear; virtual; // Resets the criteria to default
    function Matches(const Created, Modified, Accessed: TDateTime): Boolean; // Checks if a file matches the criteria

    property CreatedBefore: TDateTime read FCreatedBefore write FCreatedBefore; // Get/Set upper creation time
    property CreatedAfter: TDateTime read FCreatedAfter write FCreatedAfter; // Get/Set lower creation time
    property ModifiedBefore: TDateTime read FModifiedBefore write FModifiedBefore; // Get/Set upper modification time
    property ModifiedAfter: TDateTime read FModifiedAfter write FModifiedAfter; // Get/Set lower modification time
    property AccessedBefore: TDateTime read FAccessedBefore write FAccessedBefore; // Get/Set upper access time
    property AccessedAfter: TDateTime read FAccessedAfter write FAccessedAfter; // Get/Set lower access time
  end;

  // Class to handle criteria based on file size
  TSizeCriteria = class(TPersistent)
  private
    FMinSize: TFileSize; // Minimum file size
    FMaxSize: TFileSize; // Maximum file size
  public
    procedure Assign(Source: TPersistent); override; // Assigns values from another instance
    procedure Clear; virtual; // Resets the criteria to default
    function Matches(Size: TFileSize): Boolean; // Checks if a file matches the size criteria

    property MinSize: TFileSize read FMinSize write FMinSize; // Get/Set minimum file size
    property MaxSize: TFileSize read FMaxSize write FMaxSize; // Get/Set maximum file size
  end;

  // Class for handling criteria related to file content
  TContentCriteria = class(TPersistent)
  private
    FPhrase: String; // Search phrase to look for in file content
    FOptions: TContentSearchOptions; // Options that define how the content should be searched
  public
    procedure Assign(Source: TPersistent); override; // Copies the content criteria from another instance
    procedure Clear; virtual; // Resets the content criteria to default values
    function Matches(const FileName: String): Boolean; // Checks if the file content matches the defined phrase and options

    property Phrase: String read FPhrase write FPhrase; // The search phrase
    property Options: TContentSearchOptions read FOptions write FOptions; // Search options (e.g., case sensitivity)
  end;

  // Class for managing all search criteria
  TSearchCriteria = class(TPersistent)
  private
    FAttributes: TAttributesCriteria; // Criteria related to file attributes
    FDateTime: TDateTimeCriteria; // Criteria related to file dates and times
    FSize: TSizeCriteria; // Criteria related to file size
    FContent: TContentCriteria; // Criteria related to file content
    FFolder: String; // Starting folder for the search
    FRecursive: Boolean; // Indicates whether the search should be recursive
    FFileName: String; // File name pattern for the search
    fFilters: TStringList; // List of custom filters for the search
    procedure SetFilters(Value: TStringList); // Updates the filters list
  public
    constructor Create; // Initializes default search criteria
    destructor Destroy; override; // Frees memory and resources

    procedure Assign(Source: TPersistent); override; // Copies the search criteria from another instance
    procedure Clear; virtual; // Resets the search criteria to default values
    function MatchesFilters(const Folder, FileName: String): Boolean; // Checks if a file matches the defined filters

    property Attributes: TAttributesCriteria read FAttributes write FAttributes; // Access to attribute criteria
    property DateTime: TDateTimeCriteria read FDateTime write FDateTime; // Access to date/time criteria
    property Size: TSizeCriteria read FSize write FSize; // Access to size criteria
    property Content: TContentCriteria read FContent write FContent; // Access to content criteria
    property Folder: String read FFolder write FFolder; // Folder for the search
    property Recursive: Boolean read FRecursive write FRecursive; // Recursive search flag
    property FileName: String read FFileName write FFileName; // File name pattern
  published
    property Filters: TStringList read fFilters write SetFilters; // Custom filters for search
  end;

  // Forward declaration for TFindFile class
  TFindFile = class; // Forward declaration

  // Thread class for performing the search in the background
  TFindFileThread = class(TThread)
  private
    FOwner: TFindFile; // Reference to the owning TFindFile instance
    FileDetails: TFileDetails;  // Stores file details during the search
    FCurrentFolder: String; // Current folder being searched
    FIgnoreFolder: TFolderIgnore; // Specifies how to handle the current folder
    procedure DoFileMatch; // Notifies about a matching file
    procedure DoFolderChange; // Notifies when a folder is accessed
    procedure DoSearchBegin; // Notifies at the start of the search
    procedure DoSearchFinish; // Notifies at the end of the search
  protected
    procedure Execute; override; // Main execution logic for the thread
    procedure SearchIn(const Path: String); // Performs the search within a specific folder
  public
    constructor Create(AOwner: TFindFile); // Initializes the thread with the owning TFindFile instance
  end;

  // Class for managing the file search process
  TFindFile = class
  private
    FCriteria: TSearchCriteria; // Search criteria
    FThread: TFindFileThread; // Thread performing the search
    FOnFileMatch: TFileMatchEvent; // Event triggered for each matching file
    FOnFolderChange: TFolderChangeEvent; // Event triggered for folder changes
    FOnSearchBegin: TNotifyEvent; // Event triggered at the start of the search
    FOnSearchFinish: TNotifyEvent; // Event triggered at the end of the search
    FAborted: Boolean; // Indicates if the search was aborted
    FBusy: Boolean; // Indicates if a search is in progress
    FFileCount: Integer; // Total number of files found
    FFoldersSearchedCount: Integer; // Total number of folders searched
    FStartTime: QWord; // Timestamp of when the search started
    FSearchTime : Double; // Duration of the search
    procedure DoSearchBegin; // Internal method to trigger the search begin event
    procedure DoSearchFinish; // Internal method to trigger the search finish event
    procedure IncrementFileCount; // Increments the file count
    procedure IncrementFoldersSearchedCount; // Increments the folder count
  public
    constructor Create; // Initializes the search manager
    destructor Destroy; override; // Frees memory and resources

    procedure Execute(const StartPath: String; Recursive: Boolean); // Starts the search process
    procedure Abort; // Aborts the search process

    property Criteria: TSearchCriteria read FCriteria write FCriteria; // Access to search criteria
    property OnFileMatch: TFileMatchEvent read FOnFileMatch write FOnFileMatch; // Event for file matches
    property OnFolderChange: TFolderChangeEvent read FOnFolderChange write FOnFolderChange; // Event for folder changes
    property OnSearchBegin: TNotifyEvent read FOnSearchBegin write FOnSearchBegin; // Event for search start
    property OnSearchFinish: TNotifyEvent read FOnSearchFinish write FOnSearchFinish; // Event for search end

    property FileCount: Integer read FFileCount; // Number of files found
    property FoldersSearchedCount: Integer read FFoldersSearchedCount; // Number of folders searched
    property ElapsedTime: double read FSearchTime; // Time elapsed during the search
    property Aborted: Boolean read FAborted; // Indicates if the search was aborted
    property Busy: Boolean read FBusy; // Indicates if a search is in progress
  end;

  // Utility functions for file operations
  function FormatFileSize(const Size: TFileSize): String; // Formats a file size for display
  function PosEx(const SubStr, S: string; Offset: Integer): Integer; // Finds a substring in a string starting at a specific offset
  function MatchesContent(const FileName, SearchPhrase: String; Options: TContentSearchOptions): Boolean; // Checks if file content matches a search phrase
  function MatchesMask(const FileName, Mask: String): Boolean; // Checks if a file name matches a pattern
  function WildcardMatches(S, M: PChar): Boolean; // Checks if a string matches a wildcard pattern

implementation

{$IFDEF MSWINDOWS}
function IsReadOnly(const Attr: DWORD): Boolean;
begin
  Result := (Attr and FILE_ATTRIBUTE_READONLY) <> 0;
end;

function IsHidden(const Attr: DWORD): Boolean;
begin
  Result := (Attr and FILE_ATTRIBUTE_HIDDEN) <> 0;
end;

function IsSystemFile(const Attr: DWORD): Boolean;
begin
  Result := (Attr and FILE_ATTRIBUTE_SYSTEM) <> 0;
end;

function IsDirectory(const Attr: DWORD): Boolean;
begin
  Result := (Attr and FILE_ATTRIBUTE_DIRECTORY) <> 0;
end;

function GetFileAttributesWin(const FileName: String): DWORD;
begin
  Result := GetFileAttributes(PChar(FileName));
end;
{$ENDIF}

// Formats a file size into a human-readable string
function FormatFileSize(const Size: TFileSize): String;
const
  KB = 1024;      // 1 Kilobyte in bytes
  MB = 1024 * KB; // 1 Megabyte in bytes
  GB = 1024 * MB; // 1 Gigabyte in bytes
begin
  if Size < KB then
    Result := FormatFloat('#,##0 Bytes', Size) // Display size in bytes
  else if Size < MB then
    Result := FormatFloat('#,##0.0 KB', Size / KB)  // Display size in KB
  else if Size < GB then
    Result := FormatFloat('#,##0.0 MB', Size / MB)  // Display size in MB
  else
    Result := FormatFloat('#,##0.0 GB', Size / GB); // Display size in GB
end;


{ TAttributesCriteria }

// Constructor for initializing required and excluded attributes
constructor TAttributesCriteria.Create;
begin
  inherited Create;
  FRequired := []; // Initialize required attributes set
  FExcluded := []; // Initialize excluded attributes set
end;

// Copies the attributes criteria from another instance
procedure TAttributesCriteria.Assign(Source: TPersistent);
begin
  if Source is TAttributesCriteria then
  begin
    FRequired := TAttributesCriteria(Source).FRequired; // Copy required attributes
    FExcluded := TAttributesCriteria(Source).FExcluded; // Copy excluded attributes
  end
  else
    inherited Assign(Source); // Fallback to parent behavior
end;

// Clears all required and excluded attributes
procedure TAttributesCriteria.Clear;
begin
  FRequired := []; // Clear required attributes
  FExcluded := []; // Clear excluded attributes
end;

// Checks if the file matches the specified attributes criteria
function TAttributesCriteria.Matches(const Path: String; const SearchRec: TSearchRec): Boolean;
var
  FileAttributes: TSetOfFileAttributes; // Set to store file's attributes
  {$IFDEF UNIX}
  StatBuf: BaseUnix.Stat; // Structure to store file metadata
  {$ENDIF}
begin
  // Initialize the attributes set
  FileAttributes := [];
  {$IFDEF MSWINDOWS}
  // Retrieve attributes using Windows API
  if (GetFileAttributesWin(Path) <> INVALID_FILE_ATTRIBUTES) then
  begin
    if IsReadOnly(SearchRec.Attr) then
      Include(FileAttributes, faReadOnly);
    if IsHidden(SearchRec.Attr) then
      Include(FileAttributes, faHidden);
    if IsSystemFile(SearchRec.Attr) then
      Include(FileAttributes, faSysFile);
    if IsDirectory(SearchRec.Attr) then
      Include(FileAttributes, faDirectory);
  end;
  {$ELSE}
  // Check if the file is hidden
  if (SearchRec.Name <> '') and (SearchRec.Name[1] = '.') then
    Include(FileAttributes, faHidden);

  // Check if the file is read-only
  if FpStat(Path, StatBuf) = 0 then
    if (StatBuf.st_mode and S_IWUSR) = 0 then
      Include(FileAttributes, faReadOnly);

  // Check if the file is a directory
  if (SearchRec.Attr and SysUtils.faDirectory) <> 0 then
    Include(FileAttributes, faDirectory);

  // Add system attributes using SysUtils
  if FpStat(Path, StatBuf) = 0 then
     // Check for system files (custom logic for Unix)
    if (Pos('/etc/', Path) = 1) or (Pos('/usr/', Path) = 1) or
       (Pos('/bin/', Path) = 1) or (Pos('/sbin/', Path) = 1) then
      Include(FileAttributes, faSysFile);
  {$ENDIF}

  if (SearchRec.Attr and SysUtils.faArchive) <> 0 then
    Include(FileAttributes, faArchive);

  // Check if the file meets the required attributes
  if faReadOnly in FRequired then
    if not (faReadOnly in FileAttributes) then
      Exit(False);
  if faHidden in FRequired then
    if not (faHidden in FileAttributes) then
      Exit(False);
  if faSysFile in FRequired then
    if not (faSysFile in FileAttributes) then
      Exit(False);
  if faDirectory in FRequired then
    if not (faDirectory in FileAttributes) then
      Exit(False);

  if faArchive in FRequired then
    if not (faArchive in FileAttributes) then
      Exit(False);

  // Check if the file does not meet the excluded attributes
  if faReadOnly in FExcluded then
    if faReadOnly in FileAttributes then
      Exit(False);
  if faHidden in FExcluded then
    if faHidden in FileAttributes then
      Exit(False);
  if faSysFile in FExcluded then
    if faSysFile in FileAttributes then
      Exit(False);
  if faDirectory in FExcluded then
    if faDirectory in FileAttributes then
      Exit(False);
  if faArchive in FExcluded then
    if faArchive in FileAttributes then
      Exit(False);

  // If the file passes all checks, it matches the criteria
  Result := True;
end;


{ TDateTimeCriteria }

// Copies the date and time criteria from another instance
procedure TDateTimeCriteria.Assign(Source: TPersistent);
begin
  if Source is TDateTimeCriteria then
  begin
    FCreatedBefore := TDateTimeCriteria(Source).FCreatedBefore; // Copy "created before" date
    FCreatedAfter := TDateTimeCriteria(Source).FCreatedAfter; // Copy "created after" date
    FModifiedBefore := TDateTimeCriteria(Source).FModifiedBefore; // Copy "modified before" date
    FModifiedAfter := TDateTimeCriteria(Source).FModifiedAfter; // Copy "modified after" date
    FAccessedBefore := TDateTimeCriteria(Source).FAccessedBefore; // Copy "accessed before" date
    FAccessedAfter := TDateTimeCriteria(Source).FAccessedAfter; // Copy "accessed after" date
  end
  else
    inherited Assign(Source); // Fallback to parent behavior
end;

// Clears all date and time criteria
procedure TDateTimeCriteria.Clear;
begin
  FCreatedBefore := 0;  // Reset "created before" criterion
  FCreatedAfter := 0;   // Reset "created after" criterion
  FModifiedBefore := 0; // Reset "modified before" criterion
  FModifiedAfter := 0;  // Reset "modified after" criterion
  FAccessedBefore := 0; // Reset "accessed before" criterion
  FAccessedAfter := 0;  // Reset "accessed after" criterion
end;

// Checks if the provided creation, modification, and access times match the criteria
function TDateTimeCriteria.Matches(const Created, Modified, Accessed: TDateTime): Boolean;
begin
  Result := True; // Assume the times match the criteria by default

  // Check the "created before" criterion
  if (FCreatedBefore <> 0) and (Created >= FCreatedBefore) then Exit(False);

  // Check the "created after" criterion
  if (FCreatedAfter <> 0) and (Created <= FCreatedAfter) then Exit(False);

  // Check the "modified before" criterion
  if (FModifiedBefore <> 0) and (Modified >= FModifiedBefore) then Exit(False);

  // Check the "modified after" criterion
  if (FModifiedAfter <> 0) and (Modified <= FModifiedAfter) then Exit(False);

  // Check the "accessed before" criterion
  if (FAccessedBefore <> 0) and (Accessed >= FAccessedBefore) then Exit(False);

  // Check the "accessed after" criterion
  if (FAccessedAfter <> 0) and (Accessed <= FAccessedAfter) then Exit(False);
end;

{ TSizeCriteria }

// Assigns size criteria from another instance
procedure TSizeCriteria.Assign(Source: TPersistent);
begin
  if Source is TSizeCriteria then
  begin
    FMinSize := TSizeCriteria(Source).FMinSize; // Copy minimum size
    FMaxSize := TSizeCriteria(Source).FMaxSize; // Copy maximum size
  end
  else
    inherited Assign(Source); // Fallback to parent behavior
end;

// Clears all size criteria
procedure TSizeCriteria.Clear;
begin
  FMinSize := 0; // Reset minimum size
  FMaxSize := 0; // Reset maximum size
end;

// Checks if the file size matches the criteria
function TSizeCriteria.Matches(Size: TFileSize): Boolean;
begin
  Result := ((FMinSize = 0) or (Size >= FMinSize)) and  // Check minimum size
            ((FMaxSize = 0) or (Size <= FMaxSize));     // Check maximum size
end;

{ TContentCriteria }

// Assigns content search criteria from another instance
procedure TContentCriteria.Assign(Source: TPersistent);
begin
  if Source is TContentCriteria then
  begin
    FPhrase := TContentCriteria(Source).FPhrase;   // Copy the search phrase
    FOptions := TContentCriteria(Source).FOptions; // Copy search options
  end
  else
    inherited Assign(Source); // Fallback to parent behavior
end;

// Clears all content search criteria
procedure TContentCriteria.Clear;
begin
  FPhrase := ''; // Reset the search phrase
  FOptions := []; // Reset the search options
end;

// Checks if the file content matches the criteria
function TContentCriteria.Matches(const FileName: String): Boolean;
var
  Content: TStringList;
begin
  Result := False; // Assume the content does not match by default

  // Exit if no search phrase is defined
  if FPhrase = '' then Exit;
  Content := TStringList.Create; // Load the file content
  try
    Content.LoadFromFile(FileName);
    if csoCaseSensitive in FOptions then
      Result := Pos(FPhrase, Content.Text) > 0 // Case-sensitive search
    else
      Result := Pos(LowerCase(FPhrase), LowerCase(Content.Text)) > 0; // Case-insensitive search
  finally
    Content.Free; // Free the memory allocated for the content
  end;
end;

{ TSearchCriteria }

// Constructor to initialize all search criteria
constructor TSearchCriteria.Create;
begin
  FAttributes := TAttributesCriteria.Create; // Initialize attributes criteria
  FDateTime := TDateTimeCriteria.Create;     // Initialize date and time criteria
  FSize := TSizeCriteria.Create;             // Initialize size criteria
  FContent := TContentCriteria.Create;       // Initialize content criteria
  FFolder := '/';                            // Default search folder
  FRecursive := True;                        // Default to recursive search
  FFileName := '*';                          // Default file name pattern (all files)
  fFilters := TStringList.Create;            // Initialize filters list
  fFilters.CaseSensitive := False;           // Filters are case-insensitive by default
end;

// Destructor to free allocated memory
destructor TSearchCriteria.Destroy;
begin
  FAttributes.Free; // Free attributes criteria
  FDateTime.Free;   // Free date and time criteria
  FSize.Free;       // Free size criteria
  FContent.Free;    // Free content criteria
  fFilters.Free;    // Free filters list
  inherited Destroy;
end;

// Assigns values from another TSearchCriteria instance to this instance
procedure TSearchCriteria.Assign(Source: TPersistent);
begin
  if Source is TSearchCriteria then
  begin
    FAttributes.Assign(TSearchCriteria(Source).Attributes); // Copy attribute criteria
    FDateTime.Assign(TSearchCriteria(Source).DateTime);     // Copy date and time criteria
    FSize.Assign(TSearchCriteria(Source).Size);             // Copy size criteria
    FContent.Assign(TSearchCriteria(Source).Content);       // Copy content criteria
    FFolder := Trim(TSearchCriteria(Source).Folder);        // Copy folder path
    FRecursive := TSearchCriteria(Source).Recursive;        // Copy recursive option
    FFileName := Trim(TSearchCriteria(Source).FileName);    // Copy file name pattern
    Filters := TSearchCriteria(Source).Filters;             // Copy filters
  end
  else
    inherited Assign(Source); // If Source is not TSearchCriteria, use the base implementation
end;

// Clears all criteria values
procedure TSearchCriteria.Clear;
begin
  FAttributes.Clear; // Clear attribute criteria
  FDateTime.Clear;   // Clear date and time criteria
  FSize.Clear;       // Clear size criteria
  FContent.Clear;    // Clear content criteria
  FFolder := '';     // Reset folder path
  FRecursive := False; // Disable recursive search
  Filters.Clear;     // Clear filters
end;

// Sets the filters for file search
procedure TSearchCriteria.SetFilters(Value: TStringList);
begin
  fFilters.Assign(Value); // Copy the provided filters into the internal list
end;

// Checks if the string S matches the pattern M using wildcard rules
function WildcardMatches(S, M: PChar): Boolean;
begin
  Result := False; // Default to no match
  while (S^ <> #0) and (M^ <> #0) and (M^ <> '*') do
  begin
    // '?' matches any single character; otherwise, characters must be identical
    if (M^ <> '?') and (M^ <> S^) then
      Exit;
    Inc(S);
    Inc(M);
  end;

  // Handle the '*' wildcard (matches zero or more characters)
  if M^ = '*' then
  begin
    Inc(M);
    while (S^ <> #0) do
    begin
      if WildcardMatches(S, M) then
      begin
        Result := True;
        Exit;
      end;
      Inc(S);
    end;
    Result := (M^ = #0); // True if '*' is at the end of the pattern
  end
  else
    Result := (S^ = #0) and (M^ = #0); // Both strings must end at the same time
end;

// Checks if a file matches the defined filters
function TSearchCriteria.MatchesFilters(const Folder, FileName: String): Boolean;
var
  I: Integer;
  Path: String;
  Mask: PChar;
begin
  Result := True; // By default, the file is accepted

  // If there are no filters, all files are accepted
  if Filters.Count <> 0 then
  begin
    // Construct the full file path
    Path := IncludeTrailingPathDelimiter(Folder) + FileName;

    // If the file has no extension, append a dot to facilitate matching
    if ExtractFileExt(FileName) = '' then
      Path := Path + '.';

    // Iterate through all filters
    for I := 0 to Filters.Count - 1 do
    begin
      Mask := PChar(Filters[I]);

      // Inclusion filter (starts with '>')
      if Mask^ = '>' then
      begin
        Inc(Mask); // Skip the '>' character
        if WildcardMatches(PChar(Path), Mask) then
        begin
          Result := True; // File matches an inclusion filter
          Exit;           // No need to check further
        end;
      end
      // Exclusion filter (starts with '<')
      else if Mask^ = '<' then
      begin
        Inc(Mask); // Skip the '<' character
        if WildcardMatches(PChar(Path), Mask) then
        begin
          Result := False; // File matches an exclusion filter
          Exit;            // Reject the file immediately
        end;
      end;
    end;

    // If no inclusion filter matches, reject the file
    Result := False;
  end;
end;

{ TFindFileThread }

// Constructor to initialize the thread with its owner
constructor TFindFileThread.Create(AOwner: TFindFile);
begin
  inherited Create(True); // Create the thread in a suspended state
  FOwner := AOwner;       // Link the thread to its owner
end;

// Trigger the file match event in the main thread
procedure TFindFileThread.DoFileMatch;
begin
  if Assigned(FOwner.FOnFileMatch) then
      FOwner.FOnFileMatch(FOwner, FileDetails); // Pass the file details to the event handler
end;

// Trigger the folder change event in the main thread
procedure TFindFileThread.DoFolderChange;
begin
  if Assigned(FOwner.FOnFolderChange) then
    FOwner.FOnFolderChange(FOwner, FCurrentFolder, FIgnoreFolder); // Pass the current folder and ignore status
end;

// Trigger the search begin event in the main thread
procedure TFindFileThread.DoSearchBegin;
begin
  if Assigned(FOwner.FOnSearchBegin) then
    FOwner.FOnSearchBegin(FOwner); // Notify the owner that the search has started
end;

// Trigger the search finish event in the main thread
procedure TFindFileThread.DoSearchFinish;
begin
  if Assigned(FOwner.FOnSearchFinish) then
    FOwner.FOnSearchFinish(FOwner); // Notify the owner that the search has finished
  FOwner.FBusy := False;            // Mark the search as no longer busy
end;

{ PosEx }
// Finds the position of a substring in a string, starting from a specified offset
function PosEx(const SubStr, S: string; Offset: Integer): Integer;
var
  FoundPos: Integer;
begin
  // Return 0 if the offset is out of range
  if (Offset > Length(S)) or (Offset < 1) then
    Exit(0);

  // Search for the substring starting from the offset
  FoundPos := Pos(SubStr, Copy(S, Offset, MaxInt));
  if FoundPos > 0 then
    Result := FoundPos + Offset - 1 // Adjust the position relative to the original string
  else
    Result := 0; // Substring not found
end;

// Function to check if the content of a file matches a search phrase with given options
function MatchesContent(const FileName, SearchPhrase: String; Options: TContentSearchOptions): Boolean;
var
  Content: TStringList; // To store the file content
  AdjustedContent, AdjustedPhrase: String; // Adjusted strings for case sensitivity
  StartPos, EndPos: Integer; // Positions for searching phrases
begin
  Result := False; // Default to no match

  // Load the file content into a TStringList
  Content := TStringList.Create;
  try
    try
      Content.LoadFromFile(FileName); // Load the file content
    except
      Exit; // Skip files that cannot be accessed
    end;

    // Prepare content and phrase for comparison
    AdjustedContent := Content.Text;
    AdjustedPhrase := SearchPhrase;

    // Convert to lowercase if case sensitivity is not required
    if not (csoCaseSensitive in Options) then
    begin
      AdjustedContent := LowerCase(AdjustedContent);
      AdjustedPhrase := LowerCase(AdjustedPhrase);
    end;

    if csoWholeWord in Options then
    begin
      // Search for whole words
      StartPos := Pos(AdjustedPhrase, AdjustedContent);
      while StartPos > 0 do
      begin
        EndPos := StartPos + Length(AdjustedPhrase) - 1;

        // Ensure boundaries of the word
        if ((StartPos = 1) or (AdjustedContent[StartPos - 1] in [' ', '.', ',', '!', '?', #10, #13])) and
           ((EndPos = Length(AdjustedContent)) or (AdjustedContent[EndPos + 1] in [' ', '.', ',', '!', '?', #10, #13])) then
        begin
          Result := True;
          Break; // Exit once a match is found
        end;

        // Continue searching for additional occurrences
        StartPos := PosEx(AdjustedPhrase, AdjustedContent, StartPos + 1);
      end;
    end
    else
    begin
      // Partial content match
      Result := Pos(AdjustedPhrase, AdjustedContent) > 0;
    end;

    // Apply negate option if set
    if csoNegate in Options then
    begin
         if Result=True then Result := not Result;
    end;
  finally
    Content.Free; // Free the TStringList object
  end;
end;

// Function to check if a file name matches a pattern (with wildcards like * and ?)
function MatchesMask(const FileName, Mask: String): Boolean;
var
  CleanFileName, CleanMask: String; // Normalized file name and mask
  i, j: Integer;                    // Indexes for file name and mask
  cFile, cMask: Char;               // Current characters being compared
begin
  CleanFileName := LowerCase(Trim(FileName)); // Normalize the file name
  CleanMask := LowerCase(Trim(Mask));         // Normalize the mask

  i := 1; // Initialize index for the file name
  j := 1; // Initialize index for the mask
  while (i <= Length(CleanFileName)) and (j <= Length(CleanMask)) do
  begin
    cFile := CleanFileName[i];
    cMask := CleanMask[j];

    if cMask = '*' then
    begin
      // '*' matches zero or more characters
      if j = Length(CleanMask) then
        Exit(True); // '*' at the end matches everything
      while (i <= Length(CleanFileName)) and not MatchesMask(Copy(CleanFileName, i, Length(CleanFileName)), Copy(CleanMask, j + 1, Length(CleanMask))) do
        Inc(i);
      Exit(i <= Length(CleanFileName));
    end
    else if (cMask = '?') or (cFile = cMask) then
    begin
      // '?' matches a single character or exact match
      Inc(i);
      Inc(j);
    end
    else
      Exit(False); // Characters do not match
  end;

  // Handle remaining '*' characters in the mask
  while (j <= Length(CleanMask)) and (CleanMask[j] = '*') do
    Inc(j); // Ignorer les '*' restants
  Result := (i > Length(CleanFileName)) and (j > Length(CleanMask)); // Match if both are exhausted
end;

// Function to split a string into a TStringList based on a delimiter
function StringListFromString(const Str: String; Delimiter: Char): TStringList;
var
  Item: String;          // Current item being processed
  StartIndex: Integer;   // Start index of the current item
  DelimiterPos: Integer; // Position of the next delimiter
  StrLen: Integer;       // Length of the input string
begin
  Result := TStringList.Create; // Initialize the result
  StrLen := Length(Str);        // Get the length of the input string
  StartIndex := 1;              // Start at the beginning of the string
  repeat
    DelimiterPos := StartIndex; // Start searching from the current position
    while (DelimiterPos <= StrLen) and (Str[DelimiterPos] <> Delimiter) do
      Inc(DelimiterPos); // Move to the next delimiter or end of string
    if StartIndex <> DelimiterPos then
    begin
      // Extract the substring and trim spaces
      Item := Trim(Copy(Str, StartIndex, DelimiterPos - StartIndex));
      // Add the item if it is not empty and not already in the list
      if (Item <> '') and (Result.IndexOf(Item) < 0) then
        Result.Add(Item);
    end;
    // Move to the next section
    StartIndex := DelimiterPos + 1;
  until StartIndex > StrLen; // Stop when the end of the string is reached
end;

// Searches for files and directories in the specified path based on criteria
procedure TFindFileThread.SearchIn(const Path: String);
var
  SearchRec: TSearchRec;       // Holds details of the file or directory being searched
  CleanPath: String;           // Normalized path for searching
  FileMatchesContent: Boolean; // Whether the file content matches search criteria
  {$IFDEF UNIX}
  StatBuf: Stat;               // Holds file metadata
  {$ENDIF}
  FullPath: String;            // Full path of the current file
  StatResult: Integer;         // Result of file stat operation
  Files: TStringList;          // List of filename patterns to search for
  I: Integer;                  // Loop counter

  {$IFDEF WINDOWS}
  FileData: TWin32FileAttributeData;
  CreationTime, LastAccessTime, LastWriteTime: TFileTime;
  LocalCreationTime, LocalAccessTime, LocalWriteTime: TSystemTime;
  {$ENDIF}
begin
  // Exit if the thread is terminated or the search is aborted
  if Terminated then Exit;
  if FOwner.Aborted then Exit;

  // Normalize and prepare the search path
  CleanPath := IncludeTrailingPathDelimiter(Trim(Path));
  FCurrentFolder := CleanPath;
  FIgnoreFolder := fiNone;

  // Notify folder change event
  Synchronize(@DoFolderChange);

  // Skip this folder if it is marked for ignoring
  if FIgnoreFolder in [fiJustThis, fiThisAndSubfolders] then Exit;

  // Increment the count of folders searched
  Synchronize(@FOwner.IncrementFoldersSearchedCount);

  // Split filename patterns by ';'
  Files := StringListFromString(FOwner.Criteria.FileName, ';');
  try
    if Files.Count = 0 then
      Files.Add('*.*'); // Default to all files if no pattern is specified

    // Loop through all patterns
    for I := 0 to Files.Count - 1 do
    begin
      // Start searching in the current directory
      if FindFirst(CleanPath + '*' {Files[I]}, SysUtils.faAnyFile, SearchRec) = 0 then
      try
        repeat
          // Exit if the thread is terminated or the search is aborted
          if Terminated then Exit;
          if FOwner.Aborted then Exit;

          // Skip special entries '.' and '..'
          if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
          begin
            // If the current entry is a directory
            if (SearchRec.Attr and SysUtils.faDirectory) = SysUtils.faDirectory then
            begin
              // Check if the directory name matches the pattern
              if MatchesMask(Trim(SearchRec.Name), Trim(Files[I])) then
              begin
                // Check if directory attributes match
                if not FOwner.Criteria.Attributes.Matches(CleanPath + SearchRec.Name,SearchRec) then
                begin
                  Continue;
                end;

                // Check if directory passes filters
                if not FOwner.Criteria.MatchesFilters('',SearchRec.Name) then Continue;

                // Populate file details for directories
                FileDetails.Location := CleanPath;
                FileDetails.Name := SearchRec.Name;
                FileDetails.IsDirectory:= True;

                // Increment file count
                Synchronize(@FOwner.IncrementFileCount);
                // Notify file match event
                Synchronize(@DoFileMatch);
              end;
            end
            else
            begin
              // If the current entry is a file
              if MatchesMask(Trim(SearchRec.Name), Trim(Files[I])) then
              begin
                FullPath := CleanPath + SearchRec.Name;

                // Check file content if a phrase is specified
                if FOwner.Criteria.Content.Phrase <> '' then
                begin
                  FileMatchesContent := MatchesContent(
                    FullPath,
                    FOwner.Criteria.Content.Phrase,
                    FOwner.Criteria.Content.Options
                  );

                  // Apply negate option
                  if csoNegate in FOwner.Criteria.Content.Options then
                    FileMatchesContent := not FileMatchesContent;

                  // Skip if content does not match
                  if not FileMatchesContent then
                    Continue;
                end;

                // Check file size criteria
                if not ((FOwner.Criteria.Size.MinSize = 0) or (SearchRec.Size  >= FOwner.Criteria.Size.MinSize)) and ((FOwner.Criteria.Size.MaxSize = 0) or (SearchRec.Size <= FOwner.Criteria.Size.MaxSize)) then Continue;

                // Check file attributes
                if not FOwner.Criteria.Attributes.Matches(FullPath,SearchRec) then
                begin
                  Continue;
                end;

                {$IFDEF UNIX}
                // Get file metadata
                StatResult := FpStat(FullPath, StatBuf);
                if StatResult = 0 then
                begin
                  // Populate file details
                  FileDetails.Location := CleanPath;
                  FileDetails.Name := SearchRec.Name;
                  FileDetails.Size := StatBuf.st_size;
                  FileDetails.ModifiedTime := UnixToDateTime(StatBuf.st_mtime);
                  FileDetails.AccessedTime := UnixToDateTime(StatBuf.st_atime);
                  FileDetails.CreatedTime := UnixToDateTime(StatBuf.st_ctime);
                  FileDetails.IsDirectory:= False;

                  // Check date/time criteria
                  if not FOwner.Criteria.DateTime.Matches(FileDetails.CreatedTime,FileDetails.ModifiedTime,FileDetails.AccessedTime) then Continue;

                  // Check if file passes filters
                  if not FOwner.Criteria.MatchesFilters(CleanPath,SearchRec.Name) then Continue;

                  // Increment file count and notify file match event
                  Synchronize(@FOwner.IncrementFileCount);
                  Synchronize(@DoFileMatch);
                end
                else
                begin
                  //WriteLn('Error with FpStat: ', fpgeterrno);
                end;
                {$ELSE}
                // Get file attributes and metadata
                if GetFileAttributesEx(PChar(FullPath), GetFileExInfoStandard, @FileData) then
                begin
                  // Convert file times to local system time
                  FileTimeToLocalFileTime(FileData.ftCreationTime, CreationTime);
                  FileTimeToLocalFileTime(FileData.ftLastAccessTime, LastAccessTime);
                  FileTimeToLocalFileTime(FileData.ftLastWriteTime, LastWriteTime);

                  FileTimeToSystemTime(CreationTime, LocalCreationTime);
                  FileTimeToSystemTime(LastAccessTime, LocalAccessTime);
                  FileTimeToSystemTime(LastWriteTime, LocalWriteTime);

                  // Populate file details
                  FileDetails.Location := CleanPath;
                  FileDetails.Name := SearchRec.Name;
                  FileDetails.Size := (Int64(FileData.nFileSizeHigh) shl 32) or FileData.nFileSizeLow;
                  FileDetails.CreatedTime := SystemTimeToDateTime(LocalCreationTime);
                  FileDetails.AccessedTime := SystemTimeToDateTime(LocalAccessTime);
                  FileDetails.ModifiedTime := SystemTimeToDateTime(LocalWriteTime);
                  FileDetails.IsDirectory := False;

                  // Check date/time criteria
                  if not FOwner.Criteria.DateTime.Matches(FileDetails.CreatedTime, FileDetails.ModifiedTime, FileDetails.AccessedTime) then
                    Continue;

                  // Check if file passes filters
                  if not FOwner.Criteria.MatchesFilters(CleanPath, SearchRec.Name) then
                    Continue;

                  // Increment file count and notify file match event
                  Synchronize(@FOwner.IncrementFileCount);
                  Synchronize(@DoFileMatch);
                end
                else
                begin
                  // Handle error retrieving file attributes
                  // ShowMessage(SysErrorMessage(GetLastError)); // Uncomment for debugging purposes
                end;
                {$ENDIF}

              end;
            end;

          end;

        until FindNext(SearchRec) <> 0; // Continue searching
      finally
        SysUtils.FindClose(SearchRec); // Free search resources
      end;
    end;
  finally
    Files.Free; // Free the pattern list
  end;

  // Recursively search subdirectories if recursive option is enabled
  if FindFirst(IncludeTrailingPathDelimiter(Path) + '*', SysUtils.faAnyFile, SearchRec) = 0 then
  try
    // un autre repeat ici pour les dossiers
    repeat
      if Terminated then Exit;
      if FOwner.Aborted then Exit;
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        if (SearchRec.Attr and SysUtils.faDirectory) = SysUtils.faDirectory then
        begin
          if FOwner.Criteria.Recursive then
            SearchIn(IncludeTrailingPathDelimiter(Path) + SearchRec.Name); // Recurse into subdirectory
        end;
      end;
    until FindNext(SearchRec) <> 0; // Continue searching
  finally
    SysUtils.FindClose(SearchRec); // Free search resources
  end;
end;

{
  procedure TFindFileThread.SearchIn(const Path: String);
var
  SearchRec: TSearchRec;
  CleanPath, FullPath: String;
  Files: TStringList;
  I: Integer;
begin
  if Terminated or FOwner.Aborted then Exit;

  // Nettoyer et standardiser le chemin
  CleanPath := IncludeTrailingPathDelimiter(Trim(Path));
  FCurrentFolder := CleanPath;
  FIgnoreFolder := fiNone;

  // Notifier les changements de dossier uniquement si nécessaire
  if Assigned(FOwner.FOnFolderChange) then
    Synchronize(@DoFolderChange);

  // Si ce dossier est à ignorer, abandonner maintenant
  if FIgnoreFolder in [fiJustThis, fiThisAndSubfolders] then Exit;

  // Compter le dossier actuel
  Synchronize(@FOwner.IncrementFoldersSearchedCount);

  // Diviser les patterns de fichiers
  Files := StringListFromString(FOwner.Criteria.FileName, ';');
  try
    if Files.Count = 0 then
      Files.Add('*.*'); // Rechercher tous les fichiers si aucun pattern spécifié

    for I := 0 to Files.Count - 1 do
    begin
      // Rechercher dans le répertoire actuel
      if FindFirst(CleanPath + Files[I], faAnyFile, SearchRec) = 0 then
      try
        repeat
          // Arrêter immédiatement si le thread est terminé ou annulé
          if Terminated or FOwner.Aborted then Exit;

          // Ignorer les entrées spéciales '.' et '..'
          if (SearchRec.Name = '.') or (SearchRec.Name = '..') then Continue;

          FullPath := CleanPath + SearchRec.Name;

          // Si c'est un répertoire
          if (SearchRec.Attr and SysUtils.faDirectory) = SysUtils.faDirectory then
          begin
            // Vérifier si on doit explorer les sous-dossiers
            if FOwner.Criteria.Recursive then
              SearchIn(FullPath);
          end
          else
          begin
            // Vérifier les critères
            if MatchesMask(SearchRec.Name, Files[I]) and
               FOwner.Criteria.Attributes.Matches(FullPath, SearchRec) and
               FOwner.Criteria.Size.Matches(SearchRec.Size) then
            begin
              // Vérification différée du contenu (si activée)
              if FOwner.Criteria.Content.Phrase <> '' then
              begin
                if not MatchesContent(FullPath, FOwner.Criteria.Content.Phrase, FOwner.Criteria.Content.Options) then
                  Continue;
              end;

              // Remplir les détails de fichier
              FileDetails.Location := CleanPath;
              FileDetails.Name := SearchRec.Name;
              FileDetails.Size := SearchRec.Size;
              FileDetails.IsDirectory := False;

              // Notifier un fichier correspondant
              Synchronize(@FOwner.IncrementFileCount);
              Synchronize(@DoFileMatch);
            end;
          end;
        until FindNext(SearchRec) <> 0;
      finally
        FindClose(SearchRec);
      end;
    end;
  finally
    Files.Free;
  end;
end;
}


// Main execution logic for the search thread
procedure TFindFileThread.Execute;
begin
  // Notify that the search is starting
  Synchronize(@DoSearchBegin);

  // Perform the search starting in the specified folder
  SearchIn(FOwner.Criteria.Folder);

  // Calculate the total search time in seconds
  FOwner.FSearchTime := (GetTickCount64 - FOwner.FStartTime) / 1000;

  // Notify that the search has finished
  Synchronize(@DoSearchFinish);
end;

{ TFindFile }

// Constructor: Initializes a new instance of TFindFile
constructor TFindFile.Create;
begin
  FCriteria := TSearchCriteria.Create; // Create search criteria object
  FAborted := False;                   // Initialize aborted flag
  FBusy := False;                      // Initialize busy state
end;

// Destructor: Frees resources associated with TFindFile
destructor TFindFile.Destroy;
begin
  Abort; // Ensure any running search is aborted
  FCriteria.Free; // Free the search criteria object
  inherited Destroy;
end;

// Increments the counter for matched files
procedure TFindFile.IncrementFileCount;
begin
  Inc(FFileCount);
end;

// Increments the counter for searched folders
procedure TFindFile.IncrementFoldersSearchedCount;
begin
  Inc(FFoldersSearchedCount);
end;

// Notifies that a search has started
procedure TFindFile.DoSearchBegin;
begin
  if Assigned(FOnSearchBegin) then
    FOnSearchBegin(Self); // Trigger the OnSearchBegin event
end;

// Notifies that a search has finished
procedure TFindFile.DoSearchFinish;
begin
  if Assigned(FOnSearchFinish) then
  begin

    FOnSearchFinish(Self); // Trigger the OnSearchFinish event
  end;
end;

// Starts a new file search
procedure TFindFile.Execute(const StartPath: String; Recursive: Boolean);
begin
  if FBusy then Exit; // Exit if a search is already in progress

  FFileCount := 0;              // Reset file count
  FFoldersSearchedCount := 0;   // Reset folder count
  FStartTime := GetTickCount64; // Record the start time
  FBusy := True;                // Set the busy state
  FAborted := False;            // Reset aborted flag

  FCriteria.Folder := StartPath; // Set the start path
  FCriteria.Recursive := Recursive; // Set whether to search recursively

  // Create and start the search thread
  FThread := TFindFileThread.Create(Self);
  FThread.Start;
end;

// Aborts an ongoing search
procedure TFindFile.Abort;
begin
  FAborted:=True; // Set the aborted flag
  if Assigned(FThread) then
  begin
    FThread.Terminate;   // Terminate the search thread
    FThread.WaitFor;     // Wait for the thread to finish
    FreeAndNil(FThread); // Free the thread object
  end;
  FBusy := False;        // Clear the busy state
end;

end.

