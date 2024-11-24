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

unit UnitMain;

{$mode objfpc}{$H+}

interface

// Imports necessary libraries for functionality
uses

  Classes, SysUtils,              // Basic Pascal classes and utilities
  Forms, Controls, Graphics,      // GUI components and graphical utilities
  Dialogs, Menus, ComCtrls,       // Dialogs, menus, and common controls
  StdCtrls, ExtCtrls, Spin,       // Standard and extended controls, spin controls
  DateTimePicker, Process,        // Date-time controls and process handling
  FindFile,Math,         // File search, UNIX base utilities, and mathematical functions
  fileinfo,                       // File version information utilities
  winpeimagereader,               // For reading Windows Portable Executable files
  elfreader,                      // For reading ELF (Linux binary) files
  machoreader,                    // For reading Mach-O (macOS binary) files
  XMLRead, XMLWrite, DOM
  {$IFDEF WINDOWS}
  ,ShellAPI
  ,Windows
  {$ELSE}
  ,BaseUnix
  {$ENDIF}
  ;


// Constants defining URLs for the application
const
  WEBSITE_URL = 'https://SoftForges.com'; // Official website for the application
  GITHUB_URL = 'https://github.com/NDXDeveloper/FileFind'; // Source code repository on GitHub

type
  // Record for storing application metadata
  TAppInfo = record
    CompanyName: string;             // Name of the company or author
    FileDescription: string;         // Short description of the application
    FileVersion: string;             // Application version
    InternalName: string;            // Internal name of the executable
    LegalCopyright: string;          // Copyright information
    OriginalFilename: string;        // Original name of the executable
    ProductName: string;             // Product name
    ProductVersion: string;          // Version of the product
  end;

  // ** Enum for supported file managers **
  TFileManager = (
    fmUnknown,    // Unknown file manager
    fmNautilus,   // GNOME's Nautilus
    fmDolphin,    // KDE's Dolphin
    fmThunar,     // XFCE's Thunar
    fmCaja,       // MATE's Caja
    fmXdgOpen     // Generic xdg-open command
  );

  // ** Main form class definition **
  { TMainForm }
   TMainForm = class(TForm)
    // UI Components
    Button_SelectFolder: TButton;            // Button for selecting a folder
    CheckBoxExcludedDirectory: TCheckBox;    // Exclude directories from search
    CheckBoxExcludedHidden: TCheckBox;       // Exclude hidden files
    CheckBoxExcludedReadOnly: TCheckBox;     // Exclude read-only files
    CheckBoxExcludedSysFile: TCheckBox;      // Exclude system files
    CheckBoxRequiredReadOnly: TCheckBox;     // Include only read-only files
    CheckBoxRequiredHidden: TCheckBox;       // Include only hidden files
    CheckBoxRequiredSysFile: TCheckBox;      // Include only system files
    CheckBoxRequiredDirectory: TCheckBox;    // Include only directories
    RecursiveCheckBox: TCheckBox;         // Checkbox for recursive search
    CheckBoxNegate: TCheckBox;            // Checkbox for negating search results
    CheckBoxWholeWord: TCheckBox;         // Checkbox for whole word search
    CheckBoxCaseSensitive: TCheckBox;     // Checkbox for case-sensitive search
    FileMaskEdit: TEdit;                  // Input for file mask
    FolderEdit: TEdit;                    // Input for folder path
    ContentSearchEdit: TEdit;             // Input for content search
    ImageLogo: TImage;              // Application logo

    // Date and Time Filters
    CheckBox_C_AftDate: TCheckBox;   // Creation date after
    CheckBox_M_AftDate: TCheckBox;   // Modification date after
    CheckBox_C_AftTime: TCheckBox;   // Creation time after
    CheckBox_A_AftDate: TCheckBox;   // Access date after
    CheckBox_M_AftTime: TCheckBox;   // Modification time after
    CheckBox_C_BefDate: TCheckBox;   // Creation date before
    CheckBox_A_AftTime: TCheckBox;   // Access time after
    CheckBox_M_BefDate: TCheckBox;   // Modification date before
    CheckBox_C_BefTime: TCheckBox;   // Creation time before
    CheckBox_A_BefDate: TCheckBox;   // Access date before
    CheckBox_M_BefTime: TCheckBox;   // Modification time before
    CheckBox_A_BefTime: TCheckBox;   // Access time before

    // Size Filters
    ComboBox_AtLeast: TComboBox;     // Minimum size filter
    ComboBox_AtMost: TComboBox;      // Maximum size filter

    // Date-Time Pickers
    DateTimePicker_M_AftDate: TDateTimePicker;  // Picker for modification date (after)
    DateTimePicker_A_AftDate: TDateTimePicker;  // Picker for access date (after)
    DateTimePicker_M_AftTime: TDateTimePicker;  // Picker for modification time (after)
    DateTimePicker_C_BefDate: TDateTimePicker;  // Picker for creation date (before)
    DateTimePicker_A_AftTime: TDateTimePicker;  // Picker for access time (after)
    DateTimePicker_M_BefDate: TDateTimePicker;  // Picker for modification date (before)
    DateTimePicker_C_BefTime: TDateTimePicker;  // Picker for creation time (before)
    DateTimePicker_C_AftDate: TDateTimePicker;  // Picker for creation date (after)
    DateTimePicker_C_AftTime: TDateTimePicker;  // Picker for creation time (after)
    DateTimePicker_A_BefDate: TDateTimePicker;  // Picker for access date (before)
    DateTimePicker_M_BefTime: TDateTimePicker;  // Picker for modification time (before)
    DateTimePicker_A_BefTime: TDateTimePicker;  // Picker for access time (before)

    // Groups and Labels
    GroupBox_Size: TGroupBox;  // Size filters
    GroupBox_Att: TGroupBox;   // Attribute filters
    Label_filt_ent: TLabel;    // Label for filter entries
    Label_Filt_Desc: TLabel;   // Label for filter description
    LabelLicense1: TLabel;     // Label for license
    LabelVersion: TLabel;      // Label for version
    LabelDeveloper: TLabel;    // Label for developer
    LabelLicense: TLabel;      // Label for license details
    LabelVisitWebsite: TLabel; // Label for visiting the website
    LabelGitHub: TLabel;       // Label for visiting the GitHub repository
    Label_SizeAtLeast: TLabel; // Label fot Min Size
    Label_SizeAtMost: TLabel;  // Label for Max Size
    Label_RequiredAttributes: TLabel; // Label for Required Attributes
    Label_ExcludedAttributes: TLabel; // Label for Excluded Attributes
    LabelAboutTitle: TLabel; // Label for About Title
    LabelAppName: TLabel; // Label for App Name
    Label_Filename: TLabel; // Label for filename
    Label_Location: TLabel; // Label for folder location
    Label_Content: TLabel; // Label for content

    // File Filters
    Filters: TMemo;            // Text area for defining custom filters

    // Context Menu
    PopupMenu1: TPopupMenu;    // Context menu for file operations
    MenuItemOpenFile: TMenuItem;     // Menu option to open a file
    MenuItemOpenLocation: TMenuItem; // Menu option to open the location of a file

    // Progress Display
    ProgressImages: TImageList;         // List of images for progress animation
    ProgressImage: TImage;              // Current progress image
    ProgressImagePanel: TPanel;         // Panel for progress animation

    // Input Fields
    SpinEdit_AtMost: TSpinEdit;         // Spin editor for maximum size
    SpinEdit_AtLeast: TSpinEdit;        // Spin editor for minimum size

    // Tabs
    PageControl1: TPageControl;         // Tab control for main search sections
    PageControl2: TPageControl;         // Additional tab control for filter options
    TabSheet_About: TTabSheet;          // Tab for "About" section
    TabSheet_NameLocation: TTabSheet;   // Tab for name and location filters
    TabSheet_DateTime: TTabSheet;       // Tab for date and time filters
    TabSheet_SizeAttributes: TTabSheet; // Tab for size and attribute filters
    TabSheet_Filters: TTabSheet;        // Tab for custom filters
    TabSheetCrea: TTabSheet;            // Tab for creation date filters
    TabSheetMod: TTabSheet;             // Tab for modification date filters
    TabSheetAcc: TTabSheet;             // Tab for access date filters

    // Status and Buttons
    StatusBar1: TStatusBar;             // Status bar to display application messages
    StopButton: TButton;                // Button to stop the search
    StartButton: TButton;               // Button to start the search

    // Timer
    ProgressImageTimer: TTimer;         // Timer for progress animation

    // Results
    ListView1: TListView;               // Display for search results

    // Event Handlers
    procedure Button_SelectFolderClick(Sender: TObject);     // Handles folder selection dialog.
    procedure ContentSearchEditChange(Sender: TObject);      // Reacts to changes in content search input.
    procedure FormDestroy(Sender: TObject);
    procedure HandleAttributeCheckBoxClick(Sender: TObject); // Handles clicks on attribute filters.
    procedure CheckBoxDateClick(Sender: TObject);            // Enables/Disables date/time filters.
    procedure FormCreate(Sender: TObject);                   // Initializes the form and components.
    procedure LabelGitHubClick(Sender: TObject);             // Opens the GitHub URL.
    procedure LabelVisitWebsiteClick(Sender: TObject);       // Opens the website URL.
    procedure ListView1ColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListView1Compare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ListView1DblClick(Sender: TObject);            // Handles double-clicks on the results list.
    procedure ListView1MouseDown(Sender: TObject; Button: TMouseButton; // Handles mouse clicks on the results list.
      Shift: TShiftState; X, Y: Integer);
    procedure MenuItemOpenFileClick(Sender: TObject);        // Handles the "Open File" menu option.
    procedure MenuItemOpenLocationClick(Sender: TObject);    // Handles the "Open Location" menu option.

    procedure ProgressImageTimerTimer(Sender: TObject);      // Updates the progress animation.
    procedure StartButtonClick(Sender: TObject);             // Starts the file search.
    procedure StopButtonClick(Sender: TObject);              // Stops the file search.
    procedure OpenFile(FilePath: String);                    // Opens a file or directory.
    procedure TabSheet_NameLocationResize(Sender: TObject);

  private
    FSearch: TFindFile; // Core object handling the file search.
    AppInfo: TAppInfo; // Holds application metadata.

    SortedColumn: Integer;
    Descending: Boolean;

    procedure OnFileMatch(Sender: TObject; const FileInfo: TFileDetails); // Handles file matches during
    procedure OnSearchBegin(Sender: TObject); // Handles the beginning of a search operation.
    procedure OnSearchFinish(Sender: TObject); // Handles the end of a search operation.
    procedure ShowStatus(Value: String); // Updates the status bar text.
    procedure OnFolderChange(Sender: TObject; const Folder: String; var IgnoreFolder: TFolderIgnore); // Handles folder changes during the search.
    procedure OpenURL(const URL: string); // Opens a URL in the default browser.
    function GetAppInfo: TAppInfo; // Retrieves application metadata.
  public
    // Public interface of the form
  end;


var
  MainForm: TMainForm;

implementation

{$R *.lfm}

function GetUserHomeDir: String;
begin
  {$IFDEF DARWIN}
  Result := GetEnvironmentVariable('HOME'); // For macOS
  {$ELSE}
    {$IFDEF UNIX}
    Result := GetEnvironmentVariable('HOME'); // For Linux
    {$ELSE}
      {$IFDEF WINDOWS}
      Result := SysUtils.GetEnvironmentVariable('USERPROFILE'); // For Windows
      {$ELSE}
      Result := ''; // Unsupported platform
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;


// Function to determine if a given file is executable
function FileIsExecutable(const FilePath: string): Boolean;
var
  {$IFDEF UNIX}
  StatBuf: BaseUnix.Stat; // Structure to hold file status information
  {$ENDIF}
  FileStream: TFileStream;    // Stream to read the file content
  Magic: array[0..3] of Byte; // Array to store the first few bytes of the file
  FirstLine: string; // String to hold the first line of the file
  Buffer: array[0..255] of Char; // Buffer to read file content
  BytesRead: LongInt; // Number of bytes read from the file
begin
  Result := False; // Initialize the result as false
  {$IFDEF UNIX}
  // Check if the file exists and get its metadata
  if FpStat(FilePath, StatBuf) <> 0 then
    Exit;

  // Check if the file is a regular file
  if not FPS_ISREG(StatBuf.st_mode) then
    Exit;

  // Check if the file has executable permissions for the current user
  if FpAccess(FilePath, X_OK) <> 0 then
    Exit;

  // Open the file to read its contents
  try
    FileStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyNone);
    try
      // Check if the file is empty
      if FileStream.Size = 0 then
        Exit; // Empty files are not executable

      // Read the first few bytes of the file
      FillChar(Magic, SizeOf(Magic), 0);
      FileStream.ReadBuffer(Magic, Min(4, FileStream.Size));

      // Check if the file is an ELF executable
      if (Magic[0] = $7F) and (Magic[1] = Ord('E')) and (Magic[2] = Ord('L')) and (Magic[3] = Ord('F')) then
        Exit(True);

      // Check if the file is a Mach-O executable (used on macOS)
      if (Magic[0] = $CF) and (Magic[1] = $FA) and (Magic[2] = $ED) and (Magic[3] = $FE) then
        Exit(True); // Mach-O file in big-endian
      if (Magic[0] = $FE) and (Magic[1] = $ED) and (Magic[2] = $FA) and (Magic[3] = $CF) then
        Exit(True); // Mach-O file in little-endian

      // If not an ELF file, check if it is a script with a shebang (#!)
      FileStream.Position := 0; // Reset file stream position to the beginning
      FillChar(Buffer, SizeOf(Buffer), 0);
      BytesRead := FileStream.Read(Buffer, Min(SizeOf(Buffer) - 1, FileStream.Size));
      SetString(FirstLine, Buffer, BytesRead);

      // Check if the first line starts with `#!`, indicating a script
      if FirstLine.StartsWith('#!') then
        Exit(True);

      // Note: Additional executable formats can be checked here if needed
    finally
      FileStream.Free; // Free the file stream
    end;
  except
    // If an error occurs during file reading, consider the file as non-executable
    Exit(False);
  end;

  // If none of the conditions are met, the file is not executable
  Result := False;
  {$ENDIF}
end;


{ TMainForm }

// Method to retrieve application metadata such as version, company, and product details
function TMainForm.GetAppInfo: TAppInfo;
var
  FileVerInfo: TFileVersionInfo; // Object to extract file version information
begin
  // Create an instance of TFileVersionInfo to read application file details
  FileVerInfo := TFileVersionInfo.Create(nil);
  try
    // Set the target file to the currently running application
    FileVerInfo.FileName := ParamStr(0);

    // Read file version information into the object
    FileVerInfo.ReadFileInfo;

    // Populate the result record with metadata values from the file
    Result.CompanyName := FileVerInfo.VersionStrings.Values['CompanyName'];
    Result.FileDescription := FileVerInfo.VersionStrings.Values['FileDescription'];
    Result.FileVersion := FileVerInfo.VersionStrings.Values['FileVersion'];
    Result.InternalName := FileVerInfo.VersionStrings.Values['InternalName'];
    Result.LegalCopyright := FileVerInfo.VersionStrings.Values['LegalCopyright'];
    Result.OriginalFilename := FileVerInfo.VersionStrings.Values['OriginalFilename'];
    Result.ProductName := FileVerInfo.VersionStrings.Values['ProductName'];
    Result.ProductVersion := FileVerInfo.VersionStrings.Values['ProductVersion'];
  finally
    // Free the TFileVersionInfo instance to release resources
    FileVerInfo.Free;
  end;
end;

// Opens a URL in the default web browser
procedure TMainForm.OpenURL(const URL: string);
var
  AProcess: TProcess; // Represents a system process
begin
  {$IFDEF WINDOWS}
    ShellExecute(Handle, 'open', PChar(URL), nil, nil, SW_NORMAL);
  {$ELSE}
  // Create a new process instance
  AProcess := TProcess.Create(nil);
  try
    {$IFDEF DARWIN}
    AProcess.Executable := 'open';
    AProcess.Parameters.Add(URL);
    {$ELSE}
    // Set the executable to 'xdg-open', a standard tool to open URLs in Linux
    AProcess.Executable := 'xdg-open';

    // Add the URL as a parameter to be opened
    AProcess.Parameters.Add(URL);

    // Execute the process to open the URL
    AProcess.Execute;
    {$ENDIF}
  finally
    // Free the process instance to release resources
    AProcess.Free;
  end;
  {$ENDIF}
end;

// Triggered when the folder being searched changes during the file search operation
procedure TMainForm.OnFolderChange(Sender: TObject; const Folder: String;
  var IgnoreFolder: TFolderIgnore);
begin
  // Updates the status bar with the current folder being processed
  ShowStatus(Folder);
end;

// Initializes the main form when it is created
procedure TMainForm.FormCreate(Sender: TObject);
begin
    inherited; // Call the inherited FormCreate method, if applicable

    SortedColumn:=-1;
    Descending:=False;

    // Optimize graphics rendering by enabling double buffering
    ProgressImagePanel.DoubleBuffered := True;

    Listview1.DoubleBuffered:=True;

    // Set the initial image for the progress indicator
    ProgressImages.GetBitmap(0, ProgressImage.Picture.Bitmap);

    // Initialize date-time pickers with default values
    DateTimePicker_C_BefDate.Date := Date;
    DateTimePicker_C_BefDate.Time := 0;
    DateTimePicker_C_AftDate.Date := Date;
    DateTimePicker_C_AftDate.Time := 0;
    DateTimePicker_C_BefTime.Time := Time;
    DateTimePicker_C_BefTime.Date := 0;
    DateTimePicker_C_AftTime.Time := Time;
    DateTimePicker_C_AftTime.Date := 0;

    // Set the initial tabs for PageControls
    PageControl2.TabIndex:=0; // Default to the first tab
    PageControl1.TabIndex:=0; // Default to the first tab

    // Retrieve application metadata and display it
    AppInfo := GetAppInfo; // Fetch app details

    // Update UI labels with metadata
    LabelVersion.Caption := 'Version : ' + AppInfo.FileVersion;
    LabelGitHub.Caption := 'Source code: GitHub';
    LabelGitHub.Font.Color := clBlue; // Make the GitHub label blue
    LabelGitHub.Cursor := crHandPoint; // Change cursor to hand for interactivity

    LabelVisitWebsite.Caption := 'SoftForges Website';
    LabelVisitWebsite.Font.Color := clNavy; // Make the website label navy
    LabelVisitWebsite.Cursor := crHandPoint; // Change cursor to hand for interactivity

    self.FolderEdit.Text:=GetUserHomeDir;
end;

// Handles the click event for the GitHub label
procedure TMainForm.LabelGitHubClick(Sender: TObject);
begin
  // Opens the GitHub URL in the default browser
  OpenURL(GITHUB_URL);
end;

// Handles the click event for the website label
procedure TMainForm.LabelVisitWebsiteClick(Sender: TObject);
begin
  // Opens the website URL in the default browser
  OpenURL(WEBSITE_URL);
end;

// Handles the click event on a column header in the ListView.
// Updates sorting preferences and triggers sorting of the ListView items based on the clicked column.
procedure TMainForm.ListView1ColumnClick(Sender: TObject; Column: TListColumn);
begin
  // Check if a search is currently in progress
  if not FSearch.Busy then
  begin
    // Temporarily disable sorting while updating sorting preferences
    TListView(Sender).SortType := stNone;
    // Update sorting preferences based on the clicked column
    if Column.Index <> SortedColumn then
    begin
      // Change the sorted column and set ascending order as default
      SortedColumn := Column.Index;
      Descending := False;
    end
    else
      // Toggle the sort direction if the same column is clicked again
      Descending := not Descending;
    // Re-enable sorting after updating preferences
    TListView(Sender).SortType := stText;
  end
  else
    // Show a warning if sorting is attempted during an ongoing search
    MessageDlg('Cannot sort the list while a search is in progress.', mtWarning, [mbOK], 0);
end;

// Compares two items in the ListView during sorting.
// Determines the order of two items based on the selected column and sort direction.
procedure TMainForm.ListView1Compare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
begin
  // Compare items based on the column index (SortedColumn)
  if SortedColumn = 0 then
    // Compare the main caption of the items if sorting by the first column
    Compare := CompareText(Item1.Caption, Item2.Caption)
  else if SortedColumn > 0 then
    // Compare sub-items if sorting by any other column
    Compare := CompareText(Item1.SubItems[SortedColumn-1],
                           Item2.SubItems[SortedColumn-1]);
  // Reverse the comparison result if sorting in descending order
  if Descending then Compare := -Compare;
end;

// Handles the double-click event on the ListView1 item
procedure TMainForm.ListView1DblClick(Sender: TObject);
var
  FilePath: string; // Full path of the selected file or folder
begin
  // Check if an item is selected in the ListView
  if ListView1.Selected <> nil then
  begin
    // Construct the full file path from the selected item's data
    FilePath := IncludeTrailingPathDelimiter(Listview1.Selected.SubItems[0]) + Listview1.Selected.Caption;

    // Open the selected file or folder
    OpenFile(FilePath);
  end;
end;

// Handles the mouse down event on ListView1
procedure TMainForm.ListView1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Item: TListItem; // Represents the item under the cursor
begin
  // Check if the right mouse button was clicked
  if Button = mbRight then
  begin
    // Get the item under the mouse cursor
    Item := ListView1.GetItemAt(X, Y);

    // If an item is under the cursor, select it
    if Item <> nil then
    begin
      ListView1.Selected := Item;

      // Show the popup menu at the current cursor position
      PopupMenu1.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    end;
  end;
end;

// Handles the click event for the "Open File" menu item
procedure TMainForm.MenuItemOpenFileClick(Sender: TObject);
var
  FilePath: string; // Full path of the selected file or folder
begin
  // Check if an item is selected in the ListView
  if Listview1.Selected <> nil then
  begin
    // Construct the full file path from the selected item's data
    FilePath := IncludeTrailingPathDelimiter(Listview1.Selected.SubItems[0]) + Listview1.Selected.Caption;

    // Open the selected file or folder
    OpenFile(FilePath);
  end;
end;


// Opens the specified file or folder using the default application
procedure TMainForm.OpenFile(FilePath: String);
var
   AProcess: TProcess; // Represents a system process
begin
  // Ensure a valid item is selected
  if Listview1.Selected <> nil then
  begin
    // Check if the file or folder exists
    if FileExists(FilePath) or DirectoryExists(FilePath) then
    begin
      {$IFDEF WINDOWS}
      if Listview1.Selected <> nil then
         with Listview1.Selected do
           ShellExecute(Handle, 'Open', PChar(Caption), nil, PChar(SubItems[0]), SW_NORMAL);
      {$ELSE}
            // Create a process to open the file or folder
      AProcess := TProcess.Create(nil);
      try
        //
         {$IFDEF DARWIN}
         // If the file is not executable, open it with the default application
         if not FileIsExecutable(FilePath) then
         begin
           AProcess.Executable := 'open';
           AProcess.Parameters.Add(FilePath);
         end
         else
         begin
           // Directly execute the file if it's executable
           AProcess.Executable := FilePath;
         end;
         {$ELSE}
         // If the file is not executable, open it with the default application
         if not FileIsExecutable(FilePath) then
         begin
           AProcess.Executable := 'xdg-open';
           AProcess.Parameters.Add(FilePath);
         end
         else
         begin
           // Directly execute the file if it's executable
           AProcess.Executable := FilePath;
         end;
         {$ENDIF}


        // Execute the process
        AProcess.Execute;

     finally
       // Free the process instance to release resources
       AProcess.Free;
     end;
      {$ENDIF}
    end
    else
    begin
      // Show a message if the file or folder does not exist
      ShowMessage('The selected file or folder does not exist.');
    end;
  end;
end;

// Handles the resizing of the "Name and Location" tab.
// Dynamically adjusts the width of the second column (index 1) of the ListView
// to occupy the remaining available space after considering other columns.
procedure TMainForm.TabSheet_NameLocationResize(Sender: TObject);
var
  TotalWidth, OtherColumnsWidth, AvailableWidth: Integer;
  i: Integer;
begin
  // Calculate the total available width for the ListView.
  TotalWidth := ListView1.ClientWidth;

  // Calculate the total width occupied by all columns except the second column (index 1).
  OtherColumnsWidth := 0;
  for i := 0 to ListView1.Columns.Count - 1 do
  begin
    if i <> 1 then
      Inc(OtherColumnsWidth, ListView1.Columns[i].Width);
  end;

  // Calculate the width available for the second column.
  AvailableWidth := TotalWidth - OtherColumnsWidth;

  // Ensure the calculated width is positive.
  if AvailableWidth > 0 then
    ListView1.Columns[1].Width := AvailableWidth // Set the second column's width to the available space.
  else
    ListView1.Columns[1].Width := 50; // Fallback: Assign a minimum width if the calculated width is invalid.
end;

// Determines the type of file manager based on its name
function GetFileManagerType(const FileManager: string): TFileManager;
begin
  // Check the file manager's name against known file managers
  if Pos('nautilus', LowerCase(FileManager)) > 0 then
    Result := fmNautilus // GNOME file manager
  else if Pos('dolphin', LowerCase(FileManager)) > 0 then
    Result := fmDolphin // KDE file manager
  else if Pos('thunar', LowerCase(FileManager)) > 0 then
    Result := fmThunar // XFCE file manager
  else if Pos('caja', LowerCase(FileManager)) > 0 then
    Result := fmCaja // MATE file manager
  else if Pos('xdg-open', LowerCase(FileManager)) > 0 then
    Result := fmXdgOpen // Generic file opener
  else
    Result := fmUnknown; // Unrecognized file manager
end;

// Retrieves the default file manager for the system
function GetDefaultFileManager: string;
var
  Output: string;
begin
  // Run `xdg-mime` to query the default handler for directories
  if RunCommand('xdg-mime', ['query', 'default', 'inode/directory'], Output) then
    Result := Trim(Output) // Trim and return the output
  else
    Result := 'xdg-open'; // Fallback to xdg-open if no default is found
end;

// Searches for the full path of an executable in the system PATH
function FindExecutableInPath(const Executable: string): string;
var
  PathEnv, SearchPath, FullPath: string;
  PathList: TStringList; // List of directories in the PATH
  I: Integer;
begin
  PathEnv := SysUtils.GetEnvironmentVariable('PATH'); // Get the system PATH environment variable
  PathList := TStringList.Create;
  try
    // Split the PATH variable into individual directories
    PathList.Text := StringReplace(PathEnv, ':', #10, [rfReplaceAll]);
    for I := 0 to PathList.Count - 1 do
    begin
      SearchPath := IncludeTrailingPathDelimiter(PathList[I]);
      FullPath := SearchPath + Executable;
      // Check if the executable exists in this directory
      if FileExists(FullPath) then
        Exit(FullPath); // Return the full path if found
    end;
    Result := ''; // Return an empty string if not found
  finally
    PathList.Free; // Free the TStringList instance
  end;
end;

// Opens the location of a selected file in the file manager
procedure TMainForm.MenuItemOpenLocationClick(Sender: TObject);
var
  FilePath, FileManager, Param: String;
  FileManagerType: TFileManager;
  AProcess: TProcess; // Process to handle file manager invocation
begin
  // Ensure an item is selected in the ListView
  if Listview1.Selected <> nil then
  begin
    // Get the full path of the selected file
    FilePath := IncludeTrailingPathDelimiter(Listview1.Selected.SubItems[0]) + Listview1.Selected.Caption;

    // Check if the file exists
    if FileExists(FilePath) or DirectoryExists(FilePath) then
    begin

      {$IFDEF WINDOWS}
      // Utiliser ShellExecute sur Windows pour ouvrir le fichier dans l'explorateur
      Param := Format('/n,/select,"%s"', [FilePath]);
      ShellExecute(Handle, 'open', 'explorer.exe', PChar(Param), nil, SW_NORMAL);
      {$ELSE}
      AProcess := TProcess.Create(nil); // Create a process instance
      try
          // Configure the process based on the file manager type
          FileManager := GetDefaultFileManager; // Retrieve the default file manager
          FileManagerType := GetFileManagerType(FileManager); // Determine its type
          {$IFDEF DARWIN}
          // Use open on macOS
          AProcess.Executable := 'open';
          AProcess.Parameters.Add('-R'); // Reveal the file
          AProcess.Parameters.Add(FilePath);
          {$ELSE}
          // Configure Linux-specific file managers
          case FileManagerType of
            fmNautilus: // GNOME's Nautilus
              begin
                AProcess.Executable := 'nautilus';
                AProcess.Parameters.Add('--select');
                AProcess.Parameters.Add(FilePath);
              end;
            fmDolphin: // KDE's Dolphin
              begin
                AProcess.Executable := 'dolphin';
                AProcess.Parameters.Add('--select');
                AProcess.Parameters.Add(FilePath);
              end;
            fmThunar: // XFCE's Thunar
              begin
                AProcess.Executable := 'thunar';
                AProcess.Parameters.Add('--select');
                AProcess.Parameters.Add(FilePath);
              end;
            fmCaja: // MATE's Caja
              begin
                AProcess.Executable := 'caja';
                AProcess.Parameters.Add('--no-desktop');
                AProcess.Parameters.Add('--browser');
                AProcess.Parameters.Add('--select=' + FilePath);
              end;
            fmXdgOpen: // Generic handler
              begin
                AProcess.Executable := 'xdg-open';
                AProcess.Parameters.Add(ExtractFileDir(FilePath));
              end;
          else
            ShowMessage('Unsupported file manager.'); // Handle unknown file managers
            Exit;
          end;

          // Ensure the executable exists, fallback to xdg-open if necessary
          if FindExecutableInPath(AProcess.Executable) = '' then
          begin
            AProcess.Executable := 'xdg-open';
            AProcess.Parameters.Clear; // Clear parameters
            AProcess.Parameters.Add(ExtractFileDir(FilePath));
          end;
          AProcess.Execute; // Run the process
        {$ENDIF}


      finally
        AProcess.Free; // Free the process instance
      end;
      {$ENDIF}

    end
    else
    begin
      // Show a message if the file does not exist
      ShowMessage('The selected file does not exist.');
    end;
  end
  else
  begin
    // Show a message if no file is selected
    ShowMessage('No file selected.');
  end;
end;



// Updates the progress animation in the UI periodically
procedure TMainForm.ProgressImageTimerTimer(Sender: TObject);
begin
  // Increment the current image index (looping back to 0 if it exceeds the count)
  ProgressImages.Tag := (ProgressImages.Tag + 1) mod ProgressImages.Count;
  // Update the image displayed to the next in the sequence
  ProgressImages.GetBitmap(ProgressImages.Tag, ProgressImage.Picture.Bitmap);
  // Refresh the image control to reflect the changes
  ProgressImage.Refresh;
end;

// Opens a folder selection dialog and updates the folder input field
procedure TMainForm.Button_SelectFolderClick(Sender: TObject);
var
  SelectDirectoryDialog: TSelectDirectoryDialog;
begin
  // Create a folder selection dialog instance
  SelectDirectoryDialog := TSelectDirectoryDialog.Create(nil);
  try
    // Set dialog title and default folder
    SelectDirectoryDialog.Title := 'Sélectionnez un dossier';
    SelectDirectoryDialog.InitialDir := GetUserDir; //'/home'; // Optionnel : dossier par défaut

    // Display the dialog and handle the selected folder
    if SelectDirectoryDialog.Execute then
    begin
      // Update the folder input field with the selected folder
      FolderEdit.Text := SelectDirectoryDialog.FileName;
    end;
  finally
    // Free the dialog instance to release resources
    SelectDirectoryDialog.Free;
  end;
end;

// Enables or disables content search options based on user input
procedure TMainForm.ContentSearchEditChange(Sender: TObject);
begin
  if Length(ContentSearchEdit.Text)>0 then
  begin
    // Enable additional search options when the search text is not empty
    if not CheckBoxNegate.Enabled then CheckBoxNegate.Enabled:= True;
    if not CheckBoxWholeWord.Enabled then CheckBoxWholeWord.Enabled:= True;
    if not CheckBoxCaseSensitive.Enabled then CheckBoxCaseSensitive.Enabled:= True;
  end
  else
  begin
    // Disable options when the search text is empty
    if CheckBoxNegate.Enabled then
    begin
      CheckBoxNegate.Enabled:= False;
      CheckBoxNegate.Checked:= False;
    end;

    if CheckBoxWholeWord.Enabled then
    begin
      CheckBoxWholeWord.Enabled:= False;
      CheckBoxWholeWord.Checked:= False;
    end;
    if CheckBoxCaseSensitive.Enabled then
    begin
      CheckBoxCaseSensitive.Enabled:= False;
      CheckBoxCaseSensitive.Checked:= False;
    end;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  //
end;

// Ensures mutual exclusivity between included and excluded attribute options
procedure TMainForm.HandleAttributeCheckBoxClick(Sender: TObject);
begin
  // Handle changes in excluded directory checkbox
  if Sender = CheckBoxExcludedDirectory then
  begin
    if CheckBoxExcludedDirectory.Checked then
      CheckBoxRequiredDirectory.Checked := False; // Uncheck conflicting option
  end
  // Handle changes in excluded hidden files checkbox
  else if Sender = CheckBoxExcludedHidden then
  begin
    if CheckBoxExcludedHidden.Checked then
      CheckBoxRequiredHidden.Checked := False;
  end
  // Handle changes in excluded read-only files checkbox
  else if Sender = CheckBoxExcludedReadOnly then
  begin
    if CheckBoxExcludedReadOnly.Checked then
      CheckBoxRequiredReadOnly.Checked := False;
  end
  // Handle changes in excluded system files checkbox
  else if Sender = CheckBoxExcludedSysFile then
  begin
    if CheckBoxExcludedSysFile.Checked then
      CheckBoxRequiredSysFile.Checked := False;
  end
  // Handle changes in required directory checkbox
  else if Sender = CheckBoxRequiredDirectory then
  begin
    if CheckBoxRequiredDirectory.Checked then
      CheckBoxExcludedDirectory.Checked := False;
  end
  // Handle changes in required hidden files checkbox
  else if Sender = CheckBoxRequiredHidden then
  begin
    if CheckBoxRequiredHidden.Checked then
      CheckBoxExcludedHidden.Checked := False;
  end
  // Handle changes in required read-only files checkbox
  else if Sender = CheckBoxRequiredReadOnly then
  begin
    if CheckBoxRequiredReadOnly.Checked then
      CheckBoxExcludedReadOnly.Checked := False;
  end
  // Handle changes in required system files checkbox
  else if Sender = CheckBoxRequiredSysFile then
  begin
    if CheckBoxRequiredSysFile.Checked then
      CheckBoxExcludedSysFile.Checked := False;
  end;
end;

// Enables/Disables date/time filters.
procedure TMainForm.CheckBoxDateClick(Sender: TObject);
begin
  // Check if the sender (the control that triggered the event) is a specific TCheckBox
  // and enable or disable the corresponding TDateTimePicker based on the checkbox's state.
  if Sender = CheckBox_A_AftDate then
    DateTimePicker_A_AftDate.Enabled := CheckBox_A_AftDate.Checked
  else if Sender = CheckBox_A_AftTime then
    DateTimePicker_A_AftTime.Enabled := CheckBox_A_AftTime.Checked
  else if Sender = CheckBox_A_BefDate then
    DateTimePicker_A_BefDate.Enabled := CheckBox_A_BefDate.Checked
  else if Sender = CheckBox_A_BefTime then
    DateTimePicker_A_BefTime.Enabled := CheckBox_A_BefTime.Checked
  else if Sender = CheckBox_C_AftDate then
    DateTimePicker_C_AftDate.Enabled := CheckBox_C_AftDate.Checked
  else if Sender = CheckBox_C_AftTime then
    DateTimePicker_C_AftTime.Enabled := CheckBox_C_AftTime.Checked
  else if Sender = CheckBox_C_BefDate then
    DateTimePicker_C_BefDate.Enabled := CheckBox_C_BefDate.Checked
  else if Sender = CheckBox_C_BefTime then
    DateTimePicker_C_BefTime.Enabled := CheckBox_C_BefTime.Checked
  else if Sender = CheckBox_M_AftDate then
    DateTimePicker_M_AftDate.Enabled := CheckBox_M_AftDate.Checked
  else if Sender = CheckBox_M_AftTime then
    DateTimePicker_M_AftTime.Enabled := CheckBox_M_AftTime.Checked
  else if Sender = CheckBox_M_BefDate then
    DateTimePicker_M_BefDate.Enabled := CheckBox_M_BefDate.Checked
  else if Sender = CheckBox_M_BefTime then
    DateTimePicker_M_BefTime.Enabled := CheckBox_M_BefTime.Checked;
end;


// Initiates the file search process based on user-defined criteria
procedure TMainForm.StartButtonClick(Sender: TObject);
var
  Options: TContentSearchOptions;
begin

  // Clear any existing search object and create a new one
  if Assigned(FSearch) Then FreeAndNil(Fsearch);
      FSearch := TFindFile.Create();

      // Clear all previously set search criteria
      FSearch.Criteria.Clear;


      FSearch.Criteria.Content.Clear;
      FSearch.Criteria.Attributes.Clear;
      FSearch.Criteria.Size.Clear;
      FSearch.Criteria.DateTime.Clear;
      FSearch.Criteria.Content.Phrase := ContentSearchEdit.Text;

      // Configure file size range criteria
      with FSearch.Criteria.Size do
      begin
        MinSize := Self.SpinEdit_AtLeast.Value;
        case Self.ComboBox_AtLeast.ItemIndex of
          1: MinSize := MinSize * 1024; // Convert to kilobytes
          2: MinSize := MinSize * 1024 * 1024; // Convert to megabytes
          3: MinSize := MinSize * 1024 * 1024 * 1024; // Convert to gigabytes
        end;
        MaxSize := Self.SpinEdit_AtMost.Value;
        case Self.ComboBox_AtMost.ItemIndex of
          1: MaxSize := MaxSize * 1024;
          2: MaxSize := MaxSize * 1024 * 1024;
          3: MaxSize := MaxSize * 1024 * 1024 * 1024;
        end;
      end;

      // Configure content search options
      FSearch.Criteria.Content.Options := [];
      Options := FSearch.Criteria.Content.Options;

      // Handle content case-sensitive search
      if CheckBoxCaseSensitive.Checked then
        Include(Options, csoCaseSensitive)
      else
        Exclude(Options, csoCaseSensitive);

      // Handle content whole word matching
      if CheckBoxWholeWord.Checked then
        Include(Options, csoWholeWord)
      else
        Exclude(Options, csoWholeWord);

      // Handle negation of the content phrase
      if CheckBoxNegate.Checked then
        Include(Options, csoNegate)
      else
        Exclude(Options, csoNegate);

      FSearch.Criteria.Content.Options := Options;

      // Set the file name mask
      FSearch.Criteria.FileName:=FileMaskEdit.Text;

      // Apply additional filters provided by the user
      FSearch.Criteria.Filters.Assign(Self.Filters.Lines);

      // Configure timestamp criteria (Created, Modified, Accessed)
      with FSearch.Criteria.DateTime do
      begin
        Clear;
        // Handle created timestamps
        if (Self.CheckBox_C_BefDate.Checked) then
          CreatedBefore := Self.DateTimePicker_C_BefDate.Date;
        if (Self.CheckBox_C_BefTime.Checked) then
          CreatedBefore := CreatedBefore + Self.DateTimePicker_C_BefTime.Time;
        if (Self.CheckBox_C_AftDate.Checked) then
          CreatedAfter := Self.DateTimePicker_C_AftDate.Date;
        if (Self.CheckBox_C_AftTime.Checked) then
          CreatedAfter := CreatedAfter + Self.DateTimePicker_C_AftTime.Time;
        // Handle modified timestamps
        if (Self.CheckBox_M_BefDate .Checked) then
          ModifiedBefore := Self.DateTimePicker_M_BefDate.Date;
        if (Self.CheckBox_M_BefTime.Checked) then
          ModifiedBefore := ModifiedBefore + Self.DateTimePicker_M_BefTime.Time;
        if (CheckBox_M_AftDate.Checked) then
          ModifiedAfter := Self.DateTimePicker_M_AftDate.Date;
        if (Self.CheckBox_M_AftTime.Checked) then
          ModifiedAfter := ModifiedAfter + Self.DateTimePicker_M_AftTime.Time;
        // Handle accessed timestamps
        if (Self.CheckBox_A_BefDate.Checked) then
          AccessedBefore := Self.DateTimePicker_A_BefDate.Date;
        if (Self.CheckBox_A_BefTime.Checked) then
          AccessedBefore := AccessedBefore + Self.DateTimePicker_A_BefTime.Time;
        if (Self.CheckBox_A_AftDate.Checked) then
          AccessedAfter := Self.DateTimePicker_A_AftDate.Date;
        if (Self.CheckBox_A_AftTime.Checked) then
          AccessedAfter := AccessedAfter + Self.DateTimePicker_A_AftTime.Time;
      end;

      // Configure file attributes criteria
      FSearch.Criteria.Attributes.Clear;
      with FSearch.Criteria.Attributes
      do begin
        // Handle required attributes
        if CheckBoxRequiredReadOnly.Checked then
         Required := Required + [FindFile.faReadOnly];
        if CheckBoxRequiredHidden.Checked then
           Required := Required + [FindFile.faHidden];
        if CheckBoxRequiredSysFile.Checked then
          Required := Required + [FindFile.faSysFile];
        if CheckBoxRequiredDirectory.Checked then
          Required := Required + [FindFile.faDirectory];

        // Handle excluded attributes
        if CheckBoxExcludedReadOnly.Checked then
          Excluded := Excluded + [FindFile.faReadOnly];
        if CheckBoxExcludedHidden.Checked then
          Excluded := Excluded + [FindFile.faHidden];
        if CheckBoxExcludedSysFile.Checked then
          Excluded := Excluded + [FindFile.faSysFile];
        if CheckBoxExcludedDirectory.Checked then
          Excluded := Excluded + [FindFile.faDirectory];
      end;

      // Attach event handlers for search lifecycle
      FSearch.OnFileMatch := @OnFileMatch;
      FSearch.OnSearchBegin := @OnSearchBegin;
      FSearch.OnSearchFinish := @OnSearchFinish;
      FSearch.OnFolderChange := @OnFolderChange;

      // Execute the search process
      FSearch.Execute(FolderEdit.Text, RecursiveCheckBox.Checked);

end;

// Cancels the ongoing file search process
procedure TMainForm.StopButtonClick(Sender: TObject);
begin
  ShowStatus('Cancelling search, please wait...');
  FSearch.Abort; // Terminates the search thread safely
end;

// Handles each matching file found during the search
procedure TMainForm.OnFileMatch(Sender: TObject; const FileInfo: TFileDetails);
begin

  // Add file details to the list view
  with ListView1.Items.Add do
  begin
    Caption := FileInfo.Name; // File name
    SubItems.Add(FileInfo.Location); // File path
    if not FileInfo.IsDirectory then begin
       SubItems.Add(FormatFileSize(FileInfo.Size)); // File size
       SubItems.Add(DateTimeToStr(FileInfo.ModifiedTime)); // Last modified time
    end;

  end;
end;

// Executes at the beginning of the search process
procedure TMainForm.OnSearchBegin(Sender: TObject);
begin
  Self.Caption:= 'Search in progress..';
  PageControl1.Enabled := False; // Disable navigation during search
  StartButton.Enabled := False; // Disable the start button
  StopButton.Enabled := True; // Enable the stop button
  ProgressImagePanel.Visible := True; // Show progress animation
  ProgressImageTimer.Enabled := True; // Start animation timer
  ListView1.Clear; // Clear previous results
end;

function Ternary(Condition: Boolean; TrueValue, FalseValue: Integer): Integer;
begin
  if Condition then
    Result := TrueValue
  else
    Result := FalseValue;
end;

// Executes when the search process finishes
procedure TMainForm.OnSearchFinish(Sender: TObject);
var
  Comment:String;
  varf: String;
begin
   if CheckboxRequiredDirectory.Checked then
    varf := 'folders'
  else
    varf := 'files';

  // Generate a summary of the search
  Comment := IntToStr(TFindFile(Sender).FoldersSearchedCount) + ' folders searched and ' + IntToStr(TFindFile(Sender).FileCount) + ' ' + varf + ' found' + ' - ' + FloatToStr(TFindFile(Sender).ElapsedTime) + ' seconds ' ;
  ShowStatus(Comment);

  // If the search was aborted
  if FSearch.Aborted then
  begin
    ShowStatus('Search cancelled - ' + StatusBar1.SimpleText);
  end;

  // Hide progress animation
  ProgressImageTimer.Enabled := False;
  ProgressImagePanel.Visible := False;

  // Re-enable UI controls
  PageControl1.Enabled := True;
  StartButton.Enabled := True;
  StopButton.Enabled := False;

  Self.Caption:= 'Search '; // Reset window title
end;

// Updates the status bar with the provided text
procedure TMainForm.ShowStatus(Value: String);
begin
  Self.StatusBar1.SimpleText:='  '+ Value;
  Self.StatusBar1.Refresh; // Ensures the status bar updates immediately
end;

end.



