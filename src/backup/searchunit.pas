unit SearchUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Unix, BaseUnix, DateUtils;

type
  TFileMatchEvent = procedure(Sender: TObject; const FileName: String) of object;
  TFolderChangeEvent = procedure(Sender: TObject; const Folder: String; var IgnoreFolder: Boolean) of object;

  TFileAttributeCriteria = set of (faDirectory, faReadOnly, faHidden, faSysFile);
  TContentSearchOption = (csoCaseSensitive, csoWholeWord);
  TContentSearchOptions = set of TContentSearchOption;

  TFileCriteria = class
  private
    fFileMasks: TStringList;
    fRecursive: Boolean;
    fMinSize: Int64;
    fMaxSize: Int64;
    fMinDate: TDateTime;
    fMaxDate: TDateTime;
    fAttributes: TFileAttributeCriteria;
    fPhrase: String;
    fContentOptions: TContentSearchOptions;
    fMinLevel: Integer;
    fMaxLevel: Integer;
    function MatchesFileName(const FileName: String): Boolean;
    function MatchesSize(const FileInfo: Stat): Boolean;
    function MatchesDate(const FileInfo: Stat): Boolean;
    function MatchesAttributes(const FileInfo: Stat; const FileName: String): Boolean;
    function MatchesContent(const FileName: String): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property FileMasks: TStringList read fFileMasks;
    property Recursive: Boolean read fRecursive write fRecursive;
    property MinSize: Int64 read fMinSize write fMinSize;
    property MaxSize: Int64 read fMaxSize write fMaxSize;
    property MinDate: TDateTime read fMinDate write fMinDate;
    property MaxDate: TDateTime read fMaxDate write fMaxDate;
    property Attributes: TFileAttributeCriteria read fAttributes write fAttributes;
    property Phrase: String read fPhrase write fPhrase;
    property ContentOptions: TContentSearchOptions read fContentOptions write fContentOptions;
    property MinLevel: Integer read fMinLevel write fMinLevel;
    property MaxLevel: Integer read fMaxLevel write fMaxLevel;
  end;

  TSearchCriteria = class
  private
    fFiles: TFileCriteria;
    fSearchFolder: String;
  public
    constructor Create;
    destructor Destroy; override;
    property Files: TFileCriteria read fFiles write fFiles;
    property SearchFolder: String read fSearchFolder write fSearchFolder;
  end;

  TFindFileThread = class;

  TFindFile = class(TComponent)
  private
    fCriteria: TSearchCriteria;
    fOnFileMatch: TFileMatchEvent;
    fOnSearchBegin: TNotifyEvent;
    fOnSearchFinish: TNotifyEvent;
    fOnFolderChange: TFolderChangeEvent;
    fSearchThread: TFindFileThread;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
    procedure Abort;
    procedure WaitForThread; // Ajout de la méthode publique
    property Criteria: TSearchCriteria read fCriteria write fCriteria;
    property OnFileMatch: TFileMatchEvent read fOnFileMatch write fOnFileMatch;
    property OnSearchBegin: TNotifyEvent read fOnSearchBegin write fOnSearchBegin;
    property OnSearchFinish: TNotifyEvent read fOnSearchFinish write fOnSearchFinish;
    property OnFolderChange: TFolderChangeEvent read fOnFolderChange write fOnFolderChange;
  end;

  TFindFileThread = class(TThread)
  private
    fOwner: TFindFile;
    fCurrentFileName: String;
    fCurrentFolder: String;
    fIgnoreFolder: Boolean;
    fErrorMessage: String; // Ajoutez une variable pour stocker le message d'erreur
    procedure DoFileMatch;
    procedure DoFolderChange;
    procedure DoSearchBegin;
    procedure DoSearchFinish;
    procedure DoErrorMessage; // Ajoutez une méthode pour afficher le message d'erreur

  protected
    procedure Execute; override;
    procedure SearchIn(const Path: String; CurrentLevel: Integer);
  public
    constructor Create(AOwner: TFindFile);
  end;

function MatchesMask(const FileName, Mask: String): Boolean;

implementation

{ Fonction de correspondance de masque }
function MatchesMask(const FileName, Mask: String): Boolean;
var
  NameIndex, MaskIndex: Integer;
  NameLength, MaskLength: Integer;
begin
  Result := False;
  NameIndex := 1;
  MaskIndex := 1;
  NameLength := Length(FileName);
  MaskLength := Length(Mask);

  while (NameIndex <= NameLength) and (MaskIndex <= MaskLength) do
  begin
    if Mask[MaskIndex] = '*' then
    begin
      Inc(MaskIndex);
      if MaskIndex > MaskLength then
      begin
        Result := True;
        Exit;
      end;

      while (NameIndex <= NameLength) and not MatchesMask(Copy(FileName, NameIndex, MaxInt), Copy(Mask, MaskIndex, MaxInt)) do
        Inc(NameIndex);
      Result := NameIndex <= NameLength;
      Exit;
    end
    else if (Mask[MaskIndex] = '?') or (FileName[NameIndex] = Mask[MaskIndex]) then
    begin
      Inc(NameIndex);
      Inc(MaskIndex);
    end
    else
      Exit(False);
  end;

  Result := (MaskIndex > MaskLength) and (NameIndex > NameLength);
end;





{ TFileCriteria }

constructor TFileCriteria.Create;
begin
  inherited Create;
  fFileMasks := TStringList.Create;
  fMinLevel := 0;
  fMaxLevel := MaxInt;
end;

destructor TFileCriteria.Destroy;
begin
  fFileMasks.Free;
  inherited Destroy;
end;


function TFileCriteria.MatchesFileName(const FileName: String): Boolean;
var
  I: Integer;
begin
  Result := fFileMasks.Count = 0; // Accepte tous si aucun masque n'est défini
  if not Result then
  begin
    for I := 0 to fFileMasks.Count - 1 do
      if MatchesMask(FileName, fFileMasks[I]) then
        Exit(True);  // Masque correspondant trouvé
  end;
end;


{
function TFileCriteria.MatchesFileName(const FileName: String): Boolean;
var
  I: Integer;
  LowerFileName: String;
  LowerMask: String;
begin
  Result := False;
  LowerFileName := LowerCase(FileName); // Convertir le nom du fichier en minuscules

  // Parcours de tous les masques
  for I := 0 to fFileMasks.Count - 1 do
  begin
    LowerMask := LowerCase(Trim(fFileMasks[I])); // Convertir chaque masque en minuscules et supprimer les espaces

    // Comparer en ignorant la casse avec le masque
    if MatchesMask(LowerFileName, LowerMask) then
      Exit(True);
  end;
end;
}
    {
function TFileCriteria.MatchesFileName(const FileName: String): Boolean;
var
  I: Integer;
  LowerFileName: String;
  LowerMask: String;
begin
  Result := False;
  LowerFileName := LowerCase(FileName); // Convertit le nom du fichier en minuscules

  // Parcours de tous les masques
  for I := 0 to fFileMasks.Count - 1 do
  begin
    LowerMask := LowerCase(Trim(fFileMasks[I])); // Convertit chaque masque en minuscules et supprime les espaces

    // Comparer le nom de fichier avec le masque en ignorant la casse
    if MatchesMask(LowerFileName, LowerMask) then
      Exit(True); // Retourne True dès qu'un masque correspond
  end;
end;
     }

function TFileCriteria.MatchesSize(const FileInfo: Stat): Boolean;
begin
  Result := ((fMinSize = 0) or (FileInfo.st_size >= fMinSize)) and
            ((fMaxSize = 0) or (FileInfo.st_size <= fMaxSize));
end;

function TFileCriteria.MatchesDate(const FileInfo: Stat): Boolean;
var
  ModifiedDate: TDateTime;
begin
  ModifiedDate := UnixToDateTime(FileInfo.st_mtime);
  Result := ((fMinDate = 0) or (ModifiedDate >= fMinDate)) and
            ((fMaxDate = 0) or (ModifiedDate <= fMaxDate));
end;

function TFileCriteria.MatchesAttributes(const FileInfo: Stat; const FileName: String): Boolean;
begin
  Result := True;
  if faDirectory in fAttributes then
    Result := Result and FPS_ISDIR(FileInfo.st_mode);
  if faReadOnly in fAttributes then
    Result := Result and ((FileInfo.st_mode and S_IWUSR) = 0);
  if faHidden in fAttributes then
    Result := Result and (FileName[1] = '.');
  if faSysFile in fAttributes then
    Result := Result and ((FileInfo.st_mode and S_IRWXU) <> S_IRWXU) and
                         ((FileInfo.st_mode and S_IRWXG) = 0) and
                         ((FileInfo.st_mode and S_IRWXO) = 0);
end;

function TFileCriteria.MatchesContent(const FileName: String): Boolean;
var
  FileStream: TFileStream;
  FileContent: String;
  SearchContent: String;
begin
  Result := True;
  if fPhrase = '' then Exit;

  Result := False;
  try
    FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      SetLength(FileContent, FileStream.Size);
      FileStream.ReadBuffer(Pointer(FileContent)^, FileStream.Size);

      SearchContent := fPhrase;
      if not (csoCaseSensitive in fContentOptions) then
      begin
        FileContent := LowerCase(FileContent);
        SearchContent := LowerCase(SearchContent);
      end;

      if csoWholeWord in fContentOptions then
      begin
        Result := Pos(' ' + SearchContent + ' ', ' ' + FileContent + ' ') > 0;
      end
      else
      begin
        Result := Pos(SearchContent, FileContent) > 0;
      end;
    finally
      FileStream.Free;
    end;
  except
    Result := False;
  end;
end;

{ TSearchCriteria }

constructor TSearchCriteria.Create;
begin
  inherited Create;
  fFiles := TFileCriteria.Create;
end;

destructor TSearchCriteria.Destroy;
begin
  fFiles.Free;
  inherited Destroy;
end;

{ TFindFile }

constructor TFindFile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCriteria := TSearchCriteria.Create;
end;

destructor TFindFile.Destroy;
begin
  Abort;
  fCriteria.Free;
  inherited Destroy;
end;

procedure TFindFile.Execute;
begin
  Abort;
  fSearchThread := TFindFileThread.Create(Self);

end;

procedure TFindFile.Abort;
begin
  if Assigned(fSearchThread) then
  begin
    if assigned(fSearchThread) then
    begin
      try
    fSearchThread.Terminate;
    fSearchThread.WaitFor;
    FreeAndNil(fSearchThread);
      finally
      end;

    end;

  end;
end;

procedure TFindFile.WaitForThread;
begin
  if Assigned(fSearchThread) then
  begin
    fSearchThread.WaitFor;  // Attendre la fin du thread
    FreeAndNil(fSearchThread); // Libérer le thread une fois terminé
  end;
end;


{ TFindFileThread }

procedure TFindFileThread.DoErrorMessage;
var Text: String;
begin
  Text:=fErrorMessage; // Écrit le message d'erreur dans la console de débogage

end;

constructor TFindFileThread.Create(AOwner: TFindFile);
begin
  inherited Create(True); // Create suspended
  fOwner := AOwner;
  FreeOnTerminate := False;
  Start;
end;

procedure TFindFileThread.Execute;
var Erreur: String;
begin
  try
        Synchronize(@DoSearchBegin);
    // Code de recherche...
    SearchIn(IncludeTrailingPathDelimiter(fOwner.Criteria.SearchFolder), 0);
    Synchronize(@DoSearchFinish);
    // Code principal de recherche ici
    //DoSearchBegin;
    //try
      //SearchIn(IncludeTrailingPathDelimiter(fOwner.Criteria.SearchFolder), 0);
    //finally
    //  DoSearchFinish;
    //end;
  except
    on E: Exception do
    begin
      Erreur := 'Erreur dans le thread : ' + E.Message;
      //Synchronize(@DoErrorMessage); // Affiche le message dans le thread principal
    end;
  end;
end;

{
procedure TFindFileThread.SearchIn(const Path: String; CurrentLevel: Integer);
var
  SearchRec: TRawByteSearchRec;
  FileInfo: Stat;
  FullPath: String;
  I: Integer;
  varFileMaskEdit: String;
begin
  if Terminated then Exit;

  // Vérification de la profondeur de récursion
  if (CurrentLevel < fOwner.Criteria.Files.MinLevel) or
     ((fOwner.Criteria.Files.MaxLevel > 0) and (CurrentLevel > fOwner.Criteria.Files.MaxLevel)) then
    Exit;

  fCurrentFolder := IncludeTrailingPathDelimiter(Path);
  fIgnoreFolder := False;
  Synchronize(@DoFolderChange);

  // Si le dossier est marqué pour être ignoré, on sort
  if fIgnoreFolder then Exit;

  // Ensuite, rechercher les fichiers correspondant aux masques
  for I := 0 to fOwner.Criteria.Files.FileMasks.Count - 1 do
  begin
    varFileMaskEdit := Trim(fOwner.Criteria.Files.FileMasks[I]); // Supprimer les espaces
    //DebugLn('Recherche de fichiers dans : ', fCurrentFolder, ' avec masque : ', varFileMaskEdit);
    try
      // Utilisation de FindFirst pour chaque masque de fichier
      if FindFirst(fCurrentFolder + varFileMaskEdit, LongInt(faAnyFile), SearchRec) = 0 then
      try
        repeat
          if Terminated then Exit;

          // Ignorer '.' et '..'
          if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
          begin
            FullPath := fCurrentFolder + SearchRec.Name;

            try
              // Récupère les informations sur le fichier ou dossier actuel
              if fpStat(PChar(FullPath), FileInfo) = 0 then
              begin
                // Vérifier si le fichier correspond aux critères définis
                if fOwner.Criteria.Files.MatchesFileName(SearchRec.Name) {and
                   fOwner.Criteria.Files.MatchesSize(FileInfo) and
                   fOwner.Criteria.Files.MatchesDate(FileInfo) and
                   fOwner.Criteria.Files.MatchesAttributes(FileInfo, SearchRec.Name) and
                   fOwner.Criteria.Files.MatchesContent(FullPath)} then
                begin
                  fCurrentFileName := FullPath;
                  Synchronize(@DoFileMatch);
                end;
              end;
            except
              on E: Exception do
              begin
                fErrorMessage := 'Erreur d''accès au fichier ou dossier : ' + FullPath + ' - ' + E.Message;
                Synchronize(@DoErrorMessage);
              end;
            end;
          end;
        until FindNext(SearchRec) <> 0;
      finally
        FindClose(SearchRec); // Libère les ressources allouées par FindFirst
      end;
    except
      on E: Exception do
      begin
        fErrorMessage := 'Erreur dans le masque de fichier : ' + E.Message;
        Synchronize(@DoErrorMessage);
      end;
    end;
  end;

    // Recherche des sous-dossiers pour la récursion
  //DebugLn('Recherche de sous-dossiers dans : ', fCurrentFolder);
  if FindFirst(fCurrentFolder + '*', faAnyFile, SearchRec) = 0 then
  try
    repeat
      if Terminated then Exit;

      // Vérifie que ce n'est ni '.' ni '..' et que c'est bien un dossier
      FullPath := fCurrentFolder + SearchRec.Name;
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and
         (fpStat(PChar(FullPath), FileInfo) = 0) and FPS_ISDIR(FileInfo.st_mode) then
      begin
        //DebugLn('Sous-dossier trouvé : ', FullPath);

        if fOwner.Criteria.Files.Recursive then
        begin
          //DebugLn('Descente récursive dans le sous-dossier : ', FullPath);
          SearchIn(FullPath, CurrentLevel + 1);
        end;
      end;
    until FindNext(SearchRec) <> 0;
  finally
    FindClose(SearchRec);
  end;
end;
}

procedure TFindFileThread.SearchIn(const Path: String; CurrentLevel: Integer);
var
  SearchRec: TRawByteSearchRec;
  FileInfo: Stat;
  FullPath: String;
  Mask: String;
begin
  if Terminated then Exit;

  // Vérification de la profondeur de récursion
  if (CurrentLevel < fOwner.Criteria.Files.MinLevel) or
     ((fOwner.Criteria.Files.MaxLevel > 0) and (CurrentLevel > fOwner.Criteria.Files.MaxLevel)) then
    Exit;

  fCurrentFolder := IncludeTrailingPathDelimiter(Path);
  fIgnoreFolder := False;
  Synchronize(@DoFolderChange);
  if fIgnoreFolder then Exit; // Si le dossier est ignoré, on sort

  // Débogage - Afficher le dossier en cours
  //DebugLn('Recherche dans le dossier : ', fCurrentFolder);

  // Recherche de fichiers et de sous-dossiers
  if FindFirst(fCurrentFolder + '*', faAnyFile, SearchRec) = 0 then
  try
    repeat
      if Terminated then Exit;

      // Ignorer '.' et '..'
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        FullPath := fCurrentFolder + SearchRec.Name;

        // Récupération des informations du fichier ou du dossier
        if fpStat(PChar(FullPath), FileInfo) = 0 then
        begin
          // Si c'est un fichier, vérifiez le masque
          if not FPS_ISDIR(FileInfo.st_mode) then
          begin
            for Mask in fOwner.Criteria.Files.FileMasks do
            begin
              // Vérification du masque - Affichage pour le débogage
              //DebugLn('Vérification du fichier : ', SearchRec.Name, ' avec le masque : ', Mask);
              if MatchesMask(SearchRec.Name, Trim(Mask)) then
              begin
                //DebugLn('Fichier correspondant trouvé : ', FullPath);
                fCurrentFileName := FullPath;
                Synchronize(@DoFileMatch);
              end;
            end;
          end
          // Si c'est un dossier, procéder à la récursion
          else if FPS_ISDIR(FileInfo.st_mode) and fOwner.Criteria.Files.Recursive then
          begin
            DebugLn('Descente récursive dans le sous-dossier : ', FullPath);
            SearchIn(FullPath, CurrentLevel + 1);
          end;
        end;
      end;
    until FindNext(SearchRec) <> 0;
  finally
    FindClose(SearchRec);
  end;
end;





procedure TFindFileThread.DoSearchBegin;
begin
  if Assigned(fOwner.fOnSearchBegin) then
    fOwner.fOnSearchBegin(fOwner);
end;

procedure TFindFileThread.DoSearchFinish;
begin
  if Assigned(fOwner.fOnSearchFinish) then
    fOwner.fOnSearchFinish(fOwner);
end;

procedure TFindFileThread.DoFolderChange;
begin
  if Assigned(fOwner.fOnFolderChange) then
    fOwner.fOnFolderChange(fOwner, fCurrentFolder, fIgnoreFolder);
end;

procedure TFindFileThread.DoFileMatch;
begin
  if Assigned(fOwner.fOnFileMatch) then
    fOwner.fOnFileMatch(fOwner, fCurrentFileName);
end;

end.

