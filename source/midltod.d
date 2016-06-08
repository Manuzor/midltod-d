import std.stdio;
import std.algorithm;
import std.conv;
import std.format;
import std.range;
import std.array;
import std.string;
import std.ascii : isDigit;
import std.uni;
import std.file : read;
import std.path;
import std.traits;
import std.typecons;
import std.datetime;

char[] View(char[] String, size_t NumToView)
{
  return String[0 .. min($, NumToView)];
}

enum CodeType
{
  INVALID,

  Import,
  CppQuote,
  Constant,
  Enum,
  Aggregate,
  Interface,
  Function,
  Alias,
}

struct BlockData
{
  CodeType Type;
  union
  {
    ImportData Import;
    CppQuoteData CppQuote;
    ConstantData Constant;
    EnumData Enum;
    AggregateData Aggregate;
    InterfaceData Interface;
    FunctionData Function;
    AliasData Alias;
  }

  string toString() const
  {
    final switch(Type)
    {
      case CodeType.Import:    return "BlockData(%s)".format(Import);
      case CodeType.CppQuote:  return "BlockData(%s)".format(CppQuote);
      case CodeType.Constant:  return "BlockData(%s)".format(Constant);
      case CodeType.Enum:      return "BlockData(%s)".format(Enum);
      case CodeType.Aggregate: return "BlockData(%s)".format(Aggregate);
      case CodeType.Interface: return "BlockData(%s)".format(Interface);
      case CodeType.Function:  return "BlockData(%s)".format(Function);
      case CodeType.Alias:     return "BlockData(%s)".format(Alias);
      case CodeType.INVALID: assert(0);
    }
  }
}

struct ImportData
{
  char[] Filename;
}

struct CppQuoteData
{
  char[] Content;
}

struct ConstantData
{
  char[] Type;
  char[] Name;
  char[] Value;
}

struct EnumData
{
  static struct Entry
  {
    char[] Key;
    char[] Value;
  }

  char[] Name;
  Entry[] Entries;
}

struct Declaration
{
  AttributeData[] Attributes;
  char[] Type;
  Declaration[] Body; // Only valid if Type == "struct" || Type == "union"
  char[] Name;
  char[][] ArrayCounts; // e.g. int Foo[MAX_STUFF][3];
}

struct AggregateData
{
  enum Type
  {
    Struct,
    Union,
  }

  Type AggregateType;
  char[] Name;
  Declaration[] Members;
}

struct InterfaceData
{
  AttributeData[] Attributes;
  char[] Name;
  char[] ParentName;
  FunctionData[] Functions;
}

struct FunctionData
{
  char[] ReturnType;
  char[] Name;
  Declaration[] Params;
}

struct AliasData
{
  char[] NewName;
  char[] OldName;
}

enum AttributeType
{
  INVALID,

  // Mostly used for interfaces:
  Object,
  Uuid,
  Local,
  PointerDefault,

  // Mostly used in functions:
  In,
  Out,
  Retval,
  SizeIs,
  Annotation,
}

struct AttributeData
{
  AttributeType Type;
  char[] Value;

  string toString() const
  {
    string Result;
    final switch(Type)
    {
      case AttributeType.Object:          Result = "object";          break;
      case AttributeType.Uuid:            Result = "uuid";            break;
      case AttributeType.Local:           Result = "local";           break;
      case AttributeType.PointerDefault:  Result = "pointer_default"; break;
      case AttributeType.In:              Result = "in";              break;
      case AttributeType.Out:             Result = "out";             break;
      case AttributeType.Retval:          Result = "retval";          break;
      case AttributeType.SizeIs:          Result = "size_is";         break;
      case AttributeType.Annotation:      Result = "annotation";      break;

      case AttributeType.INVALID: assert(0);
    }

    if(Value.length)
    {
      Result ~= `("` ~ Value ~ `")`;
    }

    return Result;
  }
}

class FormattedOutput
{
  int IndentationLevel;
  string Newline = "\n";

  File OutFile;

  @property auto Indentation() { return ' '.repeat(IndentationLevel); }
  void Indent(int Amount = 2)
  {
    IndentationLevel += Amount;
  }

  void Outdent(int Amount = 2)
  {
    IndentationLevel = max(0, IndentationLevel - Amount);
  }

  void WriteIndentation()
  {
    this.write(Indentation);
  }

  alias OutFile this;
}

FormattedOutput Log;

void ParseImport(ref char[] Source, ref BlockData[] Blocks)
{
  Log.writeln(Log.Indentation, "=== Parsing Import ===");

  assert(Source.startsWith("import"), Source.View(100));
  Source.popFrontN("import".length);
  assert(Source.front.isWhite, Source.View(100));

  Source.SkipWhiteSpace();

  assert(Source.front == '"');
  Source.popFront();

  ImportData Result;
  Result.Filename = Source.ParseEscapableString('"');
  Log.writeln(Log.Indentation, "Filename: ", Result.Filename);

  Source.SkipWhiteSpace();

  assert(Source.front == ';');

  Source.popFront();

  auto Block = BlockData(CodeType.Import);
  Block.Import = Result;
  Blocks ~= Block;
}

void ParseCppQuote(ref char[] Source, ref BlockData[] Blocks)
{
  Log.writeln(Log.Indentation, "=== Parsing CppQuote ===");

  CppQuoteData Result;

  assert(Source.startsWith("cpp_quote"));
  Source.popFrontN("cpp_quote".length);

  Source.SkipWhiteSpace();

  assert(Source.front == '(');
  Source.popFront();

  Source.SkipWhiteSpace();

  assert(Source.front == '"');
  Source.popFront();

  Result.Content = Source.ParseEscapableString('"').strip();
  Log.writeln(Log.Indentation, "Content: ", Result.Content);

  Source.SkipWhiteSpace();

  assert(Source.front == ')', Source.View(100));
  Source.popFront();

  auto Block = BlockData(CodeType.CppQuote);
  Block.CppQuote = Result;
  Blocks ~= Block;
}

void ParseConstant(ref char[] Source, ref BlockData[] Blocks)
{
  Log.writeln(Log.Indentation, "=== Parsing Constant ===");

  ConstantData Result;

  // Skip 'const' or '#define'
  auto FirstToken = Source.FastForwardUntil!isWhite;
  assert(FirstToken == "const" || FirstToken == "#define", FirstToken);

  if(FirstToken == "const")
  {
    Source.SkipWhiteSpace();

    Result.Type = Source.FastForwardUntil!isWhite;
    Log.writeln(Log.Indentation, "Type: ", Result.Type);

    Result.Name = Source.FastForwardUntil!(Char => Char == '=').strip;
    Log.writeln(Log.Indentation, "Name: ", Result.Name);

    Result.Value = Source.FastForwardUntil!(Char => Char == ';').strip;
    Log.writeln(Log.Indentation, "Value: ", Result.Value);
  }
  else
  {
    Source.SkipWhiteSpace();

    Result.Name = Source.FastForwardUntil!isWhite.strip;
    Log.writeln(Log.Indentation, "Name: ", Result.Name);

    Result.Value = Source.FastForwardUntil!(Char => Char == '\n').strip;
    Log.writeln(Log.Indentation, "Value: ", Result.Value);
  }

  auto Block = BlockData(CodeType.Constant);
  Block.Constant = Result;
  Blocks ~= Block;
}

void ParseEnum(ref char[] Source, ref BlockData[] Blocks)
{
  Log.writeln(Log.Indentation, "=== Parsing Enum ===");

  EnumData Result;

  // Skip 'typedef'
  auto TypedefToken = Source.FastForwardUntil!isWhite;
  assert(TypedefToken == "typedef");

  Source.SkipWhiteSpace();

  // Skip 'enum'
  auto EnumToken = Source.FastForwardUntil!isWhite;
  assert(EnumToken == "enum");

  // Read everything between 'enum' and '{'
  auto TagName = Source.FastForwardUntil!(Char => Char == '{').strip;
  Log.writeln(Log.Indentation, "Tag Name: ", TagName);

  auto Body = Source.FastForwardUntil!(Char => Char == '}');

  Result.Name = Source.FastForwardUntil!(Char => Char == ';').strip;
  Log.writeln(Log.Indentation, "Name: ", Result.Name);

  Log.Indent();

  while(true)
  {
    Body.SkipWhiteSpace();

    if(Body.empty) break;

    Result.Entries.length++;
    auto Entry = &Result.Entries.back;
    scope(failure) Log.writeln(Log.Indentation, "Entry: ", *Entry);

    auto EntryCode = Body.FastForwardUntil!(Char => Char == ',');
    EntryCode.SkipWhiteSpace();

    Entry.Key = EntryCode.FastForwardUntil!(Char => Char == '=').strip;
    Log.writeln(Log.Indentation, "Key: ", Entry.Key);

    EntryCode.SkipWhiteSpace();

    Entry.Value = EntryCode.strip; // The rest of the code.
    Log.writeln(Log.Indentation, "Value: ", Entry.Value);
  }

  Log.Outdent();

  auto Block = BlockData(CodeType.Enum);
  Block.Enum = Result;
  Blocks ~= Block;
}

/// Params:
///   FieldDelimiter = Usually ';' for structs and unions or ',' for function parameters.
void ParseDeclaration(ref char[] Body, ref Declaration Decl, dchar FieldDelimiter)
{
  Log.writeln(Log.Indentation, "=== Parsing Declaration ===");
  Log.Indent();
  scope(exit) Log.Outdent();

  TryParseAttributes(Body, Decl.Attributes);

  Body.SkipWhiteSpace();

  auto TempBody = Body;
  auto Prefix = TempBody.FastForwardUntil!isWhite;
  if(Prefix == "struct" || Prefix == "union")
  {
    Log.writeln(Log.Indentation, "Declaration is an inner aggregate type.");

    Decl.Type = Body.FastForwardUntil!isWhite;
    assert(Decl.Type == Prefix);

    auto InnerTagName = Body.FastForwardUntil!(Char => Char == '{').strip;
    if(InnerTagName.length)
    {
      Log.writeln(Log.Indentation, "Tag Name: ", InnerTagName);
    }
    else
    {
      Log.writeln(Log.Indentation, "No tag name found.");
    }

    auto InnerBody = Body.ParseNestedString('{', '}');
    auto InnerName = Body.FastForwardUntil!(Char => Char == FieldDelimiter).strip;

    Log.writeln(Log.Indentation, "Name: ", InnerName);

    while(true)
    {
      InnerBody.SkipWhiteSpace();

      if(InnerBody.empty) break;

      Declaration InnerDecl;
      scope(failure) Log.writeln(Log.Indentation, "Inner Declaration: ", InnerDecl);

      ParseDeclaration(InnerBody, InnerDecl, ';');
      Decl.Body ~= InnerDecl;
    }

    Log.Outdent();
  }
  else
  {
    Log.writeln(Log.Indentation, "Declaration is a regular field.");

    auto FieldCode = Body.FastForwardUntil!(Char => Char == FieldDelimiter);

    FieldCode = FieldCode.strip();

    if(FieldCode == "void")
    {
      Decl.Type = FieldCode;
      return;
    }

    while(FieldCode.back == ']')
    {
      FieldCode.popBack();
      auto ArrayCountLength = FieldCode.retro.countUntil('[');
      Decl.ArrayCounts ~= FieldCode[$ - ArrayCountLength .. $].strip;
      FieldCode.popBackN(ArrayCountLength + 1);
    }

    auto SlackUntilDelimiter = FieldCode.retro.countUntil!(Char => Char.isWhite || Char == '*');

    Decl.Type = FieldCode[0                       .. $ - SlackUntilDelimiter].strip;
    Decl.Name = FieldCode[$ - SlackUntilDelimiter .. $                      ].strip;

    Log.writeln(Log.Indentation, "Type: ", Decl.Type);
    Log.writeln(Log.Indentation, "Name: ", Decl.Name);
  }
}

void ParseAggregate(ref char[] Source, ref BlockData[] Blocks)
{
  Log.writeln(Log.Indentation, "=== Parsing Aggregate ===");

  AggregateData Result;

  auto TypedefToken = Source.FastForwardUntil!isWhite;
  assert(TypedefToken == "typedef");

  Source.SkipWhiteSpace();

  auto FirstToken = Source.FastForwardUntil!isWhite;

  if(FirstToken == "struct")
  {
    Result.AggregateType = AggregateData.Type.Struct;
  }
  else if(FirstToken == "union")
  {
    Result.AggregateType = AggregateData.Type.Union;
  }
  else assert(0);

  // Read everything between 'struct' and '{'
  auto TagName = Source.FastForwardUntil!(Char => Char == '{').strip;
  if(TagName.length)
  {
    Log.writeln(Log.Indentation, "Tag Name: ", TagName);
  }
  else
  {
    Log.writeln(Log.Indentation, "No tag name found.");
  }

  auto Body = Source.ParseNestedString('{', '}');

  Result.Name = Source.FastForwardUntil!(Char => Char == ';').strip;
  assert(Result.Name.length);
  Log.writeln(Log.Indentation, "Name: ", Result.Name);

  Log.Indent();

  while(true)
  {
    Body.SkipWhiteSpace();

    if(Body.empty) break;

    Declaration Member;
    scope(failure) Log.writeln(Log.Indentation, "Member: ", Member);

    ParseDeclaration(Body, Member, ';');

    Result.Members ~= Member;
  }

  Log.Outdent();

  auto Block = BlockData(CodeType.Aggregate);
  Block.Aggregate = Result;
  Blocks ~= Block;

  Log.writeln(Log.Indentation, "New Block: ", Blocks.back);
}

char[] ParseNestedString(ref char[] Source, char FrontChar, char BackChar, Flag!"CanExhaustSource" CanExhaustSource = Yes.CanExhaustSource)
{
  auto ParsedSource = Source;
  int NumBackCharsNeeded = 1;

  do
  {
    char Delimiter;
    ParsedSource.FastForwardUntil!(Char => Char == FrontChar || Char == BackChar)(&Delimiter);

    if(Delimiter == FrontChar)
    {
      NumBackCharsNeeded++;
    }
    else if(Delimiter == BackChar)
    {
      NumBackCharsNeeded--;
    }
    else
    {
      assert(CanExhaustSource, "Source got exhausted before the matching closing delimiter was found.");
      break;
    }
  } while(NumBackCharsNeeded);

  auto Result = Source[0 .. $ - min(Source.length, ParsedSource.length + 1)];
  Source = ParsedSource;
  return Result;
}

char[] ParseEscapableString(ref char[] Source, char Delimiter)
{
  enum char Escape = '\\';

  auto Target = Source;

  while(true)
  {
    Target = Target.find(Escape, Delimiter)[0];

    if(Target.empty || Target.front == Delimiter) break;

    if(Target.front == Escape)
    {
      Target.popFront();
      assert(Target.length, Target.View(100));
      // Skip the escaped character.
      Target.popFront();
    }
  }

  auto Result = Source[0 .. $ - Target.length];
  if(Target.length) Target.popFront();
  Source = Target;
  return Result;
}

void TryParseAttributes(ref char[] Source, ref AttributeData[] Attributes)
{
  if(Source.front != '[') return;
  Source.popFront();

  auto AttributeSource = Source.ParseNestedString('[', ']');

  typeof(return)[] Result;

  while(true)
  {
    AttributeSource.SkipWhiteSpace();

    if(AttributeSource.empty) break;

    AttributeType NameToAttributeType(char[] Name)
    {
      switch(Name)
      {
        case "object":          return AttributeType.Object;
        case "uuid":            return AttributeType.Uuid;
        case "local":           return AttributeType.Local;
        case "pointer_default": return AttributeType.PointerDefault;
        case "in":              return AttributeType.In;
        case "out":             return AttributeType.Out;
        case "retval":          return AttributeType.Retval;
        case "size_is":         return AttributeType.SizeIs;
        case "annotation":      return AttributeType.Annotation;
        default: assert(0, "Unknown IDL attribute: " ~ Name);
      }
    }

    char Delimiter;
    auto Name = AttributeSource.FastForwardUntil!(Char => Char == ',' || Char == '(')(&Delimiter).stripRight();

    AttributeData Attribute;
    Attribute.Type = NameToAttributeType(Name);

    if(Delimiter == '(')
    {
      Attribute.Value = AttributeSource.ParseNestedString('(', ')').strip;
      if(Attribute.Value.front == '"')
      {
        assert(Attribute.Value.back == '"');
        Attribute.Value.popFront();
        Attribute.Value.popBack();
      }

      AttributeSource.SkipWhiteSpace();
      if(AttributeSource.length && AttributeSource.front == ',') AttributeSource.popFront();
    }

    Attributes ~= Attribute;
  }
}

void ParseInterface(ref char[] Source, ref BlockData[] Blocks)
{
  Log.writeln(Log.Indentation, "=== Parsing Interface ===");

  InterfaceData Result;

  TryParseAttributes(Source, Result.Attributes);

  Source.SkipWhiteSpace();

  assert(Source.startsWith("interface"));
  Source.FastForwardUntil!isWhite;

  Source.SkipWhiteSpace();

  char Delimiter;
  Result.Name = Source.FastForwardUntil!(Char => Char == ';' || Char == ':' || Char == '{')(&Delimiter).strip();
  Log.writeln(Log.Indentation, "Name: ", Result.Name);

  Source.SkipWhiteSpace();

  if(Delimiter == ';')
  {
    Log.writeln(Log.Indentation, "Ignoring forward declaration");
    return;
  }

  if(Delimiter == ':')
  {
    Result.ParentName = Source.FastForwardUntil!(Char => Char == '{')(&Delimiter).strip();
    Log.writeln(Log.Indentation, "ParentName: ", Result.ParentName);
  }

  assert(Delimiter == '{');

  auto Body = Source.FastForwardUntil!(Char => Char == '}');

  // d3d11.idl contains an interface without a closing ';', so we can't assert
  // for its existance. Not sure if this is the only case.
  while(Source.front == ';') Source.popFront();

  Log.writeln(Log.Indentation, "Body: ", Body);

  Log.Indent();

  while(true)
  {
    Body.SkipWhiteSpace();

    if(Body.empty) break;

    BlockData[] FunctionBlocks;
    ParseFunction(Body, FunctionBlocks);
    foreach(ref Block ; FunctionBlocks)
    {
      assert(Block.Type == CodeType.Function);
      Result.Functions ~= Block.Function;
    }
  }

  Log.Outdent();

  Blocks ~= BlockData(CodeType.Interface);
  Blocks[$-1].Interface = Result;
}

void ParseFunction(ref char[] Source, ref BlockData[] Blocks)
{
  Log.writeln(Log.Indentation, "=== Parsing Function ===");

  FunctionData Result;

  Result.ReturnType = Source.FastForwardUntil!isWhite;
  Log.writeln(Log.Indentation, "Return Type: ", Result.ReturnType);

  Source.SkipWhiteSpace();

  Result.Name = Source.FastForwardUntil!(Char => Char == '(').stripRight();
  Log.writeln(Log.Indentation, "Name: ", Result.Name);

  auto ParamSource = Source.FastForwardUntil!(Char => Char == ';');
  Log.writeln(Log.Indentation, "Param Source: ", ParamSource);
  assert(ParamSource.back == ')');
  ParamSource.popBack();

  Log.Indent();

  while(true)
  {
    ParamSource.SkipWhiteSpace();

    if(ParamSource.empty) break;

    Declaration Param;
    ParseDeclaration(ParamSource, Param, ',');
    if(Param.Type != "void") Result.Params ~= Param;
  }

  Log.Outdent();

  Blocks ~= BlockData(CodeType.Function);
  Blocks[$-1].Function = Result;
}

void ParseAlias(ref char[] Source, ref BlockData[] Blocks)
{
  Log.writeln(Log.Indentation, "=== Parsing Alias ===");

  auto Result = BlockData(CodeType.Alias);
  Result.Alias = AliasData.init;

  auto Body = Source.FastForwardUntil!(Char => Char == ';');

  auto TypedefToken = Body.FastForwardUntil!isWhite;
  assert(TypedefToken == "typedef");

  Result.Alias.OldName = Body.FastForwardUntil!isWhite;
  Log.writeln(Log.Indentation, "Old name: ", Result.Alias.OldName);

  Result.Alias.NewName = Body.strip; // The rest.
  Log.writeln(Log.Indentation, "New name: ", Result.Alias.NewName);

  Blocks ~= Result;
}

bool IsDelimiter(CharType)(CharType Char)
{
  return Char.isWhite ||
         Char == '(' ||
         Char == ')' ||
         Char == '[' ||
         Char == ']' ||
         Char == '{' ||
         Char == '}' ||
         Char == '=' ||
         Char == ',' ||
         Char == ';';
}

/// Advances Source until Predicate(a) is true and returns the part of Source as a slice that has been skipped.
/// The matching char is written to LastCharOutPtr and not included in the return value or the advanced Source.
char[] FastForwardUntil(alias Predicate)(ref char[] Source, char* LastCharOutPtr = null)
{
  auto NewSource = Source.find!Predicate();

  if(NewSource.empty)
  {
    swap(NewSource, Source);
    return NewSource;
  }

  auto Result = Source[0 .. $ - NewSource.length];
  if(NewSource.length)
  {
    if(LastCharOutPtr) *LastCharOutPtr = NewSource[0];
    NewSource.popFront();
  }

  Source = NewSource;
  return Result;
}

// Skips white space and comments.
void SkipWhiteSpace(ref char[] Source)
{
  Source = Source.stripLeft();
}

enum ContextCharCountThreshold = 60;

BlockData[] Parse(ref char[] Source, int[] Stats)
{
  typeof(return) Result;

  while(true)
  {
    Source.SkipWhiteSpace();

    if(Source.empty) break;

    Log.writefln("Looking at: %s...", Source.View(ContextCharCountThreshold));
    Log.Indent();

    if(Source.startsWith("#pragma"))
    {
      Source.FastForwardUntil!(Char => Char == '\n');
    }
    else if(Source.startsWith("import"))
    {
      ParseImport(Source, Result);
      Stats[CodeType.Import]++;
    }
    else if(Source.startsWith("const") || Source.startsWith("#define"))
    {
      ParseConstant(Source, Result);
      Stats[CodeType.Constant]++;
    }
    else if(Source.startsWith("cpp_quote"))
    {
      ParseCppQuote(Source, Result);
      Stats[CodeType.CppQuote]++;
    }
    else if(Source.startsWith("typedef"))
    {
      auto SourceCopy = Source;
      SourceCopy.FastForwardUntil!isWhite;
      SourceCopy.SkipWhiteSpace();

      if(SourceCopy.startsWith("enum"))
      {
        ParseEnum(Source, Result);
        Stats[CodeType.Enum]++;
      }
      else if(SourceCopy.startsWith("struct") || SourceCopy.startsWith("union"))
      {
        ParseAggregate(Source, Result);
        Stats[CodeType.Aggregate]++;
      }
      else
      {
        ParseAlias(Source, Result);
        Stats[CodeType.Alias]++;
      }
    }
    else if(Source.startsWith("[") || Source.startsWith("interface"))
    {
      ParseInterface(Source, Result);
      Stats[CodeType.Interface]++;
    }
    else
    {
      assert(0, "Unknown token at: " ~ Source[0 .. min(ContextCharCountThreshold, Source.length)]);
    }

    Log.Outdent();
    Log.writeln('-'.repeat(10));
  }

  return Result;
}

void EmitConstant(ref ConstantData Constant, FormattedOutput Output)
{
  Output.writef("%senum ", Output.Indentation);
  if(Constant.Type) Output.write(Constant.Type, " ");
  auto Value = Constant.Value;
  while(Value.back.toUpper == 'L') Value.popBack();
  if(Value.back.toUpper == 'U') Value.popBack();
  Output.writef("%s = %s;%s", Constant.Name, Value, Output.Newline);
}

void EmitEnum(ref EnumData Enum, FormattedOutput Output)
{
  const BaseTypeName = "int";
  Output.write(Output.Indentation, "alias ", Enum.Name, " = ", BaseTypeName, ';', Output.Newline);
  Output.write(Output.Indentation, "enum : ", Enum.Name, Output.Newline,
               Output.Indentation, "{", Output.Newline);

  Output.Indent();

  auto Entries = Enum.Entries.dup;
  auto Prefix = commonPrefix(Enum.Name, Entries[0].Key);

  ulong MaxLen;
  foreach(ref Entry; Entries)
  {
    MaxLen = max(MaxLen, Entry.Key.length);
  }

  foreach(ref Entry; Entries)
  {
    Output.write(Output.Indentation, Entry.Key);
    if(Entry.Value.length)
    {
      Output.writef("%s = %s", ' '.repeat(MaxLen - Entry.Key.length), Entry.Value);
    }
    Output.write(",", Output.Newline);
  }
  Output.Outdent();
  Output.write(Output.Indentation, "}", Output.Newline);
}

void EmitDeclaration(ref Declaration Decl, FormattedOutput Output, Flag!"IsFunctionParam" IsFunctionParam)
{
  // Add an extra newline here in attempt to make the interface look nicer for
  // inner aggregate types.
  if(Decl.Body) Output.write(Output.Newline);

  if(Decl.Attributes)
  {
    Output.writef("%s// %([%s]%)]%s", Output.Indentation, Decl.Attributes, Output.Newline);
  }

  if(Decl.Body)
  {
    assert(!IsFunctionParam, "Function parameters in D can't be inline aggregate type declarations (probably).");
    assert(Decl.Type == "struct" || Decl.Type == "union", Decl.Type);

    Output.write(Output.Indentation);

    // Inner structs should be static.
    if(Decl.Type == "struct") Output.write("static ");

    Output.write(Decl.Type, " ", Decl.Name, Output.Newline,
                 Output.Indentation, '{', Output.Newline);

    Output.Indent();

    foreach(ref Member; Decl.Body)
    {
      EmitDeclaration(Member, Output, No.IsFunctionParam);
    }

    Output.Outdent();

    Output.write(Output.Indentation, '}');
  }
  else
  {
    auto Type = Decl.Type;
    const IsInterfaceType = Type.front == 'I' && Type.canFind!(Char => Char.isLower());
    const NumPointers = Type.count('*');
    const IsConst = Type.canFind(" const", "const ");

    char[] TypeBaseName;
    do
    {
      TypeBaseName = Type.FastForwardUntil!(Char => Char.isWhite || Char == '*');
      Type.SkipWhiteSpace();
    } while(TypeBaseName == "const");

    if(IsInterfaceType) assert(NumPointers > 0);

    Output.write(Output.Indentation);

    if(IsConst) Output.write(IsFunctionParam ? "in " : "const ");

    auto Pointers = '*'.repeat(IsInterfaceType ? NumPointers - 1 : NumPointers);
    Output.write(TypeBaseName, Pointers);

    foreach(Count; Decl.ArrayCounts)
    {
      Output.writef("[%s]", Count);
    }

    Output.write(" ", Decl.Name, IsFunctionParam ? ',' : ';');
  }

  Output.write(Output.Newline);
}

void EmitAggregate(ref AggregateData Aggregate, FormattedOutput Output)
{
  Log.writeln(Log.Indentation, "Emitting aggregate: ", Aggregate);

  Output.write(Output.Indentation);

  final switch(Aggregate.AggregateType)
  {
    case AggregateData.Type.Struct: Output.write("struct "); break;
    case AggregateData.Type.Union:  Output.write("union ");  break;
  }

  Output.write(Aggregate.Name, Output.Newline,
               Output.Indentation, "{", Output.Newline);

  Output.Indent();

  foreach(ref Member; Aggregate.Members[])
  {
    EmitDeclaration(Member, Output, No.IsFunctionParam);
  }

  Output.Outdent();

  Output.write(Output.Indentation, "}", Output.Newline);
}

void EmitInterface(ref InterfaceData Interface, FormattedOutput Output)
{
  char[] Uuid = Interface.Attributes.find!(Attr => Attr.Type == AttributeType.Uuid).front.Value;
  Output.write(Output.Indentation, "mixin DEFINE_GUID!(", Interface.Name, `, "`, Uuid, `");`, Output.Newline);

  if(Interface.Attributes)
  {
    Output.writef("%s// %([%s]%)]%s", Output.Indentation, Interface.Attributes, Output.Newline);
  }

  if(Interface.ParentName)
  {
    Output.write(Output.Indentation, "interface ", Interface.Name, " : ", Interface.ParentName, Output.Newline);
  }
  else
  {
    Output.write(Output.Indentation, "interface ", Interface.Name, Output.Newline);
  }

  Output.write(Output.Indentation, "{", Output.Newline,
               Output.Indentation, "extern(Windows):", Output.Newline,
               Output.Newline);

  Output.Indent();

  foreach(ref Function; Interface.Functions)
  {
    EmitFunction(Function, Output, No.AddExternWindows);
    Output.write(Output.Newline);
  }

  Output.Outdent();

  Output.write(Output.Indentation, "}", Output.Newline);
}

void EmitFunction(ref FunctionData Function, FormattedOutput Output, Flag!"AddExternWindows" AddExternWindows = Yes.AddExternWindows)
{
  scope(failure) Log.writeln("Failure emitting function: ", Function);

  Output.write(Output.Indentation);
  if(AddExternWindows) Output.write("extern(Windows) ");
  Output.write(Function.ReturnType, " ", Function.Name, "(");

  if(Function.Params)
  {
    Output.write(Output.Newline);

    Output.Indent();

    foreach(ref Param; Function.Params)
    {
      EmitDeclaration(Param, Output, Yes.IsFunctionParam);
    }

    Output.Outdent();
    Output.write(Output.Indentation);
  }

  Output.write(");", Output.Newline);
}

void EmitAlias(ref AliasData Alias, FormattedOutput Output)
{
  Output.write(Output.Indentation);
  Output.writef("alias %s = %s;%s", Alias.NewName, Alias.OldName, Output.Newline);
}

void EmitImport(ref ImportData Import, FormattedOutput Output)
{
  Output.write(Output.Indentation, "import ", Import.Filename.stripExtension, ';', Output.Newline);
}

void EmitCppQuote(ref CppQuoteData CppQuote, FormattedOutput Output)
{
  Output.write(Output.Indentation, CppQuote.Content, Output.Newline);
}

void EmitD3DCOLORVALUE(ref AliasData Alias, FormattedOutput Output)
{
  assert(Alias.OldName == "D3DCOLORVALUE");
  Output.write(Output.Indentation, "struct ", Alias.NewName, Output.Newline,
               Output.Indentation, "{", Output.Newline);

  Output.Indent();

  Output.write(Output.Indentation, "float r;", Output.Newline,
               Output.Indentation, "float g;", Output.Newline,
               Output.Indentation, "float b;", Output.Newline,
               Output.Indentation, "float a;", Output.Newline);

  Output.Outdent();

  Output.write(Output.Indentation, "}", Output.Newline);
}

void EmitBlocks(BlockData[] Blocks, FormattedOutput Output, int CppQuoteBatchId = 0)
{
  while(Blocks.length)
  {
    auto Block = &Blocks.front;
    Blocks.popFront();

    Output.write(Output.Newline);

    final switch(Block.Type)
    {
      case CodeType.Import:
      {
        EmitImport(Block.Import, Output);
        break;
      }
      case CodeType.Constant:
      {
        EmitConstant(Block.Constant, Output);
        break;
      }
      case CodeType.Enum:
      {
        EmitEnum(Block.Enum, Output);
        break;
      }
      case CodeType.Aggregate:
      {
        EmitAggregate(Block.Aggregate, Output);
        break;
      }
      case CodeType.Interface:
      {
        EmitInterface(Block.Interface, Output);
        break;
      }
      case CodeType.Function:
      {
        EmitFunction(Block.Function, Output);
        break;
      }
      case CodeType.Alias:
      {
        if(Block.Alias.OldName == "D3DCOLORVALUE")
        {
          EmitD3DCOLORVALUE(Block.Alias, Output);
        }
        else
        {
          EmitAlias(Block.Alias, Output);
        }

        break;
      }
      case CodeType.CppQuote:
      {
        // cpp_quotes are emitted in batches.
        const NumConsecutiveCppQuotes = Blocks.countUntil!(Block => Block.Type != CodeType.CppQuote);
        scope(exit) CppQuoteBatchId += NumConsecutiveCppQuotes;

        Output.write(Output.Indentation, "// Begin cpp_quote #", CppQuoteBatchId);
        if(NumConsecutiveCppQuotes > 1) Output.write("-", CppQuoteBatchId + NumConsecutiveCppQuotes - 1);
        Output.write(Output.Newline);

        while(true)
        {
          EmitCppQuote(Block.CppQuote, Output);
          if(Blocks.empty) break;
          Block = &Blocks.front;
          if(Block.Type != CodeType.CppQuote) break;
          Blocks.popFront();
        }

        Output.write(Output.Indentation, "// End cpp_quote #", CppQuoteBatchId);
        if(NumConsecutiveCppQuotes > 1) Output.write("-", CppQuoteBatchId + NumConsecutiveCppQuotes - 1);
        Output.write(Output.Newline);

        break;
      }
      case CodeType.INVALID: assert(0, Block.Type.to!string);
    }
  }
}

char[] ReadTextAsUTF8(in string Filename)
{
  return Filename.read().to!(char[]);
}

// Replaces all comments with spaces.
char[] Preprocess(char[] Source)
{
  auto Search = Source;
  while(Search.length)
  {
    Search = Search.find("//", "/*", "cpp_quote")[0];

    if(Search.startsWith("//"))
    {
      auto Garbage = Search.FastForwardUntil!(Char => Char == '\n');
      Garbage[] = ' ';
    }
    else if(Search.startsWith("/*"))
    {
      auto ContinueHere = Search.find("*/");
      if(ContinueHere.length) ContinueHere.popFrontN(2);
      auto Garbage = Search[0 .. $ - ContinueHere.length];
      Search = ContinueHere;
      Garbage[] = ' ';
    }
    else if(Search.startsWith("cpp_quote"))
    {
      Search.popFrontN("cpp_quote".length);
      Search.SkipWhiteSpace();
      assert(Search.front == '(');
      Search.popFront();
      Search.SkipWhiteSpace();
      assert(Search.front == '"');
      Search.popFront();
      Search.ParseEscapableString('"');
    }
  }

  return Source;
}

void main(string[] Args)
{
  const Program = Args[0];
  Args = Args[1 .. $];

  assert(Args.length == 2, "Need 2 arguments.");

  Log = new FormattedOutput();
  Log.OutFile = stderr;

  auto InFilename  = Args[0];
  auto OutFilename = Args[1];

  char[] InputBuffer = ReadTextAsUTF8(InFilename);

  InputBuffer = InputBuffer.Preprocess();

  int[CodeType.max + 1] Stats;
  auto Blocks = Parse(InputBuffer, Stats);

  auto Output = new FormattedOutput();
  if(OutFilename == "-")
  {
    Output.OutFile = stdout;
  }
  else
  {
    Output.OutFile.open(OutFilename, "w");
  }

  Log.writeln('='.repeat(72));

  Output.write("// Original file name: ", InFilename.baseName, Output.Newline);
  Output.write("// Conversion date: ", Clock.currTime, Output.Newline);
  Output.write(OutContentHeader);

  if(Stats[CodeType.CppQuote])
  {
    Output.writef(`static assert(0, "There are %s lines of cpp_quotes batched into several regions that need to be converted manually. Every batch is enclosed by // Begin/End cpp_quote #X.");`,
                  Stats[CodeType.CppQuote]);
    Output.write(Output.Newline, Output.Newline);
  }

  EmitBlocks(Blocks, Output);
}

enum OutContentHeader = q{
version(Windows):

import core.sys.windows.windows;

private mixin template DEFINE_GUID(ComType, alias IIDString)
{
  static if(!is(ComType : IUnknown))
  {
    pragma(msg, "Warning: The type " ~ ComType.stringof ~ " does not derive from IUnknown.");
  }

  // Format of a UUID:
  // [0  1  2  3  4  5  6  7]  8  [9  10 11 12] 13 [14 15 16 17] 18 [19 20] [21 22] 23 [24 25] [26 27] [28 29] [30 31] [32 33] [34 35]
  // [x  x  x  x  x  x  x  x]  -  [x  x  x  x ] -  [x  x  x  x ] -  [x  x ] [x  x ]  - [x  x ] [x  x ] [x  x ] [x  x ] [x  x ] [x  x ]
  static assert(IIDString.length == 36, "Malformed UUID string:\nGot:             %-36s\nExpected format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx".Format(IIDString));
  static assert(IIDString[8]  == '-',   "Malformed UUID string:\nGot:             %-36s\nExpected format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx".Format(IIDString));
  static assert(IIDString[13] == '-',   "Malformed UUID string:\nGot:             %-36s\nExpected format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx".Format(IIDString));
  static assert(IIDString[18] == '-',   "Malformed UUID string:\nGot:             %-36s\nExpected format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx".Format(IIDString));
  static assert(IIDString[23] == '-',   "Malformed UUID string:\nGot:             %-36s\nExpected format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx".Format(IIDString));

  private import std.format : format;

  mixin(format("immutable IID IID_%s "
               "= { 0x%s, 0x%s, 0x%s, [0x%s, 0x%s, 0x%s, 0x%s, 0x%s, 0x%s, 0x%s, 0x%s] };",
               ComType.stringof,
               IIDString[ 0 ..  8], // IID.Data1    <=> [xxxxxxxx]-xxxx-xxxx-xxxx-xxxxxxxxxxxx
               IIDString[ 9 .. 13], // IID.Data2    <=> xxxxxxxx-[xxxx]-xxxx-xxxx-xxxxxxxxxxxx
               IIDString[14 .. 18], // IID.Data3    <=> xxxxxxxx-xxxx-[xxxx]-xxxx-xxxxxxxxxxxx
               IIDString[19 .. 21], // IID.Data4[0] <=> xxxxxxxx-xxxx-xxxx-[xx]xx-xxxxxxxxxxxx
               IIDString[21 .. 23], // IID.Data4[1] <=> xxxxxxxx-xxxx-xxxx-xx[xx]-xxxxxxxxxxxx
               IIDString[24 .. 26], // IID.Data4[2] <=> xxxxxxxx-xxxx-xxxx-xxxx-[xx]xxxxxxxxxx
               IIDString[26 .. 28], // IID.Data4[3] <=> xxxxxxxx-xxxx-xxxx-xxxx-xx[xx]xxxxxxxx
               IIDString[28 .. 30], // IID.Data4[4] <=> xxxxxxxx-xxxx-xxxx-xxxx-xxxx[xx]xxxxxx
               IIDString[30 .. 32], // IID.Data4[5] <=> xxxxxxxx-xxxx-xxxx-xxxx-xxxxxx[xx]xxxx
               IIDString[32 .. 34], // IID.Data4[6] <=> xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxx[xx]xx
               IIDString[34 .. 36], // IID.Data4[7] <=> xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxx[xx]
               ));
}

// Note: Everything below this line is automatically converted and likely to
// contain errors. You should manually check it for validity, if you care
// enough.

};
