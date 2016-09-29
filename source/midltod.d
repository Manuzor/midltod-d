import std.stdio;
import std.algorithm;
import std.conv;
import std.format;
import std.range;

import std.string : strip, stripRight, stripLeft, toUpper;
import std.ascii : isDigit;
import std.uni : isWhite, isLower;
import std.file : read;
import std.path : stripExtension;

char[] view(char[] str, size_t numToView)
{
  return str[0 .. min($, numToView)];
}

enum CodeType
{
  invalid,

  import_,
  cppQuote,
  constant,
  enum_,
  aggregate,
  interface_,
  function_,
  alias_,
}

struct BlockData
{
  CodeType type;
  union
  {
    ImportData import_;
    CppQuoteData cppQuote;
    ConstantData constant;
    EnumData enum_;
    AggregateData aggregate;
    InterfaceData interface_;
    FunctionData function_;
    AliasData alias_;
  }

  string toString() const
  {
    final switch(type)
    {
      case CodeType.import_:    return "BlockData(%s)".format(import_);
      case CodeType.cppQuote:   return "BlockData(%s)".format(cppQuote);
      case CodeType.constant:   return "BlockData(%s)".format(constant);
      case CodeType.enum_:      return "BlockData(%s)".format(enum_);
      case CodeType.aggregate:  return "BlockData(%s)".format(aggregate);
      case CodeType.interface_: return "BlockData(%s)".format(interface_);
      case CodeType.function_:  return "BlockData(%s)".format(function_);
      case CodeType.alias_:     return "BlockData(%s)".format(alias_);
      case CodeType.invalid: assert(0);
    }
  }
}

struct ImportData
{
  char[] filename;
}

struct CppQuoteData
{
  char[] content;
}

struct ConstantData
{
  char[] type;
  char[] name;
  char[] value;
}

struct EnumData
{
  static struct Entry
  {
    char[] key;
    char[] value;
  }

  char[] name;
  Entry[] entries;
}

struct Declaration
{
  AttributeData[] attributes;
  char[] type;
  Declaration[] body_; // Only valid if type == "struct" || type == "union"
  char[] name;
  char[][] arrayCounts; // e.g. int Foo[MAX_STUFF][3];
}

struct AggregateData
{
  enum Type
  {
    struct_,
    union_,
  }

  Type aggregateType;
  char[] name;
  Declaration[] members;
}

struct InterfaceData
{
  AttributeData[] attributes;
  char[] name;
  char[] parentName;
  FunctionData[] functions;
}

struct FunctionData
{
  char[] returnType;
  char[] name;
  Declaration[] params;
}

struct AliasData
{
  char[] newName;
  char[] oldName;
}

enum AttributeType
{
  invalid,

  // Mostly used for interfaces:
  object,
  uuid,
  local,
  pointerDefault,

  // Mostly used in functions:
  in_,
  out_,
  retval,
  sizeIs,
  annotation,
}

struct AttributeData
{
  AttributeType type;
  char[] value;

  string toString() const
  {
    string result;
    final switch(type)
    {
      case AttributeType.object:          result = "object";          break;
      case AttributeType.uuid:            result = "uuid";            break;
      case AttributeType.local:           result = "local";           break;
      case AttributeType.pointerDefault:  result = "pointer_default"; break;
      case AttributeType.in_:             result = "in";              break;
      case AttributeType.out_:            result = "out";             break;
      case AttributeType.retval:          result = "retval";          break;
      case AttributeType.sizeIs:          result = "size_is";         break;
      case AttributeType.annotation:      result = "annotation";      break;

      case AttributeType.invalid: assert(0);
    }

    if(value.length)
    {
      result ~= `("` ~ value ~ `")`;
    }

    return result;
  }
}

class FormattedOutput
{
  int indentationLevel;
  string newline = "\n";

  File outFile;

  @property auto indentation() { return ' '.repeat(indentationLevel); }
  void indent(int amount = 2)
  {
    indentationLevel += amount;
  }

  void outdent(int amount = 2)
  {
    indentationLevel = max(0, indentationLevel - amount);
  }

  void writeIndentation()
  {
    this.write(indentation);
  }

  alias outFile this;
}

struct Log
{
  static FormattedOutput file;

  alias file this;
}

void parseImport(ref char[] source, ref BlockData[] blocks)
{
  Log.writeln(Log.indentation, "=== Parsing Import ===");

  assert(source.startsWith("import"), source.view(100));
  source.popFrontN("import".length);
  assert(source.front.isWhite, source.view(100));

  source.skipWhiteSpace();

  assert(source.front == '"');
  source.popFront();

  ImportData result;
  result.filename = source.parseEscapableString('"');
  Log.writeln(Log.indentation, "Filename: ", result.filename);

  source.skipWhiteSpace();

  assert(source.front == ';');

  source.popFront();

  auto block = BlockData(CodeType.import_);
  block.import_ = result;
  blocks ~= block;
}

void parseCppQuote(ref char[] source, ref BlockData[] blocks)
{
  Log.writeln(Log.indentation, "=== Parsing CppQuote ===");

  CppQuoteData result;

  assert(source.startsWith("cpp_quote"));
  source.popFrontN("cpp_quote".length);

  source.skipWhiteSpace();

  assert(source.front == '(');
  source.popFront();

  source.skipWhiteSpace();

  assert(source.front == '"');
  source.popFront();

  result.content = source.parseEscapableString('"').strip();
  Log.writeln(Log.indentation, "Content: ", result.content);

  source.skipWhiteSpace();

  assert(source.front == ')', source.view(100));
  source.popFront();

  auto block = BlockData(CodeType.cppQuote);
  block.cppQuote = result;
  blocks ~= block;
}

void parseConstant(ref char[] source, ref BlockData[] blocks)
{
  Log.writeln(Log.indentation, "=== Parsing Constant ===");

  ConstantData result;

  // Skip 'const' or '#define'
  const firstToken = source.fastForwardUntil!isWhite;
  assert(firstToken == "const" || firstToken == "#define", firstToken);

  if(firstToken == "const")
  {
    source.skipWhiteSpace();

    result.type = source.fastForwardUntil!isWhite;
    Log.writeln(Log.indentation, "Type: ", result.type);

    result.name = source.fastForwardUntil!(ch => ch == '=').strip;
    Log.writeln(Log.indentation, "Name: ", result.name);

    result.value = source.fastForwardUntil!(ch => ch == ';').strip;
    Log.writeln(Log.indentation, "Value: ", result.value);
  }
  else
  {
    source.skipWhiteSpace();

    result.name = source.fastForwardUntil!isWhite.strip;
    Log.writeln(Log.indentation, "Name: ", result.name);

    result.value = source.fastForwardUntil!(ch => ch == '\n').strip;
    Log.writeln(Log.indentation, "Value: ", result.value);
  }

  auto block = BlockData(CodeType.constant);
  block.constant = result;
  blocks ~= block;
}

void parseEnum(ref char[] source, ref BlockData[] blocks)
{
  Log.writeln(Log.indentation, "=== Parsing Enum ===");

  EnumData result;

  // Skip 'typedef'
  const typedefToken = source.fastForwardUntil!isWhite;
  assert(typedefToken == "typedef");

  source.skipWhiteSpace();

  // Skip 'enum'
  const EnumToken = source.fastForwardUntil!isWhite;
  assert(EnumToken == "enum");

  // Read everything between 'enum' and '{'
  auto TagName = source.fastForwardUntil!(ch => ch == '{').strip;
  Log.writeln(Log.indentation, "Tag Name: ", TagName);

  auto body_ = source.fastForwardUntil!(ch => ch == '}');

  result.name = source.fastForwardUntil!(ch => ch == ';').strip;
  Log.writeln(Log.indentation, "Name: ", result.name);

  Log.indent();

  while(true)
  {
    body_.skipWhiteSpace();

    if(body_.empty) break;

    result.entries.length++;
    auto entry = &result.entries.back;
    scope(failure) Log.writeln(Log.indentation, "Entry: ", *entry);

    auto EntryCode = body_.fastForwardUntil!(ch => ch == ',');
    EntryCode.skipWhiteSpace();

    entry.key = EntryCode.fastForwardUntil!(ch => ch == '=').strip;
    Log.writeln(Log.indentation, "key: ", entry.key);

    EntryCode.skipWhiteSpace();

    entry.value = EntryCode.strip; // The rest of the code.
    Log.writeln(Log.indentation, "Value: ", entry.value);
  }

  Log.outdent();

  auto block = BlockData(CodeType.enum_);
  block.enum_ = result;
  blocks ~= block;
}

/// params:
///   fieldDelimiter = Usually ';' for structs and unions or ',' for function parameters.
void parseDeclaration(ref char[] body_, ref Declaration decl, dchar fieldDelimiter)
{
  Log.writeln(Log.indentation, "=== Parsing Declaration ===");
  Log.indent();
  scope(exit) Log.outdent();

  tryParseAttributes(body_, decl.attributes);

  body_.skipWhiteSpace();

  auto tempBody = body_;
  const prefix = fastForwardUntil!isWhite(tempBody);
  if(prefix == "struct" || prefix == "union")
  {
    Log.writeln(Log.indentation, "Declaration is an inner aggregate type.");

    decl.type = body_.fastForwardUntil!isWhite;
    assert(decl.type == prefix);

    auto InnerTagName = body_.fastForwardUntil!(ch => ch == '{').strip;
    if(InnerTagName.length)
    {
      Log.writeln(Log.indentation, "Tag Name: ", InnerTagName);
    }
    else
    {
      Log.writeln(Log.indentation, "No tag name found.");
    }

    auto InnerBody = body_.parseNestedString('{', '}');
    auto InnerName = body_.fastForwardUntil!(ch => ch == fieldDelimiter).strip;

    Log.writeln(Log.indentation, "Name: ", InnerName);

    while(true)
    {
      InnerBody.skipWhiteSpace();

      if(InnerBody.empty) break;

      Declaration InnerDecl;
      scope(failure) Log.writeln(Log.indentation, "Inner Declaration: ", InnerDecl);

      parseDeclaration(InnerBody, InnerDecl, ';');
      decl.body_ ~= InnerDecl;
    }

    Log.outdent();
  }
  else
  {
    Log.writeln(Log.indentation, "Declaration is a regular field.");

    auto fieldCode = body_.fastForwardUntil!(ch => ch == fieldDelimiter);

    fieldCode = fieldCode.strip();

    if(fieldCode == "void")
    {
      decl.type = fieldCode;
      return;
    }

    while(fieldCode.back == ']')
    {
      fieldCode.popBack();
      auto ArrayCountLength = fieldCode.retro.countUntil('[');
      decl.arrayCounts ~= fieldCode[$ - ArrayCountLength .. $].strip;
      fieldCode.popBackN(ArrayCountLength + 1);
    }

    auto slackUntilDelimiter = fieldCode.retro.countUntil!(ch => ch.isWhite || ch == '*');

    decl.type = fieldCode[0                       .. $ - slackUntilDelimiter].strip;
    decl.name = fieldCode[$ - slackUntilDelimiter .. $                      ].strip;

    Log.writeln(Log.indentation, "Type: ", decl.type);
    Log.writeln(Log.indentation, "Name: ", decl.name);
  }
}

void parseAggregate(ref char[] source, ref BlockData[] blocks)
{
  Log.writeln(Log.indentation, "=== Parsing Aggregate ===");

  AggregateData result;

  const typedefToken = source.fastForwardUntil!isWhite;
  assert(typedefToken == "typedef");

  source.skipWhiteSpace();

  const firstToken = source.fastForwardUntil!isWhite;

  if(firstToken == "struct")
  {
    result.aggregateType = AggregateData.Type.struct_;
  }
  else if(firstToken == "union")
  {
    result.aggregateType = AggregateData.Type.union_;
  }
  else assert(0);

  // Read everything between 'struct' and '{'
  auto TagName = source.fastForwardUntil!(ch => ch == '{').strip;
  if(TagName.length)
  {
    Log.writeln(Log.indentation, "Tag Name: ", TagName);
  }
  else
  {
    Log.writeln(Log.indentation, "No tag name found.");
  }

  auto body_ = source.parseNestedString('{', '}');

  result.name = source.fastForwardUntil!(ch => ch == ';').strip;
  assert(result.name.length);
  Log.writeln(Log.indentation, "Name: ", result.name);

  Log.indent();

  while(true)
  {
    body_.skipWhiteSpace();

    if(body_.empty) break;

    Declaration member;
    scope(failure) Log.writeln(Log.indentation, "Member: ", member);

    parseDeclaration(body_, member, ';');

    result.members ~= member;
  }

  Log.outdent();

  auto block = BlockData(CodeType.aggregate);
  block.aggregate = result;
  blocks ~= block;

  Log.writeln(Log.indentation, "New Block: ", blocks.back);
}

char[] parseNestedString(ref char[] source, char frontChar, char backChar,
  Flag!"CanExhaustSource" canExhaustSource = Yes.CanExhaustSource)
{
  auto parsedSource = source;
  int numBackCharsNeeded = 1;

  do
  {
    char delimiter;
    parsedSource.fastForwardUntil!(ch => ch == frontChar || ch == backChar)(&delimiter);

    if(delimiter == frontChar)
    {
      numBackCharsNeeded++;
    }
    else if(delimiter == backChar)
    {
      numBackCharsNeeded--;
    }
    else
    {
      assert(canExhaustSource, "Source got exhausted before the matching closing delimiter was found.");
      break;
    }
  } while(numBackCharsNeeded);

  auto result = source[0 .. $ - min(source.length, parsedSource.length + 1)];
  source = parsedSource;
  return result;
}

char[] parseEscapableString(ref char[] source, char delimiter)
{
  enum char escape = '\\';

  auto target = source;

  while(true)
  {
    target = target.find(escape, delimiter)[0];

    if(target.empty || target.front == delimiter) break;

    if(target.front == escape)
    {
      target.popFront();
      assert(target.length, target.view(100));
      // Skip the escaped character.
      target.popFront();
    }
  }

  auto result = source[0 .. $ - target.length];
  if(target.length) target.popFront();
  source = target;
  return result;
}

void tryParseAttributes(ref char[] source, ref AttributeData[] attributes)
{
  if(source.front != '[') return;
  source.popFront();

  auto attributeSource = source.parseNestedString('[', ']');

  while(true)
  {
    attributeSource.skipWhiteSpace();

    if(attributeSource.empty) break;

    AttributeType nameToAttributeType(char[] name)
    {
      switch(name)
      {
        case "object":          return AttributeType.object;
        case "uuid":            return AttributeType.uuid;
        case "local":           return AttributeType.local;
        case "pointer_default": return AttributeType.pointerDefault;
        case "in":              return AttributeType.in_;
        case "out":             return AttributeType.out_;
        case "retval":          return AttributeType.retval;
        case "size_is":         return AttributeType.sizeIs;
        case "annotation":      return AttributeType.annotation;
        default: assert(0, "Unknown IDL attribute: " ~ name);
      }
    }

    char delimiter;
    auto name = attributeSource.fastForwardUntil!(ch => ch == ',' || ch == '(')(&delimiter).stripRight();

    AttributeData attribute;
    attribute.type = nameToAttributeType(name);

    if(delimiter == '(')
    {
      attribute.value = attributeSource.parseNestedString('(', ')').strip;
      if(attribute.value.front == '"')
      {
        assert(attribute.value.back == '"');
        attribute.value.popFront();
        attribute.value.popBack();
      }

      attributeSource.skipWhiteSpace();
      if(attributeSource.length && attributeSource.front == ',') attributeSource.popFront();
    }

    attributes ~= attribute;
  }
}

void parseInterface(ref char[] source, ref BlockData[] blocks)
{
  Log.writeln(Log.indentation, "=== Parsing Interface ===");

  InterfaceData result;

  tryParseAttributes(source, result.attributes);

  source.skipWhiteSpace();

  assert(source.startsWith("interface"));
  source.fastForwardUntil!isWhite;

  source.skipWhiteSpace();

  char delimiter;
  result.name = source.fastForwardUntil!(ch => ch == ';' || ch == ':' || ch == '{')(&delimiter).strip();
  Log.writeln(Log.indentation, "Name: ", result.name);

  source.skipWhiteSpace();

  if(delimiter == ';')
  {
    Log.writeln(Log.indentation, "Ignoring forward declaration");
    return;
  }

  if(delimiter == ':')
  {
    result.parentName = source.fastForwardUntil!(ch => ch == '{')(&delimiter).strip();
    Log.writeln(Log.indentation, "ParentName: ", result.parentName);
  }

  assert(delimiter == '{');

  auto body_ = source.fastForwardUntil!(ch => ch == '}');

  // d3d11.idl contains an interface without a closing ';', so we can't assert
  // for its existance. Not sure if this is the only case.
  while(source.front == ';') source.popFront();

  Log.writeln(Log.indentation, "Body: ", body_);

  Log.indent();

  while(true)
  {
    body_.skipWhiteSpace();

    if(body_.empty) break;

    BlockData[] functionBlocks;
    parseFunction(body_, functionBlocks);
    foreach(ref block ; functionBlocks)
    {
      assert(block.type == CodeType.function_);
      result.functions ~= block.function_;
    }
  }

  Log.outdent();

  blocks ~= BlockData(CodeType.interface_);
  blocks[$-1].interface_ = result;
}

void parseFunction(ref char[] source, ref BlockData[] blocks)
{
  Log.writeln(Log.indentation, "=== Parsing Function ===");

  FunctionData result;

  result.returnType = source.fastForwardUntil!isWhite;
  Log.writeln(Log.indentation, "Return Type: ", result.returnType);

  source.skipWhiteSpace();

  result.name = source.fastForwardUntil!(ch => ch == '(').stripRight();
  Log.writeln(Log.indentation, "Name: ", result.name);

  auto ParamSource = source.fastForwardUntil!(ch => ch == ';');
  Log.writeln(Log.indentation, "Param Source: ", ParamSource);
  assert(ParamSource.back == ')');
  ParamSource.popBack();

  Log.indent();

  while(true)
  {
    ParamSource.skipWhiteSpace();

    if(ParamSource.empty) break;

    Declaration param;
    parseDeclaration(ParamSource, param, ',');
    if(param.type != "void") result.params ~= param;
  }

  Log.outdent();

  blocks ~= BlockData(CodeType.function_);
  blocks[$-1].function_ = result;
}

void parseAlias(ref char[] source, ref BlockData[] blocks)
{
  Log.writeln(Log.indentation, "=== Parsing Alias ===");

  auto result = BlockData(CodeType.alias_);
  result.alias_ = AliasData.init;

  auto body_ = source.fastForwardUntil!(ch => ch == ';');

  const typedefToken = fastForwardUntil!isWhite(body_);
  assert(typedefToken == "typedef");

  result.alias_.oldName = body_.fastForwardUntil!isWhite;
  Log.writeln(Log.indentation, "Old name: ", result.alias_.oldName);

  result.alias_.newName = body_.strip; // The rest.
  Log.writeln(Log.indentation, "New name: ", result.alias_.newName);

  blocks ~= result;
}

bool isDelimiter(CharType)(CharType ch)
{
  return ch.isWhite ||
         ch == '(' ||
         ch == ')' ||
         ch == '[' ||
         ch == ']' ||
         ch == '{' ||
         ch == '}' ||
         ch == '=' ||
         ch == ',' ||
         ch == ';';
}

/// Advances source until pred(a) is true and returns the part of source as a slice that has been skipped.
/// The matching char is written to lastCharOutPtr and not included in the return value or the advanced source.
char[] fastForwardUntil(alias pred)(ref char[] source, char* lastCharOutPtr = null)
{
  auto newSource = source.find!pred();

  if(newSource.empty)
  {
    swap(newSource, source);
    return newSource;
  }

  auto result = source[0 .. $ - newSource.length];
  if(newSource.length)
  {
    if(lastCharOutPtr) *lastCharOutPtr = newSource[0];
    newSource.popFront();
  }

  source = newSource;
  return result;
}

// Skips white space and comments.
void skipWhiteSpace(ref char[] source)
{
  source = source.stripLeft();
}

enum contextCharCountThreshold = 60;

BlockData[] parse(ref char[] source, int[] stats)
{
  typeof(return) result;

  while(true)
  {
    source.skipWhiteSpace();

    if(source.empty) break;

    Log.writefln("Looking at: %s...", source.view(contextCharCountThreshold));
    Log.indent();

    if(source.startsWith("#pragma"))
    {
      source.fastForwardUntil!(ch => ch == '\n');
    }
    else if(source.startsWith("import"))
    {
      parseImport(source, result);
      stats[CodeType.import_]++;
    }
    else if(source.startsWith("const") || source.startsWith("#define"))
    {
      parseConstant(source, result);
      stats[CodeType.constant]++;
    }
    else if(source.startsWith("cpp_quote"))
    {
      parseCppQuote(source, result);
      stats[CodeType.cppQuote]++;
    }
    else if(source.startsWith("typedef"))
    {
      auto sourceCopy = source;
      sourceCopy.fastForwardUntil!isWhite;
      sourceCopy.skipWhiteSpace();

      if(sourceCopy.startsWith("enum"))
      {
        parseEnum(source, result);
        stats[CodeType.enum_]++;
      }
      else if(sourceCopy.startsWith("struct") || sourceCopy.startsWith("union"))
      {
        parseAggregate(source, result);
        stats[CodeType.aggregate]++;
      }
      else
      {
        parseAlias(source, result);
        stats[CodeType.alias_]++;
      }
    }
    else if(source.startsWith("[") || source.startsWith("interface"))
    {
      parseInterface(source, result);
      stats[CodeType.interface_]++;
    }
    else
    {
      assert(0, "Unknown token at: " ~ source[0 .. min(contextCharCountThreshold, source.length)]);
    }

    Log.outdent();
    Log.writeln('-'.repeat(10));
  }

  return result;
}

void emitConstant(ref ConstantData constant, FormattedOutput output)
{
  output.writef("%senum ", output.indentation);
  if(constant.type) output.write(constant.type, " ");
  auto value = constant.value;
  while(value.back.toUpper == 'L') value.popBack();
  if(value.back.toUpper == 'U') value.popBack();
  output.writef("%s = %s;%s", constant.name, value, output.newline);
}

void emitEnum(ref EnumData enum_, FormattedOutput output)
{
  const baseTypeName = "int";
  output.write(output.indentation, "alias ", enum_.name, " = ", baseTypeName, ';', output.newline);
  output.write(output.indentation, "enum : ", enum_.name, output.newline,
               output.indentation, "{", output.newline);

  output.indent();

  auto entries = enum_.entries.dup;

  size_t maxLen;
  foreach(ref entry; entries)
  {
    maxLen = max(maxLen, entry.key.length);
  }

  foreach(ref entry; entries)
  {
    output.write(output.indentation, entry.key);
    if(entry.value.length)
    {
      output.writef("%s = %s", ' '.repeat(maxLen - entry.key.length), entry.value);
    }
    output.write(",", output.newline);
  }
  output.outdent();
  output.write(output.indentation, "}", output.newline);
}

void emitDeclaration(ref Declaration decl, FormattedOutput output, Flag!"IsFunctionParam" isFunctionParam)
{
  // Add an extra newline here in attempt to make the interface look nicer for
  // inner aggregate types.
  if(decl.body_) output.write(output.newline);

  if(decl.attributes)
  {
    output.writef("%s// %([%s]%)]%s", output.indentation, decl.attributes, output.newline);
  }

  if(decl.body_)
  {
    assert(!isFunctionParam, "Function parameters in D can't be inline aggregate type declarations (probably).");
    assert(decl.type == "struct" || decl.type == "union", decl.type);

    output.write(output.indentation);

    // Inner structs should be static.
    if(decl.type == "struct") output.write("static ");

    output.write(decl.type, " ", decl.name, output.newline,
                 output.indentation, '{', output.newline);

    output.indent();

    foreach(ref member; decl.body_)
    {
      emitDeclaration(member, output, No.IsFunctionParam);
    }

    output.outdent();

    output.write(output.indentation, '}');
  }
  else
  {
    auto type = decl.type;
    const isInterfaceType = type.front == 'I' && type.canFind!(ch => ch.isLower());
    const numPointers = type.count('*');
    const isConst = type.canFind(" const", "const ");

    char[] typeBaseName;
    do
    {
      typeBaseName = type.fastForwardUntil!(ch => ch.isWhite || ch == '*');
      type.skipWhiteSpace();
    } while(typeBaseName == "const");

    if(isInterfaceType) assert(numPointers > 0);

    output.write(output.indentation);

    if(isConst) output.write(isFunctionParam ? "in " : "const ");

    auto pointers = '*'.repeat(isInterfaceType ? numPointers - 1 : numPointers);
    output.write(typeBaseName, pointers);

    foreach(Count; decl.arrayCounts)
    {
      output.writef("[%s]", Count);
    }

    output.write(" ", decl.name, isFunctionParam ? ',' : ';');
  }

  output.write(output.newline);
}

void emitAggregate(ref AggregateData aggregate, FormattedOutput output)
{
  Log.writeln(Log.indentation, "Emitting aggregate: ", aggregate);

  output.write(output.indentation);

  final switch(aggregate.aggregateType)
  {
    case AggregateData.Type.struct_: output.write("struct "); break;
    case AggregateData.Type.union_:  output.write("union ");  break;
  }

  output.write(aggregate.name, output.newline,
               output.indentation, "{", output.newline);

  output.indent();

  foreach(ref member; aggregate.members[])
  {
    emitDeclaration(member, output, No.IsFunctionParam);
  }

  output.outdent();

  output.write(output.indentation, "}", output.newline);
}

void emitInterface(ref InterfaceData interface_, FormattedOutput output)
{
  char[] uuid = interface_.attributes.find!(attr => attr.type == AttributeType.uuid).front.value;
  output.write(output.indentation, "mixin DEFINE_GUID!(", interface_.name, `, "`, uuid, `");`, output.newline);

  if(interface_.attributes)
  {
    output.writef("%s// %([%s]%)]%s", output.indentation, interface_.attributes, output.newline);
  }

  if(interface_.parentName)
  {
    output.write(output.indentation, "interface ", interface_.name, " : ", interface_.parentName, output.newline);
  }
  else
  {
    output.write(output.indentation, "interface ", interface_.name, output.newline);
  }

  output.write(output.indentation, "{", output.newline,
               output.indentation, "extern(Windows):", output.newline,
               output.newline);

  output.indent();

  foreach(ref function_; interface_.functions)
  {
    emitFunction(function_, output, No.AddExternWindows);
    output.write(output.newline);
  }

  output.outdent();

  output.write(output.indentation, "}", output.newline);
}

void emitFunction(ref FunctionData function_, FormattedOutput output,
  Flag!"AddExternWindows" addExternWindows = Yes.AddExternWindows)
{
  scope(failure) Log.writeln("Failure emitting function: ", function_);

  output.write(output.indentation);
  if(addExternWindows) output.write("extern(Windows) ");
  output.write(function_.returnType, " ", function_.name, "(");

  if(function_.params)
  {
    output.write(output.newline);

    output.indent();

    foreach(ref param; function_.params)
    {
      emitDeclaration(param, output, Yes.IsFunctionParam);
    }

    output.outdent();
    output.write(output.indentation);
  }

  output.write(");", output.newline);
}

void emitAlias(ref AliasData alias_, FormattedOutput output)
{
  output.write(output.indentation);
  output.writef("alias %s = %s;%s", alias_.newName, alias_.oldName, output.newline);
}

void emitImport(ref ImportData import_, FormattedOutput output)
{
  output.write(output.indentation, "import ", import_.filename.stripExtension, ';', output.newline);
}

void emitCppQuote(ref CppQuoteData cppQuote, FormattedOutput output)
{
  output.write(output.indentation, cppQuote.content, output.newline);
}

void emitD3DCOLORVALUE(ref AliasData alias_, FormattedOutput output)
{
  assert(alias_.oldName == "D3DCOLORVALUE");
  output.write(output.indentation, "struct ", alias_.newName, output.newline,
               output.indentation, "{", output.newline);

  output.indent();

  output.write(output.indentation, "float r;", output.newline,
               output.indentation, "float g;", output.newline,
               output.indentation, "float b;", output.newline,
               output.indentation, "float a;", output.newline);

  output.outdent();

  output.write(output.indentation, "}", output.newline);
}

void emitBlocks(BlockData[] blocks, FormattedOutput output, int cppQuoteBatchId = 0)
{
  while(blocks.length)
  {
    auto block = &blocks.front;
    blocks.popFront();

    output.write(output.newline);

    final switch(block.type)
    {
      case CodeType.import_:
      {
        emitImport(block.import_, output);
        break;
      }
      case CodeType.constant:
      {
        emitConstant(block.constant, output);
        break;
      }
      case CodeType.enum_:
      {
        emitEnum(block.enum_, output);
        break;
      }
      case CodeType.aggregate:
      {
        emitAggregate(block.aggregate, output);
        break;
      }
      case CodeType.interface_:
      {
        emitInterface(block.interface_, output);
        break;
      }
      case CodeType.function_:
      {
        emitFunction(block.function_, output);
        break;
      }
      case CodeType.alias_:
      {
        if(block.alias_.oldName == "D3DCOLORVALUE")
        {
          emitD3DCOLORVALUE(block.alias_, output);
        }
        else
        {
          emitAlias(block.alias_, output);
        }

        break;
      }
      case CodeType.cppQuote:
      {
        // cpp_quotes are emitted in batches.
        const numConsecutiveCppQuotes = blocks.countUntil!(block => block.type != CodeType.cppQuote);
        scope(exit) cppQuoteBatchId += numConsecutiveCppQuotes;

        output.write(output.indentation, "// Begin cpp_quote #", cppQuoteBatchId);
        if(numConsecutiveCppQuotes > 1) output.write("-", cppQuoteBatchId + numConsecutiveCppQuotes - 1);
        output.write(output.newline);

        while(true)
        {
          emitCppQuote(block.cppQuote, output);
          if(blocks.empty) break;
          block = &blocks.front;
          if(block.type != CodeType.cppQuote) break;
          blocks.popFront();
        }

        output.write(output.indentation, "// End cpp_quote #", cppQuoteBatchId);
        if(numConsecutiveCppQuotes > 1) output.write("-", cppQuoteBatchId + numConsecutiveCppQuotes - 1);
        output.write(output.newline);

        break;
      }
      case CodeType.invalid: assert(0, block.type.to!string);
    }
  }
}

char[] readTextAsUTF8(in string filename)
{
  return filename.read().to!(char[]);
}

// Replaces all comments with spaces.
char[] preprocess(char[] source)
{
  auto search = source;
  while(search.length)
  {
    search = search.find("//", "/*", "cpp_quote")[0];

    if(search.startsWith("//"))
    {
      auto garbage = search.fastForwardUntil!(ch => ch == '\n');
      garbage[] = ' ';
    }
    else if(search.startsWith("/*"))
    {
      auto continueHere = search.find("*/");
      if(continueHere.length) continueHere.popFrontN(2);
      auto garbage = search[0 .. $ - continueHere.length];
      search = continueHere;
      garbage[] = ' ';
    }
    else if(search.startsWith("cpp_quote"))
    {
      search.popFrontN("cpp_quote".length);
      search.skipWhiteSpace();
      assert(search.front == '(');
      search.popFront();
      search.skipWhiteSpace();
      assert(search.front == '"');
      search.popFront();
      search.parseEscapableString('"');
    }
  }

  return source;
}

enum outContentHeader = q{
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
