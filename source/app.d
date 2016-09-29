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

import midltod;

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
