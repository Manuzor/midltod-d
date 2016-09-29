import std.stdio;
import std.range;
import std.path;
import std.datetime;

import midltod;

void main(string[] args)
{
  args = args[1 .. $];

  assert(args.length == 2, "Need 2 arguments.");

  Log = new FormattedOutput();
  Log.outFile = stderr;

  auto inFilename  = args[0];
  auto outFilename = args[1];

  char[] inputBuffer = readTextAsUTF8(inFilename);

  inputBuffer = inputBuffer.preprocess();

  int[CodeType.max + 1] stats;
  auto blocks = parse(inputBuffer, stats);

  auto output = new FormattedOutput();
  if(outFilename == "-")
  {
    output.outFile = stdout;
  }
  else
  {
    output.outFile.open(outFilename, "w");
  }

  Log.writeln('='.repeat(72));

  output.write("// Original file name: ", inFilename.baseName, output.newline);
  output.write("// Conversion date: ", Clock.currTime, output.newline);
  output.write(outContentHeader);

  if(stats[CodeType.cppQuote])
  {
    output.writef(`static assert(0, "There are %s lines of cpp_quotes batched into several regions that need to ` ~
                  `be converted manually. Every batch is enclosed by // Begin/End cpp_quote #X.");`,
                  stats[CodeType.cppQuote]);
    output.write(output.newline, output.newline);
  }

  emitBlocks(blocks, output);
}
