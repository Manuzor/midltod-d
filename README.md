# midltod-d
Tool written in the D Programming Language to convert Microsoft Interface Definition Language files to D source code.

The tool has only been used and tested on Windows platforms.

_Note: I've written this tool in a hurry to get work done. It's not perfect and the API is basically non-existant. I'm not actively working on it right now but I'd gladly accept pull requests!_

## Building
Simply use (dub)[https://code.dlang.org] to build the stand-alone executable. After the build process is done, you can find it in the `build` subfolder.

Right now, this tool compiles on both Windows and Linux.

## Usage
Use `midltod.exe` from the commandline like this:

```
midltod.exe Path\To\microsoft.idl Output\microsoft.d
```

The given paths are relative to the current working dir. The first argument is the MS IDL file to be converted and the second argument is the destination file.

In theory, you can also use `midltod.d` as a library.
