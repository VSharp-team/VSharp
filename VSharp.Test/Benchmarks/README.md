## How to run benchmarks

- Clone or download the following benchmarks repository: https://github.com/VSharp-team/VSharp-bench

- Create the a file named `*.runsettings` with the following structure:

```xml
<?xml version="1.0" encoding="utf-8"?>
<RunSettings>
  <TestRunParameters>
    <Parameter name="BenchmarkDllsPath" value="Path to the root directory of benchmarks repository" />
  </TestRunParameters>
</RunSettings>
```

- Specify path to the file in IDE settings or from console (see https://learn.microsoft.com/en-us/visualstudio/test/configure-unit-tests-by-using-a-dot-runsettings-file?view=vs-2022)