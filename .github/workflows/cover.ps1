$DLL_PATH="$Env:GITHUB_WORKSPACE/benchmarks/$Env:SUITE/$Env:DLL.dll"
$DATE=Get-Date -Format "yyyy-MM-dd_HH-mm"
$OUTPUT_DIR_NAME="$DATE-$Env:SUITE-$Env:DLL-$Env:NAMESPACE$Env:CLASS$Env:METHOD"
$OUTPUT_PATH="$Env:GITHUB_WORKSPACE/outputs/$OUTPUT_DIR_NAME"
$TEMP_DIR="$Env:GITHUB_WORKSPACE/tmp"

md $OUTPUT_PATH

foreach ($s in $Env:SEARCHERS.Split(" "))
{
    $SEARCHER_PATH="$OUTPUT_PATH/$s"
    md $SEARCHER_PATH

    foreach ($t in $Env:TIMEOUTS.Split(" "))
    {
        md $TEMP_DIR
        
        $TIMEOUT_PATH="$SEARCHER_PATH/$t"
        md $TIMEOUT_PATH  

        cd "$Env:GITHUB_WORKSPACE/runner"

        if ($Env:NAMESPACE) 
        {
            dotnet VSharp.Runner.dll --namespace "$Env:NAMESPACE" "$DLL_PATH" -t "$t" -o "$TEMP_DIR" --strat "$s" -v StatisticsCollection
        } 
        elseif ($Env:METHOD) 
        {
            dotnet VSharp.Runner.dll --method "$Env:METHOD" "$DLL_PATH" -t "$t" -o "$TEMP_DIR" --strat "$s" -v StatisticsCollection
        } 
        elseif ($Env:CLASS) 
        {
            dotnet VSharp.Runner.dll --public-methods-of-class "$Env:CLASS" "$DLL_PATH" -t "$t" -o "$TEMP_DIR" --strat "$s" -v StatisticsCollection
        } 
        else 
        {
            dotnet VSharp.Runner.dll --all-public-methods "$DLL_PATH" -t "$t" -o "$TEMP_DIR" --strat "$s" -v StatisticsCollection
        } 

        cd "$Env:GITHUB_WORKSPACE/test_runner"

        dotnet-dotcover exec VSharp.TestRunner.dll "$TEMP_DIR/VSharp.tests.0" --dcFilters="-:module=Microsoft.*;-:module=FSharp.*;-:class=VSharp.*;-:module=VSharp.Utils" --dcReportType="JSON|HTML" --dcOutput="$TIMEOUT_PATH/coverage.json;$TIMEOUT_PATH/coverage.html"

        Copy-Item "$TEMP_DIR\VSharp.tests.0\reports\continuous.csv" -Destination "$TIMEOUT_PATH"
        Remove-Item $TEMP_DIR -Recurse -Force
    }
}

Add-Content -Path $Env:GITHUB_OUTPUT -Value "artifact_dir=$OUTPUT_PATH"
Add-Content -Path $Env:GITHUB_OUTPUT -Value "artifact_name=$OUTPUT_DIR_NAME"

Exit 0
