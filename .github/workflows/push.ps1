git clone "https://$Env:BENCH_RESULTS_TOKEN@github.com/VSharp-team/VSharp-bench-results.git" $Env:GITHUB_WORKSPACE\results
$REPO_RESULT_PATH = "$Env:GITHUB_WORKSPACE\results\$Env:GITHUB_REF_NAME\$Env:ARTIFACT_NAME"
md "$REPO_RESULT_PATH"
Copy-Item "$Env:ARTIFACT_DIR\*" "$REPO_RESULT_PATH" -Recurse
cd $Env:GITHUB_WORKSPACE\results
git config user.name "VSharp Bot"
git config user.email "bot@vsharp.com"
git add "$REPO_RESULT_PATH"
git commit -am "Bench results: $Env:GITHUB_REPOSITORY@$Env:GITHUB_SHA"
git remote remove origin
git remote add origin "https://$Env:BENCH_RESULTS_TOKEN@github.com/VSharp-team/VSharp-bench-results.git"
do 
{
    $FAILED = $false
    try
    {
        git pull origin main --rebase
        git push --set-upstream origin main
    } 
    catch 
    {       
        $FAILED = $true 
    }

} while ($FAILED)
