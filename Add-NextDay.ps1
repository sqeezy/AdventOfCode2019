$lastProjectPath = gci .\src "Day*" | Select -ExpandProperty FullName | Sort-Object -Descending | Select -First 1
$numDay = [regex]::matches($lastProjectPath, ".*Day([0-9]+)").Groups[-1].Value
$next = "{0:d2}" -f ([int]$numDay + 1)
$newProject = $lastProjectPath -replace "Day([0-9]+)", "Day$next"

dotnet new console -lang f# -o $newProject
dotnet sln add $newProject
dotnet add $newProject reference ./src/Shared