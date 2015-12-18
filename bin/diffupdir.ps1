#
# diffupdir.ps1
#
[CmdletBinding()]
Param(
   [Parameter(Mandatory=$true)]
   [string]$reldir
)
$dir1 = "C:\ProjectK\src\QA\CLR\testsrc"
$path1 = Join-Path $dir1 $reldir
$path2 = Join-Path $path1 ".."
$scriptBlock = [ScriptBlock]::Create("windiff $path1 $path2")
Start-Job -ScriptBlock $scriptBlock
