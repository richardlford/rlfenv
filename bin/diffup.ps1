#
# diffup.ps1
#
[CmdletBinding()]
Param(
   [Parameter(Mandatory=$true)]
   [string]$item
)
$dir = Get-Location
$upitem = Join-Path ".." $item
$scriptBlock = [ScriptBlock]::Create("cd $dir; windiff $item $upitem")
Start-Job -ScriptBlock $scriptBlock

