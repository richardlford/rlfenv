﻿# -------------------------------------------------------------------------
#
# This script provides LLILC branch management facilities.
#
# -------------------------------------------------------------------------

<#
.SYNOPSIS
    Support branch management for LLILC team.

.DESCRIPTION
    This script sets up an environment and provides functions for
    performing branch management tasks.

    We support two configurations for LLVM and related projects.

    - Unified configuration has Clang and LLILC repositories
      contained within the LLVM tree. Suffix to denote this is "uni"
    - Separate configuration has Clang and LLILC repositories
      outside the LLVM tree. Suffix to denote this is "sep". 

    We test both configurations to check that they work.

.EXAMPLE
    ManageLLILCBranch.ps1
#>

[CmdletBinding()]
Param(
   [Parameter(Mandatory=$true)]
   [string]$GitUserName,
   [string]$BaseWorkDir = ""
)

$Global:mgll = @{}
$Global:mgll.GitUserName = $GitUserName
$Global:mgll.BaseWorkDir = $BaseWorkDir

# -------------------------------------------------------------------------
#
# Capture environment variables after a batch file runs.
# $File is the batch file to run.
# $target is the arguments to pass to the batch file.
# 
# -------------------------------------------------------------------------

function Global:MgEnvCapture([string]$File, [string]$target)
{
  # This is complicated two things:
  # 1. When powershell invokes a cmd or bat file, any environment variables
  # changes made by that file are lost when the file finishes executing.
  # So one must resort to trickery to capture the environment variables.
  # 2. by the fact that the path to the batch
  # file that set these has blanks in it. 
  #
  # To work around the limitations of cmd we create a temporary batch file and
  # execute it to capture the new environment variables.
  # Create the temp file in the user's temp directory and
  # use the current pid to avoid comflict with other instances
  # that might be running.

  $TempBat = Join-Path $Env:TEMP "getvc$pid.bat"
  #echo "TempBat = $TempBat"
  #echo "file = $File"
  #echo "target = $target"
  ("call ""$File"" $target", "echo ENV_VARS_START", "set") | Out-File -Encoding ascii $TempBat
  $CmdOut = cmd /q /c $TempBat

  # If batch file fails we need to propagate the error.
  if ($LASTEXITCODE -gt 0) {
    $CmdOut
    exit $LASTEXITCODE
  }

  Remove-Item $TempBat | Out-Null

  ## Erase our current set of environment variables
  Remove-Item -path env:* | Out-Null

  ## Go through the environment variables returned by cmd.exe.
  ## For each of them, set the variable in our local environment.

  $FoundStartFlag = $false

  foreach ($Line in $CmdOut) {
    if ($Line -eq "ENV_VARS_START") {
      Write-Output ""
      $FoundStartFlag = $true
      continue
    }

    if ($FoundStartFlag -and ($Line -match "^(.*?)=(.*)$")) {
      $N = $matches[1]
      if ($N -eq "prompt") {
        # Ignore: Setting the prompt environment variable has no
        #         connection to the PowerShell prompt
      } elseif ($N -eq "title") {
        $host.ui.rawui.windowtitle = $matches[2]
        Set-Item -Path "env:$N" -Value $matches[2]
      } else {
        Set-Item -Path "env:$N" -Value $matches[2]
      }
    }
    elseif (!$FoundStartFlag) {
      # Output prior to our special flag is stuff generated by
      # setenv.cmd. Just pass it on to whomever is watching.
      Write-Output $Line
    }
  }
}

# -------------------------------------------------------------------------
#
# Set Visual Studio Command line environment variables.
# 
# -------------------------------------------------------------------------

function Global:MgVC12Vars64
{
  MgEnvCapture "$Env:VS120COMNTOOLS\..\..\vc\vcvarsall.bat" "x64"
}

function Global:MgVC14Vars64
{
  MgEnvCapture "$Env:VS140COMNTOOLS\..\..\vc\vcvarsall.bat" "x64"
}

function Global:MgVCVars64
{
  $devVersion = $Global:mgll.DevVersion
  $commonTools = Get-Content env:VS$($devVersion)COMNTOOLS
  MgEnvCapture "$commonTools\..\..\vc\vcvarsall.bat" "x64"
}

function Global:MgMkBldDir([string] $buildtype)
{
    $Exists = Test-Path $Global:mgll.llvmbld
    if (!$Exists) {
    New-Item $Global:mgll.llvmbld -itemtype Directory  | Out-Null
    }
    if (!$Global:mgll.uni) {
        $Exists = Test-Path $Global:mgll.llilcbld
        if (!$Exists) {
        New-Item $Global:mgll.llilcbld -itemtype Directory  | Out-Null
        }
        if ($Global:mgll.clangp) {
            $Exists = Test-Path $Global:mgll.clangbld
            if (!$Exists) {
            New-Item $Global:mgll.clangbld -itemtype Directory  | Out-Null
            }
        }
    }
}

function Global:MgMkWk
{
  $work = $Global:mgll.work
  Write-Output "work is $work"
  $Exists = Test-Path $work
  if (!$Exists) {
    New-Item $work -itemtype Directory  | Out-Null
  }

  $Exists = Test-Path $Global:mgll.test
  if (!$Exists) {
    New-Item $Global:mgll.test -itemtype Directory  | Out-Null
  }
  $Exists = Test-Path $Global:mgll.roslynrun
  if (!$Exists) {
    New-Item $Global:mgll.roslynrun -itemtype Directory  | Out-Null
  }
}

#### Clone functions

# CoreClr is always in its own directory.
function Global:MgCloneClr
{
  Push-Location $Global:mgll.work
  git clone https://github.com/dotnet/coreclr.git 
  Set-Location coreclr 
  git remote add personal https://github.com/$($Global:mgll.GitUserName)/coreclr
  Pop-Location
}

function Global:MgCloneRoslyn
{
  Push-Location $Global:mgll.work
  git clone https://github.com/dotnet/roslyn.git 
  Set-Location roslyn 
  git remote add personal https://github.com/$($Global:mgll.GitUserName)/roslyn
  Pop-Location
}

function Global:MgRestoreRoslyn
{
  Push-Location $Global:mgll.roslynsrc
  .\.nuget\NuGetRestore.ps1
  Pop-Location
}

function Global:MgCloneLLVM
{
  Push-Location $Global:mgll.work
  git clone -b MS https://github.com/microsoft/llvm llvm
  Set-Location llvm 
  git remote add personal https://github.com/$($Global:mgll.GitUserName)/llvm
  Pop-Location
}

function Global:MgClonellilc
{
  if ($Global:mgll.uni) {
      Push-Location $Global:mgll.llvmtoolssrc
  }
  else {
      Push-Location $Global:mgll.work
  }
  git clone https://github.com/dotnet/llilc
  Set-Location llilc 
  git remote add personal https://github.com/$($Global:mgll.GitUserName)/llilc
  Pop-Location
}

function Global:MgCloneClang
{
  if (!$Global:mgll.clangp) {
    throw "MgCloneClang: Not configured for clang"
  }
  if ($Global:mgll.uni) {
      Push-Location $Global:mgll.llvmtoolssrc
  }
  else {
      Push-Location $Global:mgll.work
  }
  git clone -b MS https://github.com/microsoft/clang
  Set-Location clang 
  git remote add personal https://github.com/$($Global:mgll.GitUserName)/clang
  Pop-Location
}

function Global:MgCloneExtra
{
  Push-Location $Global:mgll.clangsrc
  Set-Location tools
  git clone -b MS https://github.com/microsoft/clang-tools-extra extra
  Set-Location extra 
  git remote add personal https://github.com/$($Global:mgll.GitUserName)/clang-tools-extra
  Pop-Location
}

# Collective cloning methods

function Global:MgCloneLLS
{
  MgCloneLLVM
  MgClonellilc
  if ($Global:mgll.clangp) {
      MgCloneClang
      MgCloneExtra
  }
}

#### Cmake functions

# Unified Cmake
function Global:MgCmLLVM
{
  Push-Location $Global:mgll.llvmbld
  if ($Global:mgll.uni) {
      cmake -G "Visual Studio $($Global:mgll.CmakeVersion) Win64" $Global:mgll.llvmsrc "-DWITH_CORECLR=$($Global:mgll.clrbld)" -DLLVM_ENABLE_DOXYGEN=ON -DLLVM_TARGETS_TO_BUILD:STRING=X86 "-DCMAKE_BUILD_TYPE=$($Global:mgll.buildtype)" $Global:mgll.buildcmakeoptions $args
  }
  else {
      cmake -G "Visual Studio $($Global:mgll.CmakeVersion) Win64" $Global:mgll.llvmsrc -DLLVM_ENABLE_DOXYGEN=ON -DLLVM_TARGETS_TO_BUILD:STRING=X86 "-DCMAKE_BUILD_TYPE=$($Global:mgll.buildtype)" $Global:mgll.buildcmakeoptions $args
  }
  Pop-Location
}

function Global:MgCmLLILC
{
  if ($Global:mgll.uni) {
      throw "MgCmLLILC: Not configured for separate cmake"
  }
  Push-Location $Global:mgll.llilcbld
  cmake -G "Visual Studio $($Global:mgll.CmakeVersion) Win64" $Global:mgll.llilcsrc "-DWITH_CORECLR=$($Global:mgll.clrbld)" "-DWITH_LLVM=$($Global:mgll.llvmbld)" -DLLVM_TARGETS_TO_BUILD:STRING=X86 -DLLVM_ENABLE_DOXYGEN=ON "-DCMAKE_BUILD_TYPE=$($Global:mgll.buildtype)" $Global:mgll.buildcmakeoptions
  Pop-Location
}

function Global:MgCmClang
{
  if ($Global:mgll.uni) {
      throw "MgCmClang: Not configured for separate cmake"
  }
  Push-Location $Global:mgll.clangbld
  # Put llvm-config on path so clang can find it.
  $savePath = $env:path
  $llvmConfigPath = Join-Path $Global:mgll.llvmbld "Debug\bin"
  $env:path =  $llvmConfigPath + ";" + $env:path
  cmake -G "Visual Studio $($Global:mgll.CmakeVersion) Win64" $Global:mgll.clangsrc "-DCMAKE_BUILD_TYPE=$($Global:mgll.buildtype)" $Global:mgll.buildcmakeoptions $args
  $env:path = $savePath
  Pop-Location
}

#### Build functions

function Global:MgBldClr
{
  # Note: the binaries are built in the source tree, that is we
  # we set location to the source rather than the binary.
  Push-Location $Global:mgll.clrsrc
  ./build.cmd vs2015
  Pop-Location
}

function Global:MgBldClrNoTests
{
  # Note: the binaries are built in the source tree, that is we
  # we set location to the source rather than the binary.
  Push-Location $Global:mgll.clrsrc
  ./build_notests.cmd
  Pop-Location
}


function Global:MgBldRoslyn
{
  # Note: the binaries are built in the source tree, that is we
  # we set location to the source rather than the binary.
  Push-Location $Global:mgll.roslynsrc
  MgVCVars64
  msbuild /m /v:d src/Compilers/CSharp/CscCore/CscCore.csproj
  Pop-Location
}

function Global:MgLLILCBldRoslyn([string]$rosrunsubdir)
{
    $rosrundir = Join-Path $Global:mgll.roslynsrc $rosrunsubdir
    Push-Location $Global:mgll.roslynsrc
    MgVCVars64
    msbuild /fl /m /v:d /p:CSCTOOLPATH=$rosrundir /p:CSCTOOLEXE=runcsc.cmd src/Compilers/CSharp/CscCore/CscCore.csproj
    Pop-Location
}

function Global:MgRyujitBldRoslyn([string]$rosrunsubdir)
{
    $rosrundir = Join-Path $Global:mgll.roslynsrc $rosrunsubdir
    Push-Location $Global:mgll.roslynsrc
    MgVCVars64
    msbuild /fl /m /v:d /p:CSCTOOLPATH=$rosrundir /p:CSCTOOLEXE=csc.exe src/Compilers/CSharp/CscCore/CscCore.csproj
    Pop-Location
}

function Global:MgBldLLVM
{
  MgVCVars64
  Push-Location $Global:mgll.llvmbld
  msbuild LLVM.sln /p:Configuration=$($Global:mgll.buildtype) /p:Platform=x64 /t:ALL_BUILD
  Pop-Location
}

function Global:MgBldLLILC
{
  if ($Global:mgll.uni) {
      throw "MgBldLLILC: Not configured for separate build"
  }
  MgVCVars64
  Push-Location $Global:mgll.llilcbld
  msbuild LLILC.sln /p:Configuration=$($Global:mgll.buildtype) /p:Platform=x64 /t:ALL_BUILD
  Pop-Location
}

function Global:MgBldClang
{
  if ($Global:mgll.uni) {
      throw "MgBldClang: Not configured for separate build"
  }
  if (!$Global:mgll.clangp) {
      throw "MgBldClang: Not configured for clang"
  }
  MgVCVars64
  Push-Location $Global:mgll.clangbld
  msbuild Clang.sln /p:Configuration=$($Global:mgll.buildtype) /p:Platform=x64 /t:ALL_BUILD
  Pop-Location
}

# Build LLVM and dependent tools.
function Global:MgBldLLS
{
  MgCmLLVM

  # LLVM build needed before LLILC cmake
  # or Clang cmake.
  MgBldLLVM

  if (!$Global:mgll.uni) {
      MgCmLLILC
      MgBldLLILC
      if ($Global:mgll.clangp) {
          MgCmClang
          MgBldClang
      }
  }
}

#### Configure and build in proper order

# Do all cmakes and builds. Assumes cloning has been done.
function Global:MgCmBldAll
{
  # Do CoreClr first as other need it but it has no dependencies.
  MgBldClr
  MgBldLLS
}

# Just rebuild LLILC after a change.
function Global:MgRebldLLILC
{
  if ($Global:mgll.uni) {
    MgBldLLVM
  }
  else {
    MgBldLLILC
  }
}

#### branching

function Global:MgBranchDir([string]$dir)
{
  Push-Location $dir
  Write-Output "--------- Making branch $($Global:mgll.branch) in $dir ---------"
  git checkout -b $Global:mgll.branch
  Pop-Location
}

function Global:MgBranch
{
  MgBranchDir $Global:mgll.llvmsrc
  MgBranchDir $Global:mgll.llilcsrc
  if ($Global:mgll.clangp) {
      MgBranchDir $Global:mgll.clangsrc
      MgBranchDir $Global:mgll.extrasrc
  }
}

#### Sync/merge functions.

function Global:MgFMWorker
{
  git fetch origin -v
  git merge origin/master
}

function Global:MgFMDir([string]$dir)
{
  Push-Location $dir
  Write-Output "--------- Doing fetch and merge in $dir ---------"
  MgFMWorker
  Pop-Location
}


function Global:MgFMClr
{
  MgFMDir $Global:mgll.clrsrc
}

function Global:MgFM
{
  MgFMDir $Global:mgll.llvmsrc
  MgFMDir $Global:mgll.llilcsrc
  if ($Global:mgll.clangp) {
      MgFMDir $Global:mgll.clangsrc
      MgFMDir $Global:mgll.extrasrc
  }
}

# Status functions

function Global:MgStatDir([string]$dir)
{
  Push-Location $dir
  Write-Output "--------- Doing git status in $dir ---------"
  git status
  Pop-Location
}

function Global:MgStatClr
{
  MgStatDir $Global:mgll.clrsrc
}

function Global:MgStat
{
  MgStatDir $Global:mgll.llvmsrc
  MgStatDir $Global:mgll.llilcsrc
  if ($Global:mgll.clangp) {
      MgStatDir $Global:mgll.clangsrc
      MgStatDir $Global:mgll.extrasrc
  }
}

# Test functions

function Global:MgPyTest
{
  $Exists = Test-Path $Global:mgll.testdir
  if ($Exists) {
    Remove-Item -Recurse $Global:mgll.testdir 
  }
  New-Item $Global:mgll.testdir -itemtype Directory  | Out-Null
  Push-Location $Global:mgll.clrtests
  python $Global:mgll.runtestpy -a x64 -b debug -d summary -r $Global:mgll.testdir -j $Global:mgll.llilcjit -c $Global:mgll.clrbld
  $code = $lastexitcode
  Pop-Location
  Write-Output "Test script exited with code $code"
  if ($code -ne 0) {
    throw "There were failures and/or diffs"
  }
}

function Global:MgPySingle([string]$testpath, [bool]$debug)
{
  $extra = ""
  if ($debug) {
    $extra = $Global:mgll.windbg

    Write-Output "python $($Global:mgll.runtestpy) -a x64 -b debug -d summary -j $($Global:mgll.llilcjit) -c $($Global:mgll.clrbld) -s $testpath -g $extra"
  }
  python $Global:mgll.runtestpy -a x64 -b debug -d summary -j $Global:mgll.llilcjit -c $Global:mgll.clrbld -s $testpath --ca '/v' -g $extra
  $code = $lastexitcode
  Write-Output "Test exited with code $code"
  if ($code -ne 0) {
    throw "There were failures and/or diffs"
  }
}

function Global:MgPushDir([string]$dirname)
{
    write-output "Greetings form MgPush, dirname=$dirname"
    $hash = $Global:mgll.src
    #Write-Output "hash = $hash"
    #$hash | Out-Default
    $dir = $hash[$dirname]
    Write-Output "dir = $dir"
    $baseBranch = $Global:mgll.baseBranchMap[$dirname]
    $mergeBranch = $Global:mgll.branch
    $headDir = Join-Path $dir ".git/refs/heads"
    $baseCommitHeadFile = Join-Path $headDir $baseBranch
    $mergeCommitHeadFile = Join-Path $headDir $mergeBranch
    $baseCommit = Get-Content $baseCommitHeadFile
    $mergeCommit = Get-Content $mergeCommitHeadFile
    Write-Output "base commit is $baseCommit"
    Write-Output "merge commit is $mergeCommit"
    $doPush = $false
    if ($baseCommit -eq $mergeCommit) {
        Write-Output "Merge has no changes for dirname=$dirname"
        $doPush = ($dirname -eq "llilc")
    }
    else {
        $doPush = $true
    }
    if ($doPush) {
        Write-Output "Pushing dirname=$dirname"
        Push-Location $dir
        git push --set-upstream origin $mergeBranch
        Pop-Location
    }
}

function Global:MgPush
{
  MgPushDir llvm
  MgPushDir llilc
  if ($Global:mgll.clangp) {
      MgPushDir clang
      MgPushDir extra
  }
}

function Global:MgUpdateDir([string]$dirname)
{
  write-output "Greetings form MgUpdateDir, dirname=$dirname"
  $hash = $Global:mgll.src
  #Write-Output "hash = $hash"
  #$hash | Out-Default
  $dir = $hash[$dirname]
  Write-Output "dir = $dir"
  $baseBranch = $Global:mgll.baseBranchMap[$dirname]
  Push-Location $dir
  git fetch origin -v
  git merge origin/$baseBranch
  Pop-Location
}

function Global:MgUpdate
{
  MgUpdateDir llvm
  MgUpdateDir llilc
  if ($Global:mgll.clangp) {
      MgUpdateDir clang
      MgUpdateDir extra
  }
}

function Global:MgFullBuildDir([string] $baseDir)
{
    return $baseDir + "-" + $Global:mgll.buildtype
}

function Global:MgSetBuildType([string] $buildtype)
{
    $buildtype = MgExpandBuildType $buildtype
    $Global:mgll.buildtype = $buildtype
    $Global:mgll.llvmbld = MgFullBuildDir($Global:mgll.llvmbldbase)
    $env:LLVMBUILD = $Global:mgll.llvmbld
    $Global:mgll.buildcmakeoptions = ""
    if ($buildtype -eq "RelWithDebInfo") {
        $Global:mgll.buildcmakeoptions = "-DLLVM_ENABLE_ASSERTIONS=ON"
    }
    if ($Global:mgll.uni) {
        $Global:mgll.llilcjit = Join-Path $Global:mgll.llvmbld "bin/$($buildtype)/llilcjit.dll"
    }
    else {
        $Global:mgll.llilcbld = MgFullBuildDir($Global:mgll.llilcbldbase)
        $Global:mgll.llilcjit = Join-Path $Global:mgll.llilcbld "bin/$($buildtype)/llilcjit.dll"
        if ($Global:mgll.clangp) {
            $Global:mgll.clangbld = MgFullBuildDir($Global:mgll.clangbldbase)
        }
    }
    # Make build directories.
    MgMkBldDir($buildtype)
}

function Global:MgCloneBranchMerge
{
  Write-Output("mgll is ")
  $Global:mgll | Out-Default
  MgCloneClr
  MgCloneLLS
  MgBranch
  MgFM
  MgStat
}

function Global:MgBldTest
{
  MgBldClr
  MgBldLLS
  MgPyTest
}

function Global:MgCopyJit
{
    Copy-Item $Global:mgll.llilcjit $Global:mgll.clrbld
}

function Global:MgCopyJitRos
{
    Copy-Item $Global:mgll.llilcjit $Global:mgll.roslynrun

}

# Just makes the boot directory
function Global:MgBootSetupRoslyn([string]$rosrunsubdir)
{
    MgRosRemoveDirectory($rosrunsubdir)
    $rosrundir = Join-Path $Global:mgll.roslynsrc $rosrunsubdir
    $rosrundirExists = Test-Path $rosrundir
    if (!$rosrundirExists) {
        New-Item $rosrundir -ItemType directory
    }
    else {
      throw "Failed to remove $rosrundir"
    }

    $roslynWild = Join-Path $Global:mgll.roslynsrc "Binaries\Debug\core-clr\*"
    Copy-Item -Force -Recurse $roslynWild -Destination $rosrundir

    $coreclrExeWild = Join-Path $Global:mgll.clrbld "*.exe"
    Copy-Item -Force $coreclrExeWild -Destination $rosrundir
    $coreclrDllWild = Join-Path $Global:mgll.clrbld "*.dll"
    Copy-Item -Force -Recurse $coreclrDllWild -Destination $rosrundir
    $coreclrPDBWild = Join-Path (Join-Path $Global:mgll.clrbld "PDB") "*.pdb"
    Copy-Item -Force -Recurse $coreclrPDBWild -Destination $rosrundir

    # Copy csc.exe to csc.dll and CoreConsole.exe to csc.exe. 
    # CoreConsole.exe (named csc.exe) masquerades as csc.exe by starting
    # up the CoreClr and then loading and executing csc.dll (really the original csc.exe).
    $cscexepath = Join-Path $rosrundir "csc.exe"
    $cscdllpath = Join-Path $rosrundir "csc.dll"
    $coreconsolepath = Join-Path $Global:mgll.clrbld "CoreConsole.exe"
    Copy-Item -Force $cscexepath $cscdllpath
    Copy-Item -Force $coreconsolepath $cscexepath
}

function Global:MgCopyRoslyn([string]$rosrunsubdir)
{
    MgRosRemoveDirectory($rosrunsubdir)
    $rosrundir = Join-Path $Global:mgll.roslynsrc $rosrunsubdir
    $rosrundirExists = Test-Path $rosrundir
    if (!$rosrundirExists) {
        New-Item $rosrundir -ItemType directory
    }
    else {
      throw "Failed to remove $rosrundir"
    }

    $roslynWild = Join-Path $Global:mgll.roslynsrc "Binaries\Debug\core-clr\*"
    Copy-Item -Force -Recurse $roslynWild -Destination $rosrundir

    if ($Global:mgll.sepclr) {
        # Do not copy from clr build.
        # Jit and python script goes where clr build is.
        $corerundir = $Global:mgll.clrbld
    }
    else {
        $coreclrExeWild = Join-Path $Global:mgll.clrbld "*.exe"
        Copy-Item -Force $coreclrExeWild -Destination $rosrundir
        $coreclrDllWild = Join-Path $Global:mgll.clrbld "*.dll"
        Copy-Item -Force -Recurse $coreclrDllWild -Destination $rosrundir
        $coreclrPDBWild = Join-Path (Join-Path $Global:mgll.clrbld "PDB") "*.pdb"
        Copy-Item -Force -Recurse $coreclrPDBWild -Destination $rosrundir

        $corerundir = $rosrundir
    }

    Copy-Item $Global:mgll.llilcjit $corerundir

    #("py", "pyproj", "sln" ) | % {
    #    $path = Join-Path $llilc_test_dir ("llilc_run." + $_)
    #    Copy-Item $path $corerundir
    #}

    # Copy csc.exe to csc.dll and CoreConsole.exe to csc.exe. 
    # CoreConsole.exe (named csc.exe) masquerades as csc.exe by starting
    # up the CoreClr and then loading and executing csc.dll (really the original csc.exe).
    $cscexepath = Join-Path $rosrundir "csc.exe"
    $cscdllpath = Join-Path $rosrundir "csc.dll"
    $coreconsolepath = Join-Path $Global:mgll.clrbld "CoreConsole.exe"
    Copy-Item -Force $cscexepath $cscdllpath
    Copy-Item -Force $coreconsolepath $cscexepath

    $command = ''

    $llilc_test_dir = Join-Path $Global:mgll.llilcsrc "test"
    $pyrunpath = Join-Path $llilc_test_dir llilc_run.py
    $command += 'python "' + $pyrunpath + '"'

    $command += ' --llilc-coreclr-runtime-path "' + $Global:mgll.clrbld + '"'

    $csc_path = Join-Path $rosrundir "csc.exe"
    $command += ' --llilc-app-path "' + $csc_path + '"'
    
    $command += ' %*'

    $content = ($command, "exit /b %ERRORLEVEL%")

    $RosCmd = Join-Path $rosrundir "runcsc.cmd"
    $content | Out-File  -Encoding ascii $RosCmd


}

# Create a small command file named $name.cmd which will 
# use LLILC to compile and execute the managed program at
# $path.
function Global:MgMakeLlilcRunCmd([string]$name, [string]$pathandextraargs)
{
    # Create a small command file to invoke the python script
    # that does the real work of running the application.
    $RosCmd = Join-Path $Global:mgll.clrbld "$name.cmd"
    $content = (('python "%~dp0llilc_run.py" ' + $pathandextraargs + ' %*'), "exit /b %ERRORLEVEL%")
    $content | Out-File  -Encoding ascii $RosCmd
}

function Global:MgRosTestClone
{
  MgCloneClr
  MgCloneLLS
  MgCloneRoslyn
}

function Global:MgRosTestBld
{
  MgBldClr
  MgBldLLS
  MgRestoreRoslyn
  MgBldRoslyn
  MgCopyRoslyn
  MgCopyJitRos
}

function Global:MgRosRemoveDirectory([string]$subdir)
{
    $roslyn_dir = Join-Path $Global:mgll.roslynsrc $subdir
    $rosdirExists = Test-Path $roslyn_dir
    if ($rosdirExists) {
        Remove-Item -Recurse -Force $roslyn_dir
    }
}

function Global:MgRosCopyDirectory([string]$srcsubdir, [string]$destsubdir)
{
    $srcdir = Join-Path $Global:mgll.roslynsrc $srcsubdir
    $destdir = Join-Path $Global:mgll.roslynsrc $destsubdir
    Copy-Item -Recurse -Force $srcdir $destdir
}

function Global:MgRosStage([int]$stage)
{
    $stagedir = "stage" + $stage
    MgRosRemoveDirectory("Binaries")
    MgRosRemoveDirectory($stagedir)
    if ($stage -le 0) {
        MgBldRoslyn
    }
    else {
        $prevStageDir = "stage" + ($stage - 1)
        MgLLILCBldRoslyn($prevStageDir)
    }
    MgCopyRoslyn $stagedir
}

function Global:MgLLILCEnv
{
  $env:COMPlus_JitNoInline="1"
  $env:COMPlus_AltJit="DomainNeutralILStubClass:IL_STUB_PInvoke"
  $env:COMPlus_AltJitName="llilcjit.dll"
  $env:COMPlus_GCConservative="1"
  $env:COMPlus_ZapDisable="1"
  $env:COMPlus_DumpLLVMIR="summary"
}

function Global:MgRosRunWindbg
{
    Push-Location $Global:mgll.rosportable
    MgLLILCEnv
    $cmd =  "$($Global:mgll.windbg) $($Global:mgll.roscorerun) $($Global:mgll.roscsc) @$($Global:mgll.rosrsp)"
    Write-Output "Command: $cmd"
    &$cmd
    Pop-Location
}

function Global:MgExpandBuildType([string]$buildtype)
{
    switch -Regex ($buildtype) {
        "RelWithDebInfo" { break }
        "d.*" { $buildtype = "Debug"; break }
        "r.*" { $buildtype = "Release"; break }
        "(c.*|RelWithDebInfo)" { $buildtype = "RelWithDebInfo"; break }
        default { throw "Unknown build type abbrev: $buildtype"; break }
    }
    $buildtype
}

function Global:MgDevClone
{
  MgCloneClr
  MgCloneLLS
}

function Global:MgDevBld
{
  MgBldClr
  MgBldLLS
}

function Global:MgSetDevVersion([string]$devVersion)
{
    if (($devVersion -ne "120") -and ($devVersion -ne "140")) {
        throw "Unsupported dev version: $devVersion"
    }
    $Global:mgll.DevVersion = $devVersion
}

function Global:MgWorkDirName([string] $subdir)
{
    if ($Global:mgll.BaseWorkDir -eq "") {
        return $subdir
    }
    return Join-Path $Global:mgll.BaseWorkDir $subdir
}

function Global:MgFormat([bool]$fix = $false)
{
    Push-Location $mgll.llilcsrc
    MgVC12Vars64
    $fixarg = ""
    if ($fix) {
        $fixarg = "--fix"
    }

    C:\Python34\python.exe utils/ccformat.py --llvm-source $mgll.llvmsrc --llvm-build $mgll.llvmbld --coreclr-build $mgll.clrbld $fixarg
    Pop-Location
    # Switch back to selected VS.
    MgVCVars64

}
function Global:MgDev12Init([string] $subdir, [string]$buildtype)
{
    $wk = MgWorkDirName $subdir
    $buildtype = MgExpandBuildType $buildtype
    $branch = "fix"
    $uni = $true
    $clangp = $false
    MgInit $wk $buildtype $branch $uni $clangp "120"
}

function Global:MgDevInit([string] $subdir, [string]$buildtype)
{
    $wk = MgWorkDirName $subdir
    $buildtype = MgExpandBuildType $buildtype
    $branch = "fix"
    $uni = $true
    $clangp = $false
    MgInit $wk $buildtype $branch $uni $clangp "140"
}

# Setup to do a forward integration.
function Global:MgFiInit([string] $subdir, [string]$buildtype = "c")
{
    $wk = MgWorkDirName $subdir
    $buildtype = MgExpandBuildType $buildtype
    $branch = "mergetest"
    $uni = $true
    $clangp = $true
    MgInit $wk $buildtype $branch $uni $clangp "140"
}

function Global:MgRosInit([string] $subdir, [string]$buildtype = "c")
{
    $wk = MgWorkDirName $subdir
    $buildtype = MgExpandBuildType $buildtype
    $branch = "rosfix"
    $uni = $true
    $clangp = $false
    MgInit $wk $buildtype $branch $uni $clangp "140"
}

function Global:MgDocInit
{
    $wk = MgWorkDirName "doc"
    $buildtype = "Debug"
    $branch = "updatedoc"
    $uni = $false
    $clangp = $false
    MgInit $wk $buildtype $branch $uni $clangp "120"
}

# Initialize a LLILC workspace
#
# \param $wk           Path to the workspace
# \param $buildtype    Debug, Release, or RelWithDebInfo
# \param $branch       Branch on which modifications are to be made.
# \param $uni          True if in unified LLVM mode (i.e. LLILC and CLANG in LLVM repo)
# \param $clangp       True if include clang.
# \param $devversion   12 or 14.
# 
function Global:MgInit([string]$wk, [string]$buildtype, [string]$branch, 
                       [bool]$uni, [bool]$clangp, [string]$devVersion)
{
    $buildtype = MgExpandBuildType $buildtype
    if ($wk -eq "") {
        throw "You must use specify the location of the workspace";
    }
    $Global:mgll.work = $wk

    if ($buildtype -eq "") {
        throw "You must use specify the build type";
    }
    $Global:mgll.buildtype = $buildtype


    if ($branch -eq "") {
        throw "You must use specify the branch";
    }
    $Global:mgll.branch = $branch
    $Global:mgll.uni = $uni
    $Global:mgll.clangp = $clangp

    if (($devVersion -ne "120") -and ($devVersion -ne "140")) {
        throw "Unsupported dev version: $devVersion"
    }
    $Global:mgll.DevVersion = $devVersion
    $devMajorVersion = $devVersion.Substring(0, 2)
    $Global:mgll.DevMajorVersion = $devMajorVersion
    if ($devMajorVersion -eq "12") {
        $Global:mgll.CmakeVersion = $devMajorVersion + " 2013"
    }else{
        $Global:mgll.CmakeVersion = $devMajorVersion + " 2015"
    }

    $Global:mgll.clrsrc = Join-Path $wk "coreclr"
    $env:CORECLRSOURCE = $Global:mgll.clrsrc
    $Global:mgll.clrtests = Join-Path $Global:mgll.clrsrc "tests"
    $Global:mgll.clrbld = Join-Path $Global:mgll.clrsrc "bin/Product/Windows_NT.x64.Debug"

    $Global:mgll.roslynsrc = Join-Path $wk "roslyn"
    $Global:mgll.roslynrun = Join-Path $wk "rosrun"
    $Global:mgll.rosportable = Join-Path $Global:mgll.roslynsrc "src/Compilers/CSharp/Portable"
    $Global:mgll.rosrsp = Join-Path $wk  "rsps/Microsoft.CodeAnalysis.CSharp.DLl.rsp"
    $Global:mgll.roscsc = Join-Path $Global:mgll.roslynrun "csc.exe"
    $Global:mgll.roscorerun = Join-Path $Global:mgll.roslynrun "corerun.exe"
    $Global:mgll.roscoreclrbindir = Join-Path $Global:mgll.roslynsrc "Binaries\Debug\core-clr"
  
    $Global:mgll.windbg = "c:/dbg64/windbg.exe"

    $Global:mgll.llvmsrc = Join-Path $wk "llvm"
    $env:LLVMSOURCE = $Global:mgll.llvmsrc
    $Global:mgll.llvmtoolssrc = Join-Path $Global:mgll.llvmsrc "tools"
    $Global:mgll.llvmbldbase = Join-Path $wk "llvm-build"

    if ($Global:mgll.uni) {
        # Unified paths
        $Global:mgll.llilcsrc = Join-Path $Global:mgll.llvmsrc "tools/llilc"
        $env:LLILCSOURCE = $Global:mgll.llilcsrc
        if ($clangp) {
            $Global:mgll.clangsrc = Join-Path $Global:mgll.llvmsrc "tools/clang"
        }
    }
    else {
        # Separate path
        $Global:mgll.llilcsrc = Join-Path $wk "llilc"
        $Global:mgll.llilcbldbase = Join-Path $wk "llilc-build"
        if ($clangp) {
            $Global:mgll.clangsrc = Join-Path $wk "clang";
            $Global:mgll.clangbldbase = Join-Path $wk "clang-build"
        }
    }
    if ($clangp) {
        $Global:mgll.extrasrc = Join-Path $Global:mgll.clangsrc "tools/extra"
    }

    $Global:mgll.runtestpy = Join-Path $Global:mgll.llilcsrc "test/llilc_runtest.py"
    $Global:mgll.test = Join-Path $wk test
    $Global:mgll.testdir = Join-Path $wk "target"

    $baseBranch = @{}
    $baseBranch.llvm = "MS"
    $baseBranch.llilc = "master"
    if ($clangp) {
        $baseBranch.clang = "MS"
        $baseBranch.extra = "MS"
    }
    $Global:mgll.baseBranchMap = $baseBranch

    $src = @{}
    $src.llvm = $Global:mgll.llvmsrc
    $src.llilc = $Global:mgll.llilcsrc
    if ($clangp) {
        $src.clang = $Global:mgll.clangsrc
        $src.extra = $Global:mgll.extrasrc
    }
    $Global:mgll.src = $src

    MgSetBuildType $buildtype

    # Controls whether coreclr is copied into stage directories or kept
    # separate.
    $Global:mgll.sepclr = $true

    Write-Output("mgll is ")
    $Global:mgll | Out-Default
    MgMkWk
    Push-Location $Global:mgll.work
}


# -------------------------------------------------------------------------
#
# The Script
#
# -------------------------------------------------------------------------

$Global:mgll | Out-Default
