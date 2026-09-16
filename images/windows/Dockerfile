FROM mcr.microsoft.com/windows/servercore:ltsc2022

SHELL ["powershell", "-command"]

COPY zlibwapi.lib zlibwapi.dll C:/

WORKDIR C:/

RUN	echo 'Installing chocolatey windows package manager' ; \
	iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1')) ; \
	choco feature enable -n=allowGlobalConfirmation

RUN choco install R ; \
    choco install rtools ; \
    choco install python ; \
    Import-Module $env:ChocolateyInstall\helpers\chocolateyProfile.psm1 ; \
    refreshenv ; \
    $rPath = (Get-ChildItem 'C:\Program Files\R' -Directory | Sort-Object Name -Descending | Select-Object -First 1).FullName ; \
    $pyPath = (Get-ChildItem 'C:\' -Directory -Filter 'Python*' | Sort-Object Name -Descending | Select-Object -First 1).FullName ; \
    Move-Item $rPath 'C:\R' ; \
    Move-Item $pyPath 'C:\Py' ; \
    [System.Environment]::SetEnvironmentVariable('PATH', [System.Environment]::GetEnvironmentVariable('PATH', 'Machine') + ';C:\R\bin;C:\Py;C:\Py\Scripts', 'Machine') ; \
    $env:Path += ';C:\R\bin;C:\Py;C:\Py\Scripts' ; \
    Rscript -e \"install.packages(c('R6','Rcpp','R.utils','testthat','collections'), repos='https://cloud.r-project.org'); invisible(sapply(c('R6','Rcpp','R.utils','testthat','collections'), function(p) library(p, character.only=TRUE)))\" ; \
    if ($LASTEXITCODE -ne 0) { throw 'R dependency install failed - see Rscript output above' } ; \
    Remove-Item -Recurse -Force "$env:TEMP\chocolatey" -ErrorAction SilentlyContinue ; \
    Remove-Item -Recurse -Force "$env:ChocolateyInstall\lib-bkp" -ErrorAction SilentlyContinue ; \
    Remove-Item -Recurse -Force "$env:ChocolateyInstall\logs\*" -ErrorAction SilentlyContinue

# R/Python are unpinned (always install latest); the RUN step above discovers whatever
# got installed and moves it to C:\R / C:\Py, so this PATH never needs a version number.
# Rtools is intentionally NOT moved or added here - R finds it on its own via Makeconf,
# and relocating it breaks its bundled MSYS2 shell (gcc stops resolving inside it).
ENV PATH="C:\\R\\bin;C:\\Py;C:\\Py\\Scripts;C:\\ProgramData\\chocolatey\\bin;C:\\Windows\\system32;C:\\Windows;C:\\Windows\\System32\\Wbem;C:\\Windows\\System32\\WindowsPowerShell\\v1.0\\;C:\\Windows\\System32\\OpenSSH\\;${Path}"

# Fail the build if PATH is broken, instead of surfacing as a failure in a later CI job.
RUN $ErrorActionPreference = 'Stop' ; \
    python --version ; \
    R.exe --version ; \
    Rscript --version

CMD ["powershell"]
