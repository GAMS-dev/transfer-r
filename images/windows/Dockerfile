FROM mcr.microsoft.com/windows/server:ltsc2022

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
    [System.Environment]::SetEnvironmentVariable('PATH', [System.Environment]::GetEnvironmentVariable('PATH', 'Machine') + ';C:\Program Files\R\R-4.4.1\bin;C:\rtools44\usr\bin;C:\rtools44\mingw64\bin', 'Machine')

# install dependencies
RUN Rscript -e \"install.packages('R6', repos='http://cran.us.r-project.org')\" ; \
    Rscript -e \"install.packages('Rcpp', repos='http://cran.us.r-project.org')\" ; \
    Rscript -e \"install.packages('R.utils', repos='http://cran.us.r-project.org')\" ; \
    Rscript -e \"install.packages('testthat', repos='http://cran.us.r-project.org')\" ; \
    Rscript -e \"install.packages('collections', repos='http://cran.us.r-project.org')\"

CMD ["powershell"]
