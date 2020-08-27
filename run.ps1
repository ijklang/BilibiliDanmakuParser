dotnet build -v:q
Write-Host "--------------------------------------"
./bin/Debug/netcoreapp3.1/BilibiliDanmakuParser $args[0] $args[1]