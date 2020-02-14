@echo off

dotnet tool restore

# Use paket and not fake for restoring packages
# c.f. https://github.com/fsharp/FAKE/issues/2181 
dotnet paket restore
set PAKET_SKIP_RESTORE_TARGETS true

dotnet fake run build.fsx %*
