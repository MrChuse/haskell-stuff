ghc %1.hs -outputdir dist && %1.exe && del %1.exe
@REM && rmdir /q /s dist