@echo off
setlocal
if "%VCPKG_ROOT%"=="" if defined VCPKG_INSTALLATION_ROOT set "VCPKG_ROOT=%VCPKG_INSTALLATION_ROOT%"
if "%VCPKG_ROOT%"=="" if exist "C:\vcpkg\scripts\buildsystems\vcpkg.cmake" set "VCPKG_ROOT=C:\vcpkg"
if "%VCPKG_ROOT%"=="" if exist "C:\tools\vcpkg\scripts\buildsystems\vcpkg.cmake" set "VCPKG_ROOT=C:\tools\vcpkg"
if "%VCPKG_ROOT%"=="" (
    echo VCPKG_ROOT is not set and no vcpkg install was found. 1>&2
    exit /b 1
)

cmake -G Ninja -S . -B _build_cmake ^
    -DCMAKE_BUILD_TYPE=Release ^
    -DCMAKE_TOOLCHAIN_FILE="%VCPKG_ROOT%/scripts/buildsystems/vcpkg.cmake" ^
    -DERLANG_ERTS_DIR=%1 ^
    -DERL_INTERFACE_DIR=%2 ^
    -DANGLE_INCLUDE_DIR="%ANGLE_INCLUDE_DIR%" ^
    -DANGLE_LIB_DIR="%ANGLE_LIB_DIR%"
exit /b %ERRORLEVEL%
