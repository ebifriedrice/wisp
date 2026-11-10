# How to use the MSBuild SDK

This page shows you how to build and run a Wisp project with `dotnet msbuild`
using the `Wisp.Sdk` package.

The SDK runs the installed `wisp` tool; it does not replace the tool.

## Install the tool and SDK packages

Build the packages from the repository:

```console
$ dotnet pack src/Wisp.Cli/Wisp.Cli.fsproj -c Release -o artifacts/nuget
$ dotnet pack sdk/Wisp.Sdk/Wisp.Sdk.csproj -c Release -o artifacts/nuget
$ dotnet tool install --tool-path artifacts/tools \
    --add-source artifacts/nuget Wisp.Cli --version 0.1.2-bootstrap
```

## Pin the SDK version

Create a `global.json` next to your project:

```json
{
  "msbuild-sdks": {
    "Wisp.Sdk": "0.1.2-bootstrap"
  }
}
```

If the packages are not in your default NuGet feeds, add a `NuGet.config` that
points at the directory containing the `.nupkg` files:

```xml
<configuration>
  <packageSources>
    <add key="wisp-local" value="../../artifacts/nuget" />
  </packageSources>
</configuration>
```

## Use the SDK in the project file

```xml
<Project Sdk="Wisp.Sdk">
  <PropertyGroup>
    <Name>demo.app</Name>
    <RootNamespace>demo.app</RootNamespace>
    <StartupObject>demo.app/main</StartupObject>
  </PropertyGroup>
  <ItemGroup>
    <Wisp Include="src/**/*.wisp" />
  </ItemGroup>
</Project>
```

The manifest body is the same as a plain `*.wisp.proj`; the `Sdk` attribute adds
the build targets.

## Build and run

```console
$ dotnet msbuild app.wisp.proj -t:WispBuild -p:WispTool=artifacts/tools/wisp
$ dotnet msbuild app.wisp.proj -t:WispRun   -p:WispTool=artifacts/tools/wisp
```

`WispRun` depends on `WispBuild`.

If the tool is already on `PATH`, omit `-p:WispTool=...`.

The targets are:

- `WispBuild` — run `wisp build`.
- `WispRun` — run `wisp build`, then `wisp run`.
- `Wisp` — alias for `WispRun`.

See the [project file reference](../reference/project-file.md) for the SDK
properties and target list.
