#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
version="${1:-0.1.2-bootstrap}"
tool="${WISP_TOOL:-$root/artifacts/tools/wisp}"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

mkdir -p "$tmp/src"

cat > "$tmp/global.json" <<EOF
{
  "msbuild-sdks": {
    "Wisp.Sdk": "$version"
  }
}
EOF

cat > "$tmp/NuGet.config" <<EOF
<configuration>
  <packageSources>
    <clear />
    <add key="local" value="$root/artifacts/nuget" />
  </packageSources>
</configuration>
EOF

cat > "$tmp/sdk-test.wisp.proj" <<'EOF'
<Project Sdk="Wisp.Sdk">
  <PropertyGroup>
    <Name>sdk-test</Name>
    <RootNamespace>sdk.test</RootNamespace>
    <StartupObject>sdk.test/main</StartupObject>
  </PropertyGroup>
  <ItemGroup>
    <Wisp Include="src/**/*.wisp" />
  </ItemGroup>
</Project>
EOF

cat > "$tmp/src/main.wisp" <<'EOF'
(ns sdk.test)
(defn main [] (println "msbuild sdk works"))
EOF

output="$(dotnet msbuild "$tmp/sdk-test.wisp.proj" -t:WispRun -p:WispTool="$tool" -v:minimal)"
printf '%s\n' "$output"
printf '%s\n' "$output" | grep -q "msbuild sdk works"
