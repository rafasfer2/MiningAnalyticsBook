# Atualiza strings em docs/search.json para usar ' - ' em vez de ':' e remover parênteses em Exemplo
$path = Join-Path $PSScriptRoot '..\docs\search.json'
if (-not (Test-Path $path)) { Write-Output "Arquivo não encontrado: $path"; exit 0 }
$orig = Get-Content -Raw -Encoding UTF8 $path
$content = $orig -replace 'Figura\s*1\.1\s*[:：]', 'Figura 1.1 -' -replace 'Exemplo\s*1\.1\s*\(', 'Exemplo 1.1 - '
if ($content -ne $orig) { Set-Content -Path $path -Value $content -Encoding UTF8; Write-Output "Atualizado: $path" } else { Write-Output "Sem alterações: $path" }
