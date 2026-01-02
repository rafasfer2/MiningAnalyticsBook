$path = Join-Path $PSScriptRoot '..\docs\search.json'
if (-not (Test-Path $path)) { Write-Output "Arquivo não encontrado: $path"; exit 0 }
$orig = Get-Content -Raw -Encoding UTF8 $path
# Remove parêntese de fechamento imediatamente após o título do Exemplo
$content = $orig -replace 'Exemplo\s*(\d+(?:\.\d+)*)\s*-\s*([^\)]*)\)', 'Exemplo $1 - $2'
if ($content -ne $orig) { Set-Content -Path $path -Value $content -Encoding UTF8; Write-Output "Atualizado: $path" } else { Write-Output "Sem alterações: $path" }
