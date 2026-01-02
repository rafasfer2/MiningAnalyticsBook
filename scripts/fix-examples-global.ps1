# Corrige ocorrências remanescentes de 'Figura X.Y:' e 'Exemplo X.Y (Título)'
$root = Join-Path $PSScriptRoot '..\docs'
if (-not (Test-Path $root)) { Write-Error "Pasta docs/ não encontrada: $root"; exit 1 }
Get-ChildItem -Path $root -Recurse -Include *.html, *.json | ForEach-Object {
    $path = $_.FullName
    $orig = Get-Content -Raw -Encoding UTF8 $path
    $content = $orig

    # Figura/Tabela: permite NBSP ou espaços entre palavra e número
    $content = $content -replace '((?:Figura|Tabela)\s*[\u00A0\s]*)(\d+(?:\.\d+)*)\s*[:：]\s*', '$1$2 - '

    # Exemplo: captura número e texto entre parênteses
    $content = $content -replace 'Exemplo\s*(\d+(?:\.\d+)*)\s*\(\s*([^\)]*?)\s*\)', 'Exemplo $1 - $2'

    if ($content -ne $orig) {
        Set-Content -Path $path -Value $content -Encoding UTF8
        Write-Output "Atualizado: $path"
    }
    else {
        Write-Output "Sem alterações: $path"
    }
}
