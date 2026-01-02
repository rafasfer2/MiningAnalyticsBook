#!/usr/bin/env pwsh
# Post-build: normaliza legendas de figuras e títulos de exemplos em docs/
# 1) "Figura X.Y: ..."  -> "Figura X.Y - ..."
# 2) "Exemplo X.Y (Título)" -> "Exemplo X.Y - Título"

$root = Join-Path $PSScriptRoot '..\docs'
if (-not (Test-Path $root)) {
    Write-Error "Pasta docs/ não encontrada: $root"
    exit 1
}

function Fix-Text([string]$text) {
    $t = $text
    # Trata espaços normais, NBSP (&nbsp;) e Unicode NBSP
    # Substitui prefixos de figuras/tabelas: "Figura 1.1:" ou "Figura&nbsp;1.1:" -> "Figura 1.1 - "
    $t = $t -replace '((?:Figura|Tabela)\s*(?:&nbsp;|\u00A0|\s*)\d+(?:\.\d+)*)\s*[:：]\s*', '$1 - '

    # Substitui títulos de Exemplo com parênteses: Exemplo 1.1 (Título) -> Exemplo 1.1 - Título
    $t = $t -replace 'Exemplo\s*(\d+(?:\.\d+)*)\s*\(\s*', 'Exemplo $1 - '

    # Remove parênteses de fechamento que ficam antes de tags (ex: ")</strong>")
    $t = $t -replace '\)\s*(</strong>)', '$1'

    return $t
}

# Processa arquivos HTML
Get-ChildItem -Path $root -Recurse -Filter *.html | ForEach-Object {
    $path = $_.FullName
    $orig = Get-Content -Raw -Encoding UTF8 $path
    $content = Fix-Text $orig
    if ($content -ne $orig) {
        Set-Content -Path $path -Value $content -Encoding UTF8
        Write-Output "Atualizado: $path"
    }
    else {
        Write-Output "Sem alterações: $path"
    }
}

# Processa arquivos JSON (inclui search.json)
Get-ChildItem -Path $root -Recurse -Filter *.json | ForEach-Object {
    $path = $_.FullName
    $orig = Get-Content -Raw -Encoding UTF8 $path
    $content = Fix-Text $orig
    if ($content -ne $orig) {
        Set-Content -Path $path -Value $content -Encoding UTF8
        Write-Output "Atualizado: $path"
    }
    else {
        Write-Output "Sem alterações: $path"
    }
}
